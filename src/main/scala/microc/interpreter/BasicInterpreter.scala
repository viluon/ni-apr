package microc.interpreter

import microc.analysis.Declarations
import microc.ast._

import java.io.{Reader, Writer}

class BasicInterpreter(program: Program, declarations: Declarations, stdin: Reader, stdout: Writer)
  extends Interpreter {

  object Context {
    def pure[A](x: A): Context[A] = ValidContext(x, Array(), Map())
    def sequence[A](xs: List[Context[A]]): Context[List[A]] = xs.foldLeft(pure(List())) { (acc, ctx) =>
//      acc.flatMap(xs => ctx.map(x => x :: xs))
      ???
    }
  }

  sealed trait Context[+A] {
    import Context.sequence

    def heap: Array[Value]
    def env: Map[Identifier, Int]
    def v: A

    def map[B](f: A => B): Context[B] = this match {
      case ctx@ValidContext(_, _, _) => ctx.copy(v = f(v))
      case err: ErrorContext => err
    }

    def flatMap[B](f: A => Context[B]): Context[B] = this match {
      case err: ErrorContext => err
      case ValidContext(v, heap, env) =>
        f(v) match {
          case err: ErrorContext => err
          case ValidContext(newV, newHeap, newEnv) =>
            ValidContext(v = newV, heap = heap ++ newHeap, env = env ++ newEnv)
        }
    }

    def withFilter(f: A => Boolean): Context[A] = this match {
      case ctx@ValidContext(v, _, _) if f(v) => ctx
      case _: ValidContext[A] => crash(???, ???)
      case err: ErrorContext => err
    }

    // TODO should insert match on `this` too?
    def insert[B](x: B): Context[B] = ValidContext(v = x, heap = heap, env = env)

    def crash(msg: String, span: Span): ErrorContext = ErrorContext(msg, span, heap, env)

    def deref(expr: Expr): Context[Value] = for (addr <- eval(expr); v <- deref(addr, expr.span)) yield v
    def deref(v: Value, span: Span): Context[Value]

    def alloc(obj: Value): Context[Int] = this match {
      case ValidContext(_, heap, env) => ValidContext(heap.length, heap :+ obj, env)
      case err: ErrorContext => err
    }

    def bind(id: Identifier, addr: Int): Context[()] = this match {
      case ValidContext(_, heap, env) => ValidContext((), heap, env + (id, addr))
      case err: ErrorContext => err
    }

    def eval(expr: Expr): Context[Value] = expr match {
      case Null(_) => ???
      case Number(value, _) => insert(IntVal(value))
      case id@Identifier(_, _) => insert(heap(env(id)))
      case BinaryOp(operator, left, right, _) => binOp(operator, left, right)
      case CallFuncExpr(targetFun, argExprs, span) => (for (fn <- deref(targetFun)) yield fn match {
        case addr: FunAddrVal =>
          program.declarations.filter(_.isInstanceOf[FunDecl]).find(_.name == addr.name) match {
            case None => crash(s"attempt to call undefined function ${addr.name}", span.highlighting(targetFun.span))
            case Some(decl: FunDecl) =>
              val ctx = argExprs.foldLeft(insert(List[Value]())) { case (ctx, argExp) =>
                // FIXME ugly
                val xs = ctx.v
                for (x <- ctx.eval(argExp)) yield x :: xs
              }
              // FIXME what is the receiver in the following call() call?
              for (args <- ctx) yield call(decl, args, span)
          }
        case _ => crash(s"expected a function address, got $v", targetFun.span)
      }).flatMap(identity).flatMap(identity)
      case Input(_) => ???
      case Alloc(expr, _) => ???
      case VarRef(id, _) => insert(AddrVal(env(id)))
      case Deref(pointer, span) => for (addr <- eval(pointer); value <- deref(addr, span)) yield value
      case Record(fields, _) => ???
      case FieldAccess(record, field, _) => ???
    }

    def call(f: FunDecl, args: List[Value], span: Span): Context[Value] = {
      val bindings = for (
        (decl, arg) <- f.params.zip(args)
      ) yield (Identifier(decl.name, decl.span), arg)
      val bound = bindings.foldLeft(this.insert(())) { case (acc, (id, arg)) =>
        acc.flatMap(_ => alloc(arg).flatMap(addr => bind(id, addr)))
      }
      f.block.vars
    }

    def binOp(operator: BinaryOperator, left: Expr, right: Expr): Context[Value] = {
      for (lv <- eval(left); rv <- eval(right))
        yield (lv, rv) match {
          case (IntVal(l), IntVal(r)) =>
            insert(IntVal(operator match {
              case Plus => l + r
              case Minus => l - r
              case Times => l * r
              case Divide => l / r
              case Equal => if (l == r) 1 else 0
              case LessThan => if (l < r) 1 else 0
            }))
          case (IntVal(_), problem) => crash(s"operator $operator expected int, got $problem", right.span)
          case (problem, _) => crash(s"operator $operator expected int, got $problem", left.span)
        }
    }.flatMap(identity)
  }

  case class ValidContext[A](v: A, heap: Array[Value], env: Map[Identifier, Int]) extends Context[A] {
    override def deref(v: Value, span: Span): Context[Value] = v match {
      case AddrVal(addr) => insert(heap(addr))
      // TODO functions?
      case value => crash(s"attempt to dereference $value", span)
    }
  }

  case class ErrorContext(msg: String, span: Span, heap: Array[Value], env: Map[Identifier, Int]) extends Context[Nothing] {
    def v: Nothing = ???
    override def deref(v: Value, s: Span): Context[Value] = ???
  }

  sealed trait Value
  case class IntVal(n: Int) extends Value
  case class AddrVal(addr: Int) extends Value
  case class FunAddrVal(name: String) extends Value
  case class RecordVal() extends Value // TODO

  def run(mainArgs: List[Int] = Nil): Int = program.declarations.find(_.name == "main") match {
    case Some(decl@FunDecl(name, params, block, span)) =>
      Context.pure(()).call(decl, mainArgs.map(IntVal), span) match {
        case ValidContext(IntVal(n), _, _) => n
        case ValidContext(v, _, _) => throw ExecutionException(s"expected an integer, but main returned $v", span)
        case ErrorContext(msg, span, _, _) => throw ExecutionException(msg, span)
      }
    case _ => throw ExecutionException("undefined entry point", program.span)
  }
}
