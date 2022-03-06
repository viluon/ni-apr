package microc.interpreter

import microc.analysis.Declarations
import microc.ast._
import microc.util.ErrorState
import microc.util.ErrorState.{ErrorStateOps, put, crash => _crash, get => _get, pure => _pure}

import java.io.{Reader, Writer}

class BasicInterpreter(program: Program, declarations: Declarations, stdin: Reader, stdout: Writer)
  extends Interpreter {

  type Context[A] = ErrorState[(String, Span), InterpreterState, A]
  type Env = Map[Identifier, Int]
  type Heap = Array[Value]

  case class InterpreterState(heap: Heap, env: Env)

  def get: Context[InterpreterState] = _get
  def crash[A](e: String, sp: Span): Context[A] = _crash((e, sp))
  def pure[A](x: A): Context[A] = _pure(x)

  def heap: Context[Heap] = for (InterpreterState(heap, _) <- get) yield heap
  def env: Context[Env] = for (InterpreterState(_, env) <- get) yield env
  def setHeap(heap: Heap): Context[Unit] = for (s <- get) yield put(s.copy(heap = heap))
  def setEnv(env: Env): Context[Unit] = for (s <- get) yield put(s.copy(env = env))

  // TODO: what should dereferencing a function mean?
  def deref(v: Value): Context[Option[Value]] = v match {
    case AddrVal(addr) => for (h <- heap) yield Some(h(addr))
    case FunAddrVal(name) => ???
    case _ => pure(None)
  }

  def alloc(obj: Value): Context[Int] = for (h <- heap; _ <- setHeap(h :+ obj)) yield h.length
  def bind(id: Identifier, addr: Int): Context[Unit] = for (e <- env; _ <- setEnv(e + ((id, addr)))) yield ()
  def lookup(id: Identifier): Context[Int] = (for (e <- env) yield e.get(id) match {
    case Some(addr) => pure(addr)
    case None => crash(s"undefined ${id.name}", id.span)
  }).flatten

  def eval(expr: Expr): Context[Value] = expr match {
    case Null(_) => ???
    case Number(value, _) => pure(IntVal(value))
    case id@Identifier(_, _) => for (addr <- lookup(id); h <- heap) yield h(addr)
    case BinaryOp(operator, left, right, _) => binOp(operator, left, right)
    case CallFuncExpr(targetFun, argExprs, span) => (for (fnAddr <- eval(targetFun); fn <- deref(fnAddr)) yield fn match {
      case addr: FunAddrVal =>
        program.declarations.filter(_.isInstanceOf[FunDecl]).find(_.name == addr.name) match {
          case None => crash(s"attempt to call undefined function ${addr.name}", span.highlighting(targetFun.span))
          case Some(decl: FunDecl) =>
            val ctx = argExprs.foldLeft(pure(List[Value]())) { case (ctx, argExp) =>
              // FIXME ugly
//              val xs = ctx.v
//              for (x <- ctx.eval(argExp)) yield x :: xs
              ???
            }
            // FIXME what is the receiver in the following call() call?
            for (args <- ctx) yield call(decl, args, span)
        }
      case _ => crash(s"expected a function address, got " /*FIXME*/, targetFun.span)
    }).flatten.flatten
    case Input(_) => ???
    case Alloc(expr, _) => ???
    case VarRef(id, _) => for (e <- env) yield AddrVal(e(id))
    case Deref(pointer, span) => (for (addr <- eval(pointer); v <- deref(addr)) yield v match {
      case Some(value) => pure(value)
      case None => crash(s"cannot dereference $v", span)
    }).flatten
    case Record(fields, _) => ???
    case FieldAccess(record, field, _) => ???
  }

  def call(f: FunDecl, args: List[Value], span: Span): Context[Value] = {
    val bindings = for (
      (decl, arg) <- f.params.zip(args)
    ) yield (Identifier(decl.name, decl.span), arg)
    val bound = bindings.foldLeft(pure(())) { case (acc, (id, arg)) =>
      acc.flatMap(_ => alloc(arg).flatMap(addr => bind(id, addr)))
    }
    f.block.vars
    ???
  }

  def binOp(operator: BinaryOperator, left: Expr, right: Expr): Context[Value] = {
    for (lv <- eval(left); rv <- eval(right))
      yield (lv, rv) match {
        case (IntVal(l), IntVal(r)) =>
          pure(IntVal(operator match {
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
  }.flatten

  sealed trait Value
  case class IntVal(n: Int) extends Value
  case class AddrVal(addr: Int) extends Value
  case class FunAddrVal(name: String) extends Value
  case class RecordVal() extends Value // TODO

  def run(mainArgs: List[Int] = Nil): Int = program.declarations.find(_.name == "main") match {
    case Some(decl@FunDecl(name, params, block, span)) =>
      call(decl, mainArgs.map(IntVal), span)(InterpreterState(Array(), Map())) match {
        case Right((IntVal(n), _)) => n
        case Right((v, _)) => throw ExecutionException(s"expected an integer, but main returned $v", span)
        case Left((msg, span)) => throw ExecutionException(msg, span)
      }
    case _ => throw ExecutionException("undefined entry point", program.span)
  }
}
