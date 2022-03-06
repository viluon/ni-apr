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

  // help type inference
  def get: Context[InterpreterState] = _get
  def crash[A](e: String, sp: Span): Context[A] = _crash((e, sp))
  def pure[A](x: A): Context[A] = _pure(x)

  def heap: Context[Heap] = for (InterpreterState(heap, _) <- get) yield heap
  def env: Context[Env] = for (InterpreterState(_, env) <- get) yield env
  def setHeap(heap: Heap): Context[Unit] = for (s <- get) yield put(s.copy(heap = heap))
  def setEnv(env: Env): Context[Unit] = for (s <- get) yield put(s.copy(env = env))

  def deref(v: Value): Context[Option[Either[Option[FunDecl], Value]]] = v match {
    case AddrVal(addr) => for (h <- heap) yield Some(Right(h(addr)))
    case FunAddrVal(name) => pure(Some(Left(program
      .declarations
      .filter(_.isInstanceOf[FunDecl])
      .find(_.name == name)
      .map(_.asInstanceOf[FunDecl])
    )))
    case _ => pure(None)
  }

  def alloc(obj: Value): Context[Int] = for (h <- heap; _ <- setHeap(h :+ obj)) yield h.length
  def bind(id: Identifier, addr: Int): Context[Unit] = for (e <- env; _ <- setEnv(e + ((id, addr)))) yield ()
  def lookup(id: Identifier): Context[Value] = env.flatMap(_.get(id) match {
    case Some(addr) => for (h <- heap) yield h(addr)
    case None => program.declarations.find(_.name == id.name) match {
      case Some(decl) => decl match {
        case IdentifierDecl(_, _) => throw new IllegalStateException("accessing an unallocated variable")
        case FunDecl(name, _, _, _) => pure(FunAddrVal(name))
      }
      case None => crash(s"undefined ${id.name}", id.span)
    }
  })

  def eval(stmt: Stmt): Context[Value] = stmt match {
    case block: StmtInNestedBlock => ???
    case block: Block => ???
    case ReturnStmt(expr, span) => eval(expr)
    case VarStmt(decls, span) => ???
  }

  def eval(expr: Expr): Context[Value] = expr match {
    case Null(_) => pure(NullVal)
    case Number(value, _) => pure(IntVal(value))
    case id@Identifier(_, _) => lookup(id)
    case BinaryOp(operator, left, right, _) => evalBinOp(operator, left, right)
    case CallFuncExpr(targetFun, argExprs, span) => evalCall(targetFun, argExprs, span)
    case Input(_) => ???
    case Alloc(expr, _) => for (v <- eval(expr); addr <- alloc(v)) yield IntVal(addr)
    case VarRef(id, _) => for (e <- env) yield AddrVal(e(id))
    case Deref(pointer, span) => (for (addr <- eval(pointer); v <- deref(addr)) yield v match {
      case Some(Right(value)) => pure(value)
      case Some(Left(Some(f))) => crash(s"attempt to dereference function ${f.name}, which can only be called", span)
      case Some(Left(None)) => crash(s"attempt to dereference undefined function $addr", span) // can this actually happen?
      case None => crash(s"attempt to dereference $v", span)
    }).flatten
    case Record(fields, _) => ???
    case FieldAccess(record, field, _) => ???
  }

  private def evalCall(targetFun: Expr, argExprs: List[Expr], span: Span): Context[Value] = {
    for (fnAddr <- eval(targetFun); fn <- deref(fnAddr)) yield fn match {
      case None => crash(s"attempt to call $fnAddr", span.highlighting(targetFun.span))
      case Some(Left(maybeDecl)) => maybeDecl match {
        case Some(decl) => for (
          originalEnv <- env;
          args <- argExprs.foldLeft(pure(List[Value]())) {
            case (soFar, argExp) => for (xs <- soFar; x <- eval(argExp)) yield x :: xs
          };
          v <- call(decl, args);
          _ <- setEnv(originalEnv)
        ) yield v
        case None => crash(s"attempt to call undefined function $fnAddr", span.highlighting(targetFun.span))
      }
      case Some(Right(v)) => crash(s"expected a function address, got $v", targetFun.span)
    }
  }.flatten

  def reduce[A](xs: Iterable[A])(f: A => Context[Unit]): Context[Unit] =
    xs.foldLeft(pure(()))((ctx, a) => ctx.flatMap(_ => f(a)))

  def call(f: FunDecl, args: List[Value]): Context[Value] = {
    val bindings = for (
      (decl, arg) <- f.params.zip(args)
    ) yield (Identifier(decl.name, decl.span), arg)

    for (
      // bind arguments
      _ <- reduce(bindings) { case (id, arg) => alloc(arg).map(addr => bind(id, addr)) };
      // allocate variables
      _ <- reduce(f.block.vars.flatMap(_.decls)) { case IdentifierDecl(name, span) =>
        alloc(NullVal).map(addr => bind(Identifier(name, span), addr))
      };
      // evaluate body
      v <- f.block.body.foldLeft(pure(NullVal: Value))((ctx, stmt) =>
        for (_ <- ctx; v <- eval(stmt)) yield v
      )
    ) yield v
  }

  def evalBinOp(operator: BinaryOperator, left: Expr, right: Expr): Context[Value] = {
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
  object NullVal extends Value
  case class IntVal(n: Int) extends Value
  case class AddrVal(addr: Int) extends Value
  case class FunAddrVal(name: String) extends Value
  case class RecordVal() extends Value // TODO

  def run(mainArgs: List[Int] = Nil): Int = {
    program.declarations.filter(_.isInstanceOf[FunDecl]).find(_.name == "main") match {
      case Some(decl@FunDecl(_, _, _, span)) =>
        call(decl, mainArgs.map(IntVal))(InterpreterState(Array(), Map())) match {
          case Right((IntVal(n), _)) => n
          case Right((v, _)) => throw ExecutionException(s"expected an integer, but main returned $v", span)
          case Left((msg, span)) => throw ExecutionException(msg, span)
        }
      case _ => throw ExecutionException("undefined entry point", program.span)
    }
  }
}
