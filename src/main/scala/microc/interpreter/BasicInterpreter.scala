package microc.interpreter

import microc.analysis.Declarations
import microc.ast._
import microc.util.ErrorState
import microc.util.ErrorState.{ErrorStateOps, put, reduce, crash => _crash, get => _get, pure => _pure}

import java.io.{Reader, Writer}

class BasicInterpreter(program: Program, declarations: Declarations, stdin: Reader, stdout: Writer)
  extends Interpreter {

  case class Error(msg: String, location: Span, hints: List[(String, Span)] = Nil)

  type Context[A] = ErrorState[List[Error], InterpreterState, A]
  type Env = Map[IdentifierDecl, Int]
  type Heap = Array[Value]

  // help type inference
  def get: Context[InterpreterState] = _get
  def crash[A](e: String, sp: Span): Context[A] = _crash(List(Error(e, sp)))
  def pure[A](x: A): Context[A] = _pure(x)

  implicit class ErrorContextOps[A](x: Context[A]) {
    def mapErr(pf: PartialFunction[List[Error], List[Error]]): Context[A] = s => x(s) match {
      case Left(errs) => Left(pf.applyOrElse(errs, (x: List[Error]) => x))
      case r => r
    }

    def withHint(hint: String, loc: Span): Context[A] = mapErr {
      case (err@Error(_, _, hints)) :: errs => err.copy(hints = (hint, loc) :: hints) :: errs
    }
  }

  def heap: Context[Heap] = for (InterpreterState(heap, _) <- get) yield heap
  def env: Context[Env] = for (InterpreterState(_, env) <- get) yield env
  // replacing flatMap with map in the definitions below surprisingly typechecks
  // took me 2 hours to track down
  def setHeap(heap: Heap): Context[Unit] = get.flatMap(s => put(s.copy(heap = heap)))
  def setEnv(env: Env): Context[Unit] = get.flatMap(s => put(s.copy(env = env)))

  def deref(v: Value): Context[Option[Either[FunDecl, Value]]] = v match {
    case AddrVal(addr) => for (h <- heap) yield Some(Right(h(addr)))
    case FunAddrVal(decl) => pure(Some(Left(decl)))
    case _ => pure(None)
  }

  def alloc(obj: Value): Context[Int] = for (h <- heap; () <- setHeap(h :+ obj); () <- pure(())) yield h.length
  def bind(id: IdentifierDecl, addr: Int): Context[Unit] = for (e <- env; () <- setEnv(e + ((id, addr))); () <- pure(())) yield ()

  def lookup(decl: Decl): Context[Option[Value]] = decl match {
    case id: IdentifierDecl => env.flatMap(_.get(id) match {
      case Some(addr) => for (h <- heap) yield Some(h(addr))
      case None => pure(None)
    })
    case fn: FunDecl => pure(Some(FunAddrVal(fn)))
  }

  def eval(stmt: Stmt): Context[Value] = stmt match {
    case block: StmtInNestedBlock => block match {
      case AssignStmt(id: Identifier, right, span) => for (
        e <- env;
        addr <- declarations(id) match {
          case id: IdentifierDecl => pure(e(id))
          case _: FunDecl => crash("cannot assign to a function", span.highlighting(id.span))
        };
        r <- eval(right);
        h <- heap;
        () <- setHeap(h.updated(addr, r))
      ) yield NullVal
      case AssignStmt(Deref(pointer, ptrSpan), right, span) => for (
        ptr <- eval(pointer);
        addr <- ptr match {
          case AddrVal(addr) => pure(addr)
          case NullVal => crash(s"attempt to dereference a null value", ptrSpan.highlighting(pointer.span))
          case v => crash(s"attempt to dereference $v", ptrSpan.highlighting(pointer.span))
        };
        v <- eval(right);
        h <- heap;
        () <- setHeap(h.updated(addr, v))
      ) yield v
      case AssignStmt(left, _, span) => crash(s"cannot assign to $left", span.highlighting(left.span))
      case NestedBlockStmt(body, span) => body.foldLeft(pure(NullVal: Value)) {
        case (ctx, stmt) => ctx.flatMap(_ => eval(stmt))
      }
      case IfStmt(guard, thenBranch, elseBranch, span) => for (
        condition <- eval(guard);
        result <- eval(if (condition.truthy) thenBranch else elseBranch.getOrElse(???))
      ) yield result
      case WhileStmt(guard, block, span) => ???
      case OutputStmt(expr, span) => ???
    }
    case block: Block => block match {
      case NestedBlockStmt(body, span) => ???
      case FunBlockStmt(vars, stmts, ret, span) => ???
    }
    case ReturnStmt(expr, span) => eval(expr)
    case VarStmt(decls, span) =>
      // allocate variables
      for (() <- reduce(decls) { decl: IdentifierDecl =>
        alloc(NullVal).flatMap(addr => bind(decl, addr))
      }) yield NullVal
  }

  def eval(expr: Expr): Context[Value] = expr match {
    case Null(_) => pure(NullVal)
    case Number(value, _) => pure(IntVal(value))
    case id@Identifier(_, span) => lookup(declarations(id)).flatMap {
      case Some(v) => pure(v)
      case None => crash(s"undefined variable $id", span)
    }
    case BinaryOp(operator, left, right, _) => evalBinOp(operator, left, right)
    case CallFuncExpr(targetFun, argExprs, span) => evalCall(targetFun, argExprs, span)
    case Input(_) => ???
    case Alloc(expr, _) => for (v <- eval(expr); addr <- alloc(v)) yield AddrVal(addr)
    case VarRef(id, _) => for (e <- env) yield declarations(id) match {
      case id: IdentifierDecl => AddrVal(e(id))
      case fn: FunDecl => FunAddrVal(fn) // FIXME should be illegal
    }
    case Deref(pointer, span) => (for (addr <- eval(pointer); v <- deref(addr)) yield v match {
      case Some(Right(value)) => pure(value)
      case Some(Left(fnDecl)) => crash(s"attempt to dereference function ${fnDecl.name}, which can only be called", span)
      case None => crash(s"attempt to dereference $v", span)
    }).flatten
    case Record(fields, _) => ???
    case FieldAccess(record, field, _) => ???
  }

  private def evalCall(targetFun: Expr, argExprs: List[Expr], span: Span): Context[Value] = {
    for (fnAddr <- eval(targetFun); fn <- deref(fnAddr)) yield fn match {
      case None => crash(s"attempt to call undefined $fnAddr", span.highlighting(targetFun.span))
      case Some(Left(decl)) => for (
        originalEnv <- env;
        args <- argExprs.foldLeft(pure(List[Value]())) {
          case (soFar, argExp) => for (xs <- soFar; x <- eval(argExp)) yield x :: xs
        };
        v <- call(decl, args);
        () <- setEnv(originalEnv)
      ) yield v
      case Some(Right(v)) => crash(s"expected a function address, got $v", targetFun.span)
          .withHint("to evaluate the call here", span)
    }
  }.flatten

  def call(f: FunDecl, args: List[Value]): Context[Value] = for (
    // bind arguments
    () <- reduce(f.params.zip(args)) { case (decl, arg) => alloc(arg).flatMap(addr => bind(decl, addr)) };
    // evaluate body
    v <- f.block.body.foldLeft(pure(NullVal: Value))((ctx, stmt) =>
      for (NullVal <- ctx; v <- eval(stmt)) yield v
    )
  ) yield v

  def evalBinOp(operator: BinaryOperator, left: Expr, right: Expr): Context[Value] = {
    val span = left.span ++ right.span
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
        case (x@(NullVal | AddrVal(_) | FunAddrVal(_)), y@(NullVal | AddrVal(_) | FunAddrVal(_)))
          if operator == Equal => pure(IntVal(if (x == y) 1 else 0))
        case (x, y) if operator == Equal =>
          crash(s"operator $operator expected two integers or two pointers, got $x and $y", span)
        case (IntVal(_), problem) =>
          crash(s"operator $operator expected int, got $problem", span.highlighting(right.span))
        case (problem, _) =>
          crash(s"operator $operator expected int, got $problem", span.highlighting(left.span))
      }
  }.flatten

  def run(mainArgs: List[Int] = Nil): Int = {
    program.declarations.filter(_.isInstanceOf[FunDecl]).find(_.name == "main") match {
      case Some(decl@FunDecl(_, _, _, span)) =>
        call(decl, mainArgs.map(IntVal))(InterpreterState(Array(), Map())) match {
          case Right((IntVal(n), _)) => n
          case Right((v, _)) => throw ExecutionException(s"expected an integer, but main returned $v", span)
          case Left(errs) =>
            // TODO better error reporting
            val Error(msg, span, _) = errs.head
            throw ExecutionException(msg, span)
        }
      case _ => throw ExecutionException("undefined entry point", program.span)
    }
  }
}
