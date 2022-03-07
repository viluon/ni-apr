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

  // help type inference
  def get: Context[InterpreterState] = _get
  def crash[A](e: String, sp: Span): Context[A] = _crash((e, sp))
  def pure[A](x: A): Context[A] = _pure(x)

  def heap: Context[Heap] = for (InterpreterState(heap, _) <- get) yield heap
  def env: Context[Env] = for (InterpreterState(_, env) <- get) yield env
  // replacing flatMap with map in the definitions below surprisingly typechecks
  // took me 2 hours to track down
  def setHeap(heap: Heap): Context[Unit] = get.flatMap(s => put(s.copy(heap = heap)))
  def setEnv(env: Env): Context[Unit] = get.flatMap(s => put(s.copy(env = env)))

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

  def alloc(obj: Value): Context[Int] = for (h <- heap; () <- setHeap(h :+ obj); () <- pure(())) yield h.length
  def bind(id: Identifier, addr: Int): Context[Unit] = for (e <- env; () <- setEnv(e + ((id, addr))); () <- pure(())) yield ()
  def lookup(id: Identifier): Context[Value] = env.flatMap(e => e.get(id) match {
    case Some(addr) => for (h <- heap) yield h(addr)
    case None => program.declarations.find(_.name == id.name) match {
      case Some(decl) => decl match {
        case IdentifierDecl(_, _) => throw new IllegalStateException(s"accessing an unallocated variable $id")
        case FunDecl(name, _, _, _) => pure(FunAddrVal(name))
      }
      case None => crash(s"undefined ${id.name}", id.span)
    }
  })

  def eval(stmt: Stmt): Context[Value] = stmt match {
    case block: StmtInNestedBlock => block match {
      case AssignStmt(left, right, span) => for (
        l <- eval(left);
        addr <- l match {
          case AddrVal(addr) => pure(addr)
          case _ => ???
        };
        r <- eval(right);
        h <- heap;
        () <- setHeap(h.updated(addr, r));
        nh <- heap
      ) yield { println(nh.mkString("Array(", ", ", ")")); NullVal }
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
      for (
        () <- reduce(decls) { case IdentifierDecl(name, span) =>
          alloc(NullVal).flatMap(addr => bind(Identifier(name, span), addr))
        };
//        () <- decls.foldLeft(pure(())) { case (ctx, IdentifierDecl(name, span)) =>
//          ctx.flatMap(_ => alloc(NullVal).flatMap(addr => bind(Identifier(name, span), addr)))
//        };
//        () <- bind(Identifier("oof", span), 9);
//        0 <- alloc(NullVal);
//        () <- put(InterpreterState(Array(IntVal(3)), Map(Identifier("x", span) -> 9))): Context[Unit];
        ne <- env;
        nh <- heap;
        () <- pure(())
      ) yield { println("from varstmt:", ne, nh.mkString("Array(", ", ", ")")); NullVal }
  }

  def eval(expr: Expr): Context[Value] = expr match {
    case Null(_) => pure(NullVal)
    case Number(value, _) => pure(IntVal(value))
    case id@Identifier(_, _) => lookup(id)
    case BinaryOp(operator, left, right, _) => evalBinOp(operator, left, right)
    case CallFuncExpr(targetFun, argExprs, span) => evalCall(targetFun, argExprs, span)
    case Input(_) => ???
    case Alloc(expr, _) => for (v <- eval(expr); addr <- alloc(v)) yield AddrVal(addr)
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
          () <- setEnv(originalEnv)
        ) yield v
        case None => crash(s"attempt to call undefined function $fnAddr", span.highlighting(targetFun.span))
      }
      case Some(Right(v)) => crash(s"expected a function address, got $v", targetFun.span)
    }
  }.flatten

  def reduce[A](xs: Iterable[A])(f: A => Context[Unit]): Context[Unit] =
    xs.foldLeft(pure(()))((ctx, a) => ctx.flatMap { case () => f(a) })

  def call(f: FunDecl, args: List[Value]): Context[Value] = {
    val bindings = for (
      (decl, arg) <- f.params.zip(args)
    ) yield (Identifier(decl.name, decl.span), arg)

    for (
      // bind arguments
      () <- reduce(bindings) { case (id, arg) => alloc(arg).flatMap(addr => bind(id, addr)) };
      // evaluate body
      v <- f.block.body.foldLeft(pure(NullVal: Value))((ctx, stmt) =>
        for (NullVal <- ctx; v <- eval(stmt)) yield v
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
