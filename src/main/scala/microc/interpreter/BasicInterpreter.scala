package microc.interpreter

import microc.analysis.Declarations
import microc.ast._
import microc.util.ErrorState
import microc.util.ErrorState.{ErrorStateOps, foldLeft, put, reduce, crash => _crash, get => _get, pure => _pure}

import java.io.{BufferedReader, Reader, Writer}
import scala.annotation.tailrec

class BasicInterpreter(program: Program, declarations: Declarations, reader: Reader, stdout: Writer)
  extends Interpreter {
  private val stdin: BufferedReader = new BufferedReader(reader, 1)

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
    case block: StmtInNestedBlock => evalInBlock(block)
    case block: Block => throw new IllegalStateException("oops")
    case ReturnStmt(expr, _) => eval(expr)
    case VarStmt(decls, _) =>
      // allocate variables
      for (() <- reduce(decls) { decl: IdentifierDecl =>
        alloc(NullVal).flatMap(addr => bind(decl, addr))
      }) yield NullVal
  }

  def evalWhile(guard: Expr, block: StmtInNestedBlock): Context[Value] = { init =>
    @tailrec
    def loop(s: InterpreterState): Either[List[Error], (Value, InterpreterState)] =
      eval(guard)(s) match {
        case Left(errs) => Left(errs)
        case Right((v, s2)) if v.truthy => eval(block)(s2) match {
          case Left(errs) => Left(errs)
          case Right((_, s3)) => loop(s3)
        }
        case Right((v, s2)) => Right((NullVal, s2))
      }

    loop(init)
  }

  private def set(addr: Int, v: Value): Context[Unit] = for (
    h <- heap;
    () <- () match {
      case () if h.indices.contains(addr) => pure(())
      case () => throw new IllegalStateException(
        s"attempt to assign $v to address $addr, which is out of bounds (0 .. ${h.length})"
      )
    };
    () <- setHeap(h.updated(addr, v))
  ) yield ()

  private def evalAssignment(left: Expr, right: Expr, span: Span): Context[Value] = {
    def asAddr(span: Span, target: Value): Context[Int] = target match {
      case NullVal => crash(s"attempt to assign to a null pointer", span)
      case AddrVal(addr) => pure(addr)
      case v => crash(s"cannot dereference $v", span)
    }

    def findId(id: Identifier, span: Span): Context[Int] = declarations.get(id) match {
      case Some(idDecl: IdentifierDecl) => env.flatMap(_.get(idDecl) match {
        case Some(addr) => pure(addr)
        case None => crash(s"undefined reference to $id", span)
      })
      case Some(decl) => crash(s"$id does not refer to a variable, it points to $decl", span)
      case None => throw new IllegalStateException(s"semantic analysis missed $id at $span")
    }

    def describeFields(r: RecordVal) = () match {
      case _ if r.fields.isEmpty => "no fields"
      case _ if r.fields.size == 1 => s"the field ${r.fields.keysIterator.next()}"
      case _ => r.fields.keys.mkString("fields ", ",", "")
    }

    def missingField(expr: Expr, field: String, span: Span, r: RecordVal) =
      crash(s"cannot assign to field $field of record $expr", span)
        .withHint(s"$expr is a record with ${describeFields(r)}", expr.span)

    def derefRecord(expr: Expr, field: String, sp: Span, addr: Int): Context[RecordVal] = heap.flatMap(_(addr) match {
      case r: RecordVal if r.fields.contains(field) => pure(r)
      case r: RecordVal => missingField(expr, field, sp, r)
      case v => crash(s"cannot assign to field $field of variable $expr", sp)
        .withHint(s"$expr is $v, not a record", sp)
    })

    def assignRhs(addr: Int, f: Value => Value = identity): Context[Value] =
      for (v <- eval(right); () <- set(addr, f(v))) yield v

    left match {
      case DirectWrite(id, span) => findId(id, span).flatMap(assignRhs(_))
      case IndirectWrite(expr, sp) =>
        eval(expr).flatMap(target => asAddr(span.highlighting(sp), target).flatMap(assignRhs(_)))
      case DirectFieldWrite(id, field, sp) => for (
        addr <- findId(id, sp);
        rec <- derefRecord(id, field, sp, addr);
        v <- assignRhs(addr, v => RecordVal(rec.fields.updated(field, v)))
      ) yield v
      case IndirectFieldWrite(expr, field, sp) => for (
        target <- eval(expr);
        addr <- asAddr(span.highlighting(sp), target);
        rec <- derefRecord(expr, field, sp, addr);
        v <- assignRhs(addr, v => RecordVal(rec.fields.updated(field, v)))
      ) yield v
      case _ => crash(s"cannot assign to $left", span.highlighting(left.span))
    }
  }

  private def evalInBlock(block: StmtInNestedBlock): Context[Value] = block match {
    case AssignStmt(left, right, span) => evalAssignment(left, right, span)
    case NestedBlockStmt(body, _) => body.foldLeft(pure(NullVal: Value)) {
      case (ctx, stmt) => ctx.flatMap(_ => eval(stmt))
    }
    case IfStmt(guard, thenBranch, elseBranch, _) => for (
      condition <- eval(guard);
      result <- elseBranch match {
        case _ if condition.truthy => eval(thenBranch)
        case Some(alternative) => eval(alternative)
        case None => pure(NullVal)
      }
    ) yield result
    case WhileStmt(guard, block, _) => evalWhile(guard, block)
    case OutputStmt(expr, span) => eval(expr).flatMap {
      case IntVal(n) =>
        stdout.write(n.toString)
        stdout.write('\n')
        pure(NullVal)
      case v => crash(s"attempt to output $v", span)
    }
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
    case Input(_) => pure(
      try IntVal(stdin.readLine().toInt)
      catch { case _: java.io.IOException => NullVal }
    )
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
    case Record(fields, _) => foldLeft(fields)(Map[String, Value]()) {
      (acc, field) => for (
        v <- eval(field.expr);
        () <- v match {
          case RecordVal(_) => crash(s"nested records are not supported, use pointers", field.span.highlighting(field.expr.span))
          case _ => pure(())
        }
      ) yield acc + (field.name -> v)
    }.map(RecordVal)
    case FieldAccess(record, field, span) => eval(record).flatMap {
      case RecordVal(fields) => fields.get(field).map(pure).getOrElse(crash(s"no such field $field", span))
      case v => crash(s"cannot access field $field of $v, it is not a record", span.highlighting(record.span))
    }
  }

  private def evalCall(targetFun: Expr, argExprs: List[Expr], span: Span): Context[Value] = {
    for (fnAddr <- eval(targetFun); fn <- deref(fnAddr)) yield fn match {
      case None => crash(s"attempt to call undefined $fnAddr", span.highlighting(targetFun.span))
      case Some(Left(decl)) => for (
        originalEnv <- env;
        args <- argExprs.foldLeft(pure(List[Value]())) {
          case (soFar, argExp) => for (xs <- soFar; x <- eval(argExp)) yield x :: xs
        };
        v <- call(decl, args.reverse);
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
        case (IntVal(l), IntVal(r)) => operator.eval(l, r) match {
          case Some(x) => pure(IntVal(x))
          case None => crash(s"division by zero", span)
        }
        case (x@(NullVal | AddrVal(_) | FunAddrVal(_)), y@(NullVal | AddrVal(_) | FunAddrVal(_)))
          if operator == Equal() => pure(IntVal(if (x == y) 1 else 0))
        case (x, y) if operator == Equal() =>
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
