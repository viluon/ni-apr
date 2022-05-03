package microc.analysis

import microc.ast.{Alloc, AssignStmt, BinaryOp, Block, CallFuncExpr, Decl, Deref, Expr, FieldAccess, FunBlockStmt, FunDecl, Identifier, IdentifierDecl, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, Record, RecordField, ReturnStmt, Span, Stmt, StmtInNestedBlock, VarRef, VarStmt, WhileStmt}
import microc.cli.Reporter
import microc.util.ErrorState
import microc.util.ErrorState.{ErrorStateOps, reduce, crash => _crash, get => _get, pure => _pure, put => _put}
import microc.{ProgramException, ast}

import scala.annotation.tailrec
import scala.collection.immutable.{Iterable, Map}

case class SemanticError(msg: String, span: Span)

case class SemanticException(errors: List[SemanticError]) extends ProgramException("Semantic exception") {
  override def format(reporter: Reporter): String = reporter.formatErrors(errors)
}

/**
  * Semantic analysis for μC.
  *
  * It checks the following:
  *   - [x] Use of an undeclared identifier.
  *   - [x] Duplicate identifiers.
  *     note: there is a single namespace (shared by both functions and identifiers)
  *   - [x] Duplicate record field names.
  *   - [x] Assignment to a function.
  *   - [x] Getting address of a function.
  *
  * The result is a map of declarations, i.e., a map of identifiers to their declarations.
  *
  */
class SemanticAnalysis {
  type Env = Map[String, Decl]
  case class AnalyserSate(env: Env, decls: Declarations, fieldNames: Set[String])
  type Analysis[A] = ErrorState[List[SemanticError], AnalyserSate, A]

  // help type inference
  private def get: Analysis[AnalyserSate] = _get
  private def crash[A](e: String, sp: Span): Analysis[A] = _crash(List(SemanticError(e, sp)))
  private def pure[A](x: A): Analysis[A] = _pure(x)
  private def put(s: AnalyserSate): Analysis[Unit] = _put(s)

  private def env: Analysis[Env] = get.map(_.env)
  private def setEnv(e: Env): Analysis[Unit] = get.flatMap(s => put(s.copy(env = e)))
  private def declarations(d: Declarations): Analysis[Unit] = get.flatMap(s => put(s.copy(decls = d)))

  private def debug(msg: String): Analysis[Unit] = get.map { _ =>
//    print(msg)
//    println("\t" + state.env)
  }

  // fixme: redundant
  private def addDecls(d: Declarations): Analysis[Unit] =
    get.flatMap(state => d.foldLeft(pure(state.decls)) {
      case (ctx, kv@(id, decl)) => ctx.flatMap(acc => acc.get(id) match {
        case Some(previousDecl) if false && previousDecl.span != Span.invalid =>
          crash(s"redeclaration of ${decl.name}, previously declared at ${previousDecl.span}", decl.span)
        case _ => pure(acc + kv)
      })
    }.flatMap(declarations))

  def combine(saved: Env, e: Iterable[(String, Decl)]): Analysis[Env] = e.foldLeft(pure(saved)) {
    case (ctx, kv@(name, decl)) => ctx.flatMap(acc => acc.get(name) match {
      case Some(previousDecl) => crash(s"redeclaration of ${decl.name}, previously declared at ${previousDecl.span}", decl.span)
      case None => pure(acc + kv)
    })
  }

  private def inScope[A](e: Iterable[(String, Decl)])(m: Analysis[A]): Analysis[A] = for (
    saved <- env;
    updated <- combine(saved, e);
    () <- setEnv(updated);
    a <- m;
    () <- setEnv(saved)
  ) yield a

  /**
    * Catch semantic errors in the given program.
    *
    * @return The resolved references of all identifiers in the program.
    *         The map's values are a subset of all program declarations, e.g. main() is often missing.
    */
  def analyze(program: Program): (Declarations, Set[String]) = go(program)(AnalyserSate(Map(), Map(), Set())) match {
    case Left(errs) => throw SemanticException(errs)
    case Right((_, state)) => (state.decls, state.fieldNames)
  }

  private def go(program: Program): Analysis[Unit] = for (
    fnDecls <- combine(Map(), program.funs.map(f => (f.name, f: Decl)));
    () <- setEnv(fnDecls);
    () <- reduce(program.funs) { decl =>
      inScope(decl.params.map(id => (id.name, id: Decl)))(for (
        () <- debug(s"function ${decl.name}");
        () <- go(decl.block)
      ) yield ())
    }
  ) yield ()

  @tailrec
  private def lvalue(expr: Expr): Analysis[Boolean] = expr match {
    case ast.Null(span) => pure(false)
    case ast.Number(value, span) => pure(false)
    case Identifier(name, span) => env.flatMap(_.get(name) match {
      case Some(_: FunDecl) => crash(s"cannot assign to a function", expr.span)
      // not handling unbound identifiers here, regular analysis should catch those
      case _ => pure(true)
    })
    case BinaryOp(operator, left, right, span) => pure(false)
    case CallFuncExpr(targetFun, args, span) => pure(false)
    case Input(span) => pure(false)
    case Alloc(expr, span) => pure(false)
    case VarRef(id, span) => pure(false)
    case Deref(pointer, span) => lvalue(pointer)
    case Record(fields, span) => pure(false)
    case FieldAccess(record, field, span) => lvalue(record)
  }

  private def expect(a: Boolean): (String, Span) => Analysis[Unit] =
    if (a) (_, _) => pure(()) else crash

  private def go(stmt: Stmt): Analysis[Unit] = stmt match {
    case block: StmtInNestedBlock => block match {
      case AssignStmt(id: Identifier, right, span) => env.flatMap(_.get(id.name) match {
        case Some(_: IdentifierDecl) => go(id).flatMap(_ => go(right))
        case Some(_: FunDecl) => crash("cannot assign to a function", span.highlighting(id.span))
        case None => go(id) // this call will crash and take care of the error message
      })
      case AssignStmt(left, right, span) => for (
        isLvalue <- lvalue(left);
        () <- expect(isLvalue)(s"cannot assign to rvalue $left", span.highlighting(left.span));
        () <- go(left);
        () <- go(right)
      ) yield ()
      case NestedBlockStmt(body, _) => goOver(body)
      case IfStmt(guard, thenBranch, elseBranch, _) =>
        for (() <- go(guard); () <- go(thenBranch); () <- elseBranch.map(go).getOrElse(pure(()))) yield ()
      case WhileStmt(guard, block, _) => for (() <- go(guard); () <- go(block)) yield ()
      case OutputStmt(expr, _) => go(expr)
    }
    case block: Block => block match {
      case NestedBlockStmt(body, _) => goOver(body)
      case FunBlockStmt(vars, stmts, ret, _) =>
        inScope(vars.flatMap(v => v.decls).map(id => (id.name, id: Decl)))(for (
          () <- debug("function body");
          () <- goOver(stmts :+ ret)
        ) yield ())
    }
    case ReturnStmt(expr, _) => go(expr)
    case VarStmt(_, _) => throw new IllegalStateException("VarStmt must be handled by the enclosing function")
  }

  @tailrec
  private def go(expr: Expr): Analysis[Unit] = expr match {
    case ast.Null(_) => debug("expr").flatMap(_ => addDecls(Map()))
    case ast.Number(_, _) => debug("expr").flatMap(_ => addDecls(Map()))
    case id@Identifier(name, span) => env.flatMap(_.get(name) match {
      case Some(decl) => debug("expr").flatMap(_ => addDecls(Map(id -> decl)))
      case None => crash(s"undefined reference to $name", span)
    })
    case BinaryOp(_, left, right, _) => goOverExprs(List(left, right))
    case CallFuncExpr(targetFun, args, _) => goOverExprs(targetFun :: args)
    case Input(_) => debug("expr").flatMap(_ => addDecls(Map()))
    case Alloc(expr, _) => go(expr)
    case VarRef(id, _) => go(id)
    case Deref(pointer, _) => go(pointer)
    case Record(fields, _) => fields.foldLeft(pure(Map[String, RecordField]())){
      (ctx, field) => ctx.flatMap(acc => acc.get(field.name) match {
        case Some(existingField) =>
          crash(s"field ${field.name} already exists at ${existingField.span}", field.span) // todo add hint
        case None => pure(acc + (field.name -> field))
      })
    }.flatMap(_ => goOverExprs(fields.map(_.expr)))
      .flatMap(_ => get).flatMap(s => put(s.copy(fieldNames = s.fieldNames union fields.map(_.name).toSet)))
    case FieldAccess(record, _, _) => go(record)
  }

  private def goOverExprs(args: Iterable[Expr]): Analysis[Unit] = reduce(args)(go)
  private def goOver(stmts: Iterable[Stmt]): Analysis[Unit] = reduce(stmts)(go)
}
