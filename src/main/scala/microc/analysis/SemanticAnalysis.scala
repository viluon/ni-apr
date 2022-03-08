package microc.analysis

import microc.ast.{Alloc, AssignStmt, BinaryOp, Block, CallFuncExpr, Decl, Deref, Expr, FieldAccess, FunBlockStmt, FunDecl, Identifier, IdentifierDecl, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, Record, RecordField, ReturnStmt, Span, Stmt, StmtInNestedBlock, VarRef, VarStmt, WhileStmt}
import microc.cli.Reporter
import microc.util.CharacterSets.NL
import microc.util.ErrorState
import microc.util.ErrorState.{ErrorStateOps, reduce, crash => _crash, get => _get, pure => _pure, put => _put}
import microc.{ProgramException, ast}

import scala.annotation.tailrec
import scala.collection.immutable.{Iterable, Map}

case class SemanticError(msg: String, span: Span)

case class SemanticException(errors: List[SemanticError]) extends ProgramException("Semantic exception") {
  override def format(reporter: Reporter): String =
    errors.sortBy(_.span).map(x => reporter.formatError("semantic", x.msg, x.span)).mkString(NL)
}

/**
  * Semantic analysis for μC.
  *
  * It checks the following:
  *   - [x] Use of an undeclared identifier.
  *   - [ ] Duplicate identifiers.
  *     note: there is a single namespace (shared by both functions and identifiers)
  *   - [ ] Duplicate record field names.
  *   - [ ] Assignment to a function.
  *   - [ ] Getting address of a function.
  *
  * The result is a map of declarations, i.e., a map of identifiers to their declarations.
  *
  */
class SemanticAnalysis {
  type Env = Map[String, Decl]
  case class AnalyserSate(env: Env, decls: Declarations)
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
        case Some(previousDecl) => crash(s"redeclaration of ${decl.name}, previously declared at ${previousDecl.span}", decl.span)
        case None => pure(acc + kv)
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

  def analyze(program: Program): Declarations = go(program)(AnalyserSate(Map(), Map())) match {
    case Left(errs) => throw SemanticException(errs)
    case Right((_, state)) => state.decls
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

  private def go(stmt: Stmt): Analysis[Unit] = stmt match {
    case block: StmtInNestedBlock => block match {
      case AssignStmt(id: Identifier, right, span) => env.flatMap(_.get(id.name) match {
        case Some(_: IdentifierDecl) => go(id).flatMap(_ => go(right))
        case Some(_: FunDecl) => crash("cannot assign to a function", span.highlighting(id.span))
        case None => go(id) // this call will crash and take care of the error message
      })
      case AssignStmt(left@(Deref(_, _) | FieldAccess(_, _, _)), right, _) =>
        for (() <- go(left); () <- go(right)) yield ()
      case AssignStmt(left, _, span) =>
        crash(s"cannot assign to $left", span.highlighting(left.span))
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
    case FieldAccess(record, _, _) => go(record)
  }

  private def goOverExprs(args: Iterable[Expr]): Analysis[Unit] = reduce(args)(go)
  private def goOver(stmts: Iterable[Stmt]): Analysis[Unit] = reduce(stmts)(go)
}
