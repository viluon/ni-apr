package microc.analysis

import microc.ast.{Alloc, AssignStmt, BinaryOp, Block, CallFuncExpr, Decl, Deref, Expr, FieldAccess, FunBlockStmt, Identifier, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, Record, ReturnStmt, Span, Stmt, StmtInNestedBlock, VarRef, VarStmt, WhileStmt}
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
  private def declarations: Analysis[Unit] = get.map(_.decls)
  private def declarations(d: Declarations): Analysis[Unit] = get.flatMap(s => put(s.copy(decls = d)))

  private def debug(msg: String): Analysis[Unit] = get.map { state =>
//    print(msg)
//    println("\t" + state.env)
  }

  private def addDecls(d: Declarations): Analysis[Unit] =
    get.flatMap(state => declarations(state.decls ++ d)) // TODO

  def analyze(program: Program): Declarations = go(program)(AnalyserSate(Map(), Map())) match {
    case Left(errs) => throw SemanticException(errs)
    case Right((_, state)) => state.decls
  }

  private def go(program: Program): Analysis[Unit] = for (
    () <- setEnv(program.funs.map(f => (f.name, f: Decl)).toMap);
    () <- reduce(program.funs) { decl => for (
      e <- env;
      () <- setEnv(e ++ decl.params.map(id => (id.name, id: Decl)));
      () <- debug(s"function ${decl.name}");
      () <- go(decl.block);
      () <- setEnv(e)
    ) yield () }
  ) yield ()

  private def go(stmt: Stmt): Analysis[Unit] = stmt match {
    case block: StmtInNestedBlock => block match {
      case AssignStmt(left, right, span) =>
        for (() <- go(left); () <- go(right)) yield ()
      case NestedBlockStmt(body, span) => goOver(body)
      case IfStmt(guard, thenBranch, elseBranch, span) => ???
      case WhileStmt(guard, block, span) => ???
      case OutputStmt(expr, span) => ???
    }
    case block: Block => block match {
      case NestedBlockStmt(body, span) => goOver(body)
      case FunBlockStmt(vars, stmts, ret, span) => for (
        savedEnv <- env;
        () <- setEnv(savedEnv ++ vars.flatMap(v => v.decls).map(id => (id.name, id: Decl)));
        () <- debug("function body");
        () <- goOver(stmts :+ ret);
        () <- setEnv(savedEnv)
      ) yield ()
    }
    case ReturnStmt(expr, span) => go(expr)
    case VarStmt(decls, span) => ???
  }

  @tailrec
  private def go(expr: Expr): Analysis[Unit] = expr match {
    case ast.Null(span) => debug("expr").flatMap(_ => addDecls(Map()))
    case ast.Number(value, span) => debug("expr").flatMap(_ => addDecls(Map()))
    case id@Identifier(name, span) => env.flatMap(_.get(name) match {
      case Some(decl) => debug("expr").flatMap(_ => addDecls(Map(id -> decl)))
      case None => crash(s"undefined reference to $name", span)
    })
    case BinaryOp(operator, left, right, span) => goOverExprs(List(left, right))
    case CallFuncExpr(targetFun, args, span) => goOverExprs(targetFun :: args)
    case Input(span) => debug("expr").flatMap(_ => addDecls(Map()))
    case Alloc(expr, span) => go(expr)
    case VarRef(id, span) => go(id)
    case Deref(pointer, span) => go(pointer)
    case Record(fields, span) => ???
    case FieldAccess(record, field, span) => ???
  }

  private def goOverExprs(args: Iterable[Expr]): Analysis[Unit] = reduce(args)(go)
  private def goOver(stmts: Iterable[Stmt]): Analysis[Unit] = reduce(stmts)(go)
}
