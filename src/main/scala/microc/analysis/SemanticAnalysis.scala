package microc.analysis

import microc.ast.{Alloc, AssignStmt, BinaryOp, Block, CallFuncExpr, Decl, Deref, Expr, FieldAccess, FunBlockStmt, Identifier, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, Record, ReturnStmt, Span, Stmt, StmtInNestedBlock, VarRef, VarStmt, WhileStmt}
import microc.cli.Reporter
import microc.util.CharacterSets.NL
import microc.util.ErrorState
import microc.util.ErrorState.{ErrorStateOps, foldLeft, crash => _crash, get => _get, pure => _pure, put => _put}
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
  *   - Use of an undeclared identifier.
  *   - Duplicate identifiers.
  *     note: there is a single namespace (shared by both functions and identifiers)
  *   - Duplicate record field names.
  *   - Assignment to a function.
  *   - Getting address of a function.
  *
  * The result is a map of declarations, i.e., a map of identifiers to their declarations.
  *
  */
class SemanticAnalysis {
  type Env = Map[String, Decl]
  // TODO I guess it would be better to make Declarations a part of the state
  //  so that the analyser can later do many things at once
  type Analysis[A] = ErrorState[List[SemanticError], Env, A]

  // help type inference
  private def get: Analysis[Env] = _get
  private def crash[A](e: String, sp: Span): Analysis[A] = _crash(List(SemanticError(e, sp)))
  private def pure[A](x: A): Analysis[A] = _pure(x)
  private def put(e: Env): Analysis[Unit] = _put(e)

  def analyze(program: Program): Declarations = go(program)(Map()) match {
    case Left(errs) => throw SemanticException(errs)
    case Right((result, _)) => result
  }

  private def go(program: Program): Analysis[Declarations] = for (
    () <- put(program.funs.map(f => (f.name, f: Decl)).toMap);
    decls <- program.funs.foldLeft(pure(Map(): Declarations)) {
          // TODO it's not just ++, we need to check for overlap
      case (acc, decl) => for (soFar <- acc; decls <- go(decl.block)) yield soFar ++ decls
    }
  ) yield decls

  private def go(stmt: Stmt): Analysis[Declarations] = stmt match {
    case block: StmtInNestedBlock => block match {
      case AssignStmt(left, right, span) => for (
        leftDecls <- go(left);
        rightDecls <- go(right)
      ) yield leftDecls ++ rightDecls
      case NestedBlockStmt(body, span) => goOver(body)
      case IfStmt(guard, thenBranch, elseBranch, span) => ???
      case WhileStmt(guard, block, span) => ???
      case OutputStmt(expr, span) => ???
    }
    case block: Block => block match {
      case NestedBlockStmt(body, span) => goOver(body)
      case FunBlockStmt(vars, stmts, ret, span) => for (
        savedEnv <- get;
        () <- put(vars.flatMap(v => v.decls).map(id => (id.name, id: Decl)).toMap);
        decls <- goOver(stmts :+ ret);
        () <- put(savedEnv)
      ) yield decls
    }
    case ReturnStmt(expr, span) => go(expr)
    case VarStmt(decls, span) => ???
  }

  @tailrec
  private def go(expr: Expr): Analysis[Declarations] = expr match {
    case ast.Null(span) => pure(Map())
    case ast.Number(value, span) => pure(Map())
    case id@Identifier(name, span) => get.flatMap(_.get(name) match {
      case Some(decl) => pure(Map(id -> decl))
      case None => crash(s"undefined reference to $name", span)
    })
    case BinaryOp(operator, left, right, span) => goOverExprs(List(left, right))
    case CallFuncExpr(targetFun, args, span) => goOverExprs(targetFun :: args)
    case Input(span) => pure(Map())
    case Alloc(expr, span) => go(expr)
    case VarRef(id, span) => get.flatMap(_.get(id.name) match {
      case Some(decl) => pure(Map(id -> decl))
      case None => crash(s"undefined reference to ${id.name}", span)
    })
    case Deref(pointer, span) => go(pointer)
    case Record(fields, span) => ???
    case FieldAccess(record, field, span) => ???
  }

  private def goOverExprs(args: Iterable[Expr]): Analysis[Declarations] = {
    // TODO here too
    foldLeft(args)(Map(): Declarations)((acc, expr) => go(expr).map(decls => acc ++ decls))
  }

  private def goOver(stmts: Iterable[Stmt]): Analysis[Declarations] =
    foldLeft(stmts)(Map(): Declarations) {
      // TODO see the first go()
      (acc, stmt) => go(stmt).map(decls => acc ++ decls)
    }
}
