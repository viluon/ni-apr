package microc.analysis

import microc.ProgramException
import microc.ast.{AssignStmt, AstNode, AstVisitor, Decl, DirectFieldWrite, DirectWrite, FunBlockStmt, FunDecl, Identifier, IndirectFieldWrite, IndirectWrite, Loc, Program, Record, VarRef}
import microc.cli.Reporter
import microc.util.CharacterSets.NL

import scala.collection.immutable.{Iterable, Map, Set}
import scala.collection.mutable
import scala.util.DynamicVariable

case class SemanticError(msg: String, loc: Loc)

case class SemanticException(errors: List[SemanticError]) extends ProgramException("Semantic exception") {
  override def format(reporter: Reporter): String =
    errors.sortBy(_.loc).map(x => reporter.formatError("semantic", x.msg, x.loc)).mkString(NL)
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

  def analyze(program: Program): Declarations = ???
}
