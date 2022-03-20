package microc.parser

import microc.ProgramException
import microc.ast.{Expr, Program, Span, Stmt}
import microc.cli.Reporter

/**
  * Parser problem
  *
  * @param message the detail what went wrong
  * @param sp the location of the error in the source
  */
case class ParseException(message: String, sp: Span) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatErrors(List(new {
    def msg: String = message
    def span: Span = sp
  }))
}

object Parser {
  val DefaultParserName = "ll"

  val Parsers = Map(
    "ll" -> (() => new LLParser),
    "peg" -> (() => new PCParser)
  )

  def apply(): Parser = apply(DefaultParserName).get

  def apply(name: String): Option[Parser] = Parsers.get(name).map(x => x())
}

/**
  * Unified interface for parsing microC source code
  */
trait Parser {
  def parseProgram(source: String): Program
  def parseExpr(source: String): Expr
  def parseStmt(source: String): Stmt
}
