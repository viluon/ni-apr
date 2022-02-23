package microc.interpreter

import microc.ProgramException
import microc.analysis.SemanticAnalysis
import microc.ast._
import microc.cli.Reporter

import java.io.{Reader, Writer}

case class ExecutionException(message: String, loc: Loc) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatError("execution", message, loc)
}

trait Interpreter {
  def run(mainArgs: List[Int]): Int
}

object Interpreter {
  def apply(program: Program, stdin: Reader, stdout: Writer): Interpreter = {
    val declarations = new SemanticAnalysis().analyze(program)
    new BasicInterpreter(program, declarations, stdin, stdout)
  }
}