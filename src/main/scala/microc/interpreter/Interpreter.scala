package microc.interpreter

import microc.ProgramException
import microc.analysis.SemanticAnalysis
import microc.ast._
import microc.cli.Reporter

import java.io.{Reader, Writer}

case class ExecutionException(message: String, sp: Span) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatErrors(List(new {
    def msg: String = message
    def span: Span = sp
  }))
}

trait Interpreter {
  def run(mainArgs: List[Int]): Int
}

object Interpreter {
  def apply(program: Program, stdin: Reader, stdout: Writer): Interpreter = {
    val (declarations, _) = new SemanticAnalysis().analyze(program)
    new BasicInterpreter(program, declarations, stdin, stdout)
  }
}