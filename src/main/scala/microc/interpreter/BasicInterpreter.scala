package microc.interpreter

import microc.analysis.Declarations
import microc.ast._

import java.io.{Reader, Writer}

class BasicInterpreter(program: Program, declarations: Declarations, stdin: Reader, stdout: Writer)
  extends Interpreter {

  def run(mainArgs: List[Int] = Nil): Int = ???
}
