package microc

import microc.cli.Reporter

abstract class ProgramException(message: String) extends RuntimeException(message) {
  def format(reporter: Reporter): String
}
