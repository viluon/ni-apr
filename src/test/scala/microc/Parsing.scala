package microc

import microc.ast.Program
import microc.parser.Parser
import microc.util.Files.FilesOps

import java.io.File

trait Parsing {
  private def parser = Parser()

  def parseUnsafe(code: String): Program = {
    parser.parseProgram(code)
  }

  def parseUnsafe(file: File): Program = {
    parser.parseProgram(file.readAll())
  }
}
