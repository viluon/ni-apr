package microc.cli

import microc.ProgramException
import microc.analysis.SemanticAnalysis
import microc.ast.JSONAstPrinter
import microc.interpreter.BasicInterpreter
import microc.parser.Parser
import microc.util.CharacterSets.NL
import microc.util.Files._
import microc.util.json.JSONPrettyPrinter
import microc.util.{ASCIIReader, ASCIIWriter}

import java.io._
import scala.util.Using

sealed trait Action {
  def run(): Int
}

case object PrintHelpAction extends Action {
  def run(): Int = {
    println("""
         |usage: uc <ACTION>
         |
         |actions:
         |  export [options] FILE  exports the microC program in FILE to JSON
         |  
         |    options:
         |    --indent NUM         indent the result by NUM (default: no indent)
         |    --parser NAME        specify parser which parser to use
         |    --output FILE        specify the output file
         |  
         |  run [options] FILE     runs the microC program in FILE
         |    
         |    options:
         |    --ascii              convert input/output to/from ASCII codes
         |    --output             consider the return from main as output
         |    --parser NAME        specify parser which parser to use
         |""".stripMargin)
    0
  }
}

trait ParsingAction {
  def parserName: String

  protected def parser: Parser = Parser(parserName).getOrElse(throw CliException(s"unknown parser $parserName"))

  protected def readInput(file: File): String =
    try {
      file.readAll()
    } catch {
      case e: Exception => throw CliException(s"unable to read $file", e)
    }
}

case class RunAction(file: File,
                     ascii: Boolean = false,
                     output: Boolean = false,
                     parserName: String = Parser.DefaultParserName)
    extends Action
    with ParsingAction {
  override def run(): Int = {
    val source = readInput(file)
    val reporter = new Reporter(source, Some(file.getPath))

    try {
      val program = parser.parseProgram(source)
      val declarations = new SemanticAnalysis().analyze(program)
      val stdout = createOutput
      val interpreter = new BasicInterpreter(program, declarations, createInput, stdout)
      val status = interpreter.run()
      val exit = if (output) {
        stdout.write(status.toString + NL)
        0
      } else {
        status
      }

      stdout.flush()
      exit
    } catch {
      case e: ProgramException =>
        System.err.println(e.format(reporter))
        1
    }
  }

  protected def createOutput: Writer = {
    val x = new OutputStreamWriter(System.out)
    if (ascii) {
      new ASCIIWriter(x)
    } else {
      x
    }
  }

  protected def createInput: Reader = {
    val x = new InputStreamReader(System.in)
    if (ascii) {
      new ASCIIReader(x)
    } else {
      x
    }
  }
}

case class ExportAction(file: File,
                        outputFile: Option[File] = None,
                        indent: Option[Int] = None,
                        parserName: String = Parser.DefaultParserName)
    extends Action
    with ParsingAction {

  override def run(): Int = {
    val source = readInput(file)
    val reporter = new Reporter(source)

    try {
      val program = parser.parseProgram(source)
      val printer = new JSONAstPrinter
      val json = printer.serialize(program)
      val out = outputFile.map(new FileWriter(_)).getOrElse(new OutputStreamWriter(System.out))

      Using(out) { stream =>
        new JSONPrettyPrinter(indent).print(json, stream)
      }.get

      0
    } catch {
      case e: ProgramException =>
        System.err.println(e.format(reporter))
        1
    }
  }
}
