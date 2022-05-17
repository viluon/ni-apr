package microc.cli

import microc.ProgramException
import microc.analysis.dataflow.{ConstantAnalysis, DataFlowAnalysis, FixpointComputation, SignAnalysis}
import microc.analysis.{SemanticAnalysis, TypeAnalysis}
import microc.ast.{AstNormalizer, Decl, Identifier, JSONAstPrinter}
import microc.cfg.Cfg
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
         |
         |  type FILE              typechecks FILE
         |
         |  cfg FILE               exports the control flow graph of FILE to DOT
         |
         |  sign FILE              performs sign analysis on FILE
         |
         |  show-sign-tables       visualise the transfer function lookup tables
         |                         used for sign analysis
         |
         |  const FILE             performs constant analysis on FILE
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
      val declarations = new SemanticAnalysis().analyze(program)._1
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

case class TypeAction(file: File,
                      parserName: String = Parser.DefaultParserName)
  extends Action
    with ParsingAction {
  override def run(): Int = {
    val source = readInput(file)
    val reporter = new Reporter(source, Some(file.getPath))

    try {
      val program = parser.parseProgram(source)
      val (declarations, fieldNames) = new SemanticAnalysis().analyze(program)
      val (errs, types) = TypeAnalysis(declarations, fieldNames).analyze(program)

      for ((decl, t) <- types) println(s"⟦$decl⟧ = $t")

      errs match {
        case mainErr :: additionalErrs =>
          println("\nTypechecking failed:")
          // FIXME using a singleton list here as handling multiple errors in the formatter is buggy atm
          println(reporter.formatErrors(List(mainErr)))
          if (additionalErrs.nonEmpty) {
            println("\nadditional errors (may have been caused by the first error):")
            for (err <- additionalErrs) println(reporter.formatErrors(List(err)))
          }
          2
        case Nil => 0
      }
    } catch {
      case e: ProgramException =>
        System.err.println(e.format(reporter))
        1
    }
  }
}

case class CfgAction(file: File, parserName: String = Parser.DefaultParserName)
  extends Action with ParsingAction {
  override def run(): Int = {
    val source = readInput(file)
    val reporter = new Reporter(source, Some(file.getPath))

    try {
      val program = parser.parseProgram(source)
      // NB: normalization must happen before semantic analysis, otherwise conversion to CFG produces garbage
      val normalized = AstNormalizer.normalize(program)
      val (declarations, fieldNames) = new SemanticAnalysis().analyze(normalized)
      // TODO type analysis too
      val cfg = Cfg.convert(normalized)
      println(cfg.toDot(declarations))
      0
    } catch {
      case e: ProgramException =>
        System.err.println(e.format(reporter))
        1
    }
  }
}

abstract class AnalyseAction(val parserName: String = Parser.DefaultParserName)
  extends Action with ParsingAction {

  protected def analysis(decls: Map[Identifier, Decl], cfg: Cfg.Interprocedural): DataFlowAnalysis with FixpointComputation
  def file: File

  override def run(): Int = {
    val source = readInput(file)
    val reporter = new Reporter(source, Some(file.getPath))

    try {
      val program = parser.parseProgram(source)
      // TODO type analysis too
      val normalized = AstNormalizer.normalize(program)
      System.err.println(AstNormalizer.compare(program, normalized))
      val (declarations, fieldNames) = new SemanticAnalysis().analyze(normalized)
      val cfg = Cfg.convert(normalized)
      val ana = analysis(declarations, cfg)
      val result = ana.fixpoint(cfg, ana.programLat.bot)
      println(cfg.toDot(result.values.flatMap(fn => fn.view.mapValues(env =>
        "\\n" + env.map(p => p._1.name + ": " + p._2).toList.sorted.mkString("{", ",", "}")
      )).toMap, declarations))
      0
    } catch {
      case e: ProgramException =>
        System.err.println(e.format(reporter))
        1
    }
  }
}

case class SignAction(file: File) extends AnalyseAction() {
  override def analysis(decls: Map[Identifier, Decl], cfg: Cfg.Interprocedural): DataFlowAnalysis with FixpointComputation =
    new SignAnalysis(decls, cfg)
}

case class ConstAction(file: File) extends AnalyseAction() {
  override def analysis(decls: Map[Identifier, Decl], cfg: Cfg.Interprocedural): DataFlowAnalysis with FixpointComputation =
    new ConstantAnalysis(decls, cfg)
}

case class ShowSignTablesAction() extends Action {
  override def run(): Int = {
    println(SignAnalysis.renderTables(SignAnalysis.opTable))
    0
  }
}
