package microc.cli

import microc.util.parsing.combinator.SeqParsers

import java.io.File

case class CliException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

class CliParser extends SeqParsers[String] {
  type Opt[T] = Parser[T => T]

  // ----------------------------------------------------------------------------
  // METAVARS
  // ----------------------------------------------------------------------------

  lazy val NUM = elem ^? { _.toIntOption.toRight("expected number") }
  lazy val FILE = elem.withMessage(_ => "missing filename") ^^ (new File(_))
  lazy val NAME = elem

  // ----------------------------------------------------------------------------
  // OPTIONS
  // ----------------------------------------------------------------------------

  lazy val ExportOpts: Opt[ExportAction] = options(
    "--indent" ~> NUM ^^ { arg => action =>
      action.copy(indent = Some(arg))
    },
    "--output" ~> FILE ^^ { arg => action =>
      action.copy(outputFile = Some(arg))
    },
    "--parser" ~> NAME ^^ { arg => action =>
      action.copy(parserName = arg)
    }
  )

  lazy val RunOpts: Opt[RunAction] = options(
    "--ascii" ^^^ { action =>
      action.copy(ascii = true)
    },
    "--output" ^^^ { action =>
      action.copy(output = true)
    },
    "--parser" ~> NAME ^^ { arg => action =>
      action.copy(parserName = arg)
    }
  )

  // ----------------------------------------------------------------------------
  // ACTIONS
  // ----------------------------------------------------------------------------

  lazy val Export: Parser[ExportAction] =
    "export" ~> ExportOpts.* ~ FILE ^^ { case opts ~ file => opts.foldLeft(ExportAction(file))((acc, x) => x(acc)) }

  lazy val Run: Parser[RunAction] =
    "run" ~> RunOpts.* ~ FILE ^^ { case opts ~ file => opts.foldLeft(RunAction(file))((acc, x) => x(acc)) }

  lazy val Help = "--help" ^^^ PrintHelpAction

  lazy val Actions: Parser[Action] = Help | Run | Export | failure("invalid action")

  // ----------------------------------------------------------------------------
  // HELPERS
  // ----------------------------------------------------------------------------

  private lazy val InvalidOption = elem.filter(_.startsWith("-")) ^! { opt =>
    s"invalid option $opt"
  }

  private def options[A](opts: Parser[A => A]*): Parser[A => A] = {
    opts.reduce(_ | _) | InvalidOption
  }

  def parse(args: Array[String]): Action = parseAll(Actions, args) match {
    case Accept(v, _)      => v
    case Reject(msg, _, _) => throw CliException(s"error: $msg")
  }
}
