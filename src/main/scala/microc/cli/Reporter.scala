package microc.cli

import microc.ast.Loc

import scala.annotation.tailrec

class Reporter(source: String, fileName: Option[String] = None) {
  val prefix = fileName.map(_ + ":").getOrElse("")

  def formatError(kind: String, message: String, loc: Loc): String =
    s"""$prefix[$loc]: $kind error: $message
       |  ${lineContent(loc.line)}
       |  ${" " * (loc.col - 1)}^
       |""".stripMargin

  private def lineContent(line: Int): String = {
    @tailrec def loop(idx: Int, lastLine: Int, rem: Int): String = {
      if (idx == source.length) {
        source.substring(lastLine)
      } else if (source.charAt(idx) == '\n') {
        if (rem == 1) {
          source.substring(lastLine, idx)
        } else {
          loop(idx + 1, idx + 1, rem - 1)
        }
      } else {
        loop(idx + 1, lastLine, rem)
      }
    }

    loop(0, 0, line)
  }
}
