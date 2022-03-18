package microc.cli

import microc.ast.Span

import scala.annotation.tailrec

class Reporter(source: String, fileName: Option[String] = None) {
  private val prefix = fileName.map(_ + ":").getOrElse("")

  def formatError(kind: String, message: String, span: Span): String = {
    val Span(from, to, hl) = span
    val width = if (from.line == to.line) to.col - from.col + 1 else 1
    s"""$prefix[$span]: $kind error: $message
       |  ${lineContent(from.line)}
       |  ${" " * (from.col - 1)}${"¯" * width}
       |""".stripMargin
  }

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
