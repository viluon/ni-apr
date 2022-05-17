package microc.cli

import microc.ast.Span

import scala.annotation.tailrec

class Reporter(source: String, fileName: Option[String] = None) {
  import scala.language.reflectiveCalls
  private val prefix = fileName.map(_ + ":").getOrElse("")

  def formatErrors(srcErrs: Iterable[_ <: {def span: Span; def msg: String}]): String = {
    val errs = srcErrs.toList.sortBy(_.span)
    val firstLine = errs.head.span.from.line
    val lastLine = errs.last.span.to.line

    val bar = " | "
    val noBar = "   "
    assert(bar.length == noBar.length)
    val underline = '¯'

    val lines = (firstLine to lastLine).flatMap(line => {
      val lineNumber = s"% ${math.max(1, math.log10(lastLine).ceil.toInt)}d".format(line)
      (lineNumber + bar + lineContent(line)) :: (errs.find(_.span.containsLine(line)) match {
        case Some(err) =>
          val Span(from, to, maybeHl) = err.span
          val hl = maybeHl.getOrElse(err.span)
          val startCol = if (from.line == line) from.col else 1
          val endCol = if (to.line == line) to.col else lineContent(line).length
          val width = endCol - startCol + 1
          def prefix(str: String) = " " * lineNumber.length + str + " " * (startCol - 1)
          def highlight(str: String) = str.zipWithIndex.map{
            case (ch, i) if ch == underline && hl.containsPos(line, startCol + i) => '^'
            case (ch, _) => ch
          }.mkString
          prefix(bar) + highlight(underline.toString * width) :: (if (line == to.line) List(prefix(noBar) + " " + err.msg) else Nil)
        case None => Nil
      })
    })

    lines.mkString(System.lineSeparator())
  }

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
