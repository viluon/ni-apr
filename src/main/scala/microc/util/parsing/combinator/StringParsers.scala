package microc.util.parsing.combinator

import scala.language.implicitConversions
import scala.util.matching.Regex

/**
  * A character reader
  *
  * @param source The underlying string
  * @param start The start index (inclusive) from the beginning of the underlying string
  * @param end The end index (exclusive) from the beginning of the underlying string
  */
class CharInput(source: String, val start: Int, val end: Int) extends CharSequence with FiniteInput { outer =>
  def this(source: String) = this(source, 0, source.length)
  private final val EOF = '\u001a'

  protected lazy val lineStarts: Array[Int] = genIndex

  protected def genIndex: Array[Int] = {
    var lineStarts = List(0)

    for (i <- 0 until source.length) {
      if (source.charAt(i) == '\n' ||
          (source.charAt(i) == '\r' && (i == (source.length - 1) || source.charAt(i + 1) != '\n'))) {
        lineStarts = (i + 1) :: lineStarts
      }
    }

    (source.length :: lineStarts).reverse.toArray
  }

  protected lazy val line: Int = {
    var l = 0
    var h = lineStarts.length - 1
    while (l + 1 < h) {
      val m = l + (h - l) / 2
      if (start < lineStarts(m)) h = m else l = m
    }
    l + 1
  }

  protected lazy val col: Int = start - lineStarts(line - 1) + 1

  lazy val pos: Position = Position(line, col)

  def content: String = source.substring(start, end)

  override def charAt(index: Int): Char = if (isEmpty) EOF else source.charAt(start + index)

  override def isEmpty: Boolean = end == start

  override def length: Int = end - start

  def drop(n: Int): CharInput = subSequence(n, length)

  override def subSequence(start: Int, end: Int): CharInput =
    new CharInput(source, this.start + start, this.start + end) {
      override protected lazy val lineStarts: Array[Int] = outer.lineStarts
    }

  override def toString: String = s"CharInput('${charAt(0)}', $start, $length)"
}

object CharInput {
  implicit def apply(s: String): CharInput = new CharInput(s)
}

case class Position(line: Int, col: Int) {
  override def toString: String = s"$line:$col"
}

trait StringParsers extends Parsers {
  override type Input = CharInput

  implicit def string2parser(s: String): Parser[String] = lit(s)
  implicit def regex2parser(r: Regex): Parser[String] = regex(r)

  protected val whiteSpaceRegex: Option[Regex] = Some("""\s+""".r)

  protected def handleWhiteSpace(in: Input): Input = {
    for {
      r <- whiteSpaceRegex
      m <- r.findPrefixMatchOf(in)
    } yield handleWhiteSpace(in.drop(m.end)) // there can be more consecutive comments
  }.getOrElse(in)

  lazy val cursor: Parser[Position] =
    Parser { in =>
      val in2 = handleWhiteSpace(in)
      Accept(in2.pos, in)
    } label "cursor"

  lazy val elem: Parser[Char] =
    Parser { in =>
      val in2 = handleWhiteSpace(in)
      if (in2.isEmpty) Reject("expected a char, got end of input", in)
      else Accept(in2.charAt(0), in2.drop(1))
    } label "a char"

  def lit(s: String): Parser[String] =
    Parser { in =>
      val in2 = handleWhiteSpace(in)
      var i = 0
      while (i < in2.length && i < s.length && in2.charAt(i) == s.charAt(i)) {
        i += 1
      }
      if (i == s.length) {
        Accept(s, in2.drop(i))
      } else {
        val found = if (i == in2.length) {
          "end of input"
        } else {
          "'" + in2.subSequence(0, math.min(in2.length, s.length)).content.trim + "'"
        }
        Reject("expected '" + s + "', got " + found, in)
      }
    } label s"'$s'"

  def regex(r: Regex): Parser[String] =
    Parser { in =>
      val in2 = handleWhiteSpace(in)
      r.findPrefixMatchOf(in2) match {
        case Some(m) =>
          Accept(in2.subSequence(0, m.end).content, in2.drop(m.end))
        case None =>
          val found = if (in2.isEmpty) "end of input" else "'" + in2.charAt(0) + "'"
          Reject(s"expected regex '$r', got " + found, in)
      }
    } label s"'$r'"

  override def parseAll[A](p: Parser[A], in: CharInput)(implicit ev: CharInput => FiniteInput): Result[A] = {
    super.parseAll(p <~ "", in)(ev)
  }
}
