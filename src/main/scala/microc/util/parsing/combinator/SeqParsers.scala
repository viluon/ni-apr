package microc.util.parsing.combinator

import scala.language.implicitConversions

trait SeqParsers[T] extends Parsers {
  override type Input = Iterable[T]

  lazy val elem: Parser[T] = Parser { in =>
    in.headOption match {
      case Some(v) => Accept(v, in.tail)
      case None    => Reject(s"End of input", in)
    }
  }

  def lit(s: T): Parser[T] =
    elem.filterWithMessage(x => x == s, "Expected '" + s + "', got '" + _ + "'")

  implicit def lit2parser(s: T): Parser[T] = lit(s)
}
