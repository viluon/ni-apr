package microc.util.parsing.combinator

import scala.annotation.tailrec

trait FiniteInput {
  def isEmpty: Boolean
}

trait Parsers {
  type Input

  sealed trait Result[+A] {
    def success: Boolean
    def map[B](f: A => B): Result[B]
    def flatMap[B](f: A => Parser[B]): Result[B]
  }

  case class Accept[+A](value: A, reminding: Input) extends Result[A] {
    override def success: Boolean = true

    override def map[B](f: A => B): Result[B] = Accept(f(value), reminding)

    override def flatMap[B](f: A => Parser[B]): Result[B] = f(value)(reminding)
  }

  case class Reject(message: String, reminding: Input, fatal: Boolean = true) extends Result[Nothing] {
    override def success: Boolean = false

    override def map[B](f: Nothing => B): Result[B] = this

    override def flatMap[B](f: Nothing => Parser[B]): Result[B] = this
  }

  case class ~[+A, +B](a: A, b: B) {
    override def toString: String = s"$a~$b"
  }

  abstract class Parser[+A] extends (Input => Result[A]) {
    private var _label: () => String = () => ""

    def label: String = _label()
    def label(newLabel: => String): this.type = {
      lazy val l = newLabel
      this._label = () => l
      this
    }

    override def toString(): String = s"Parser ($label)"

    def flatMap[B](f: A => Parser[B]): Parser[B] = Parser(in => this(in) flatMap f)

    // alternative flatMap (x => success(f(x)))
    // alternative flatMap (x => success[B].compose(f))
    def map[B](f: A => B): Parser[B] = Parser(in => this(in) map f)

    def filter(p: A => Boolean): Parser[A] = withFilter(p)

    def withFilter(p: A => Boolean): Parser[A] = filterWithMessage(p, "unexpected: " + _)

    def filterWithMessage(p: A => Boolean, msg: A => String): Parser[A] = Parser { in =>
      this(in) match {
        case Accept(v, r) if p(v) => Accept(v, r)
        case Accept(v, _)         => Reject(msg(v), in)
        case r: Reject            => r
      }
    }

    def withMessage(msg: Input => String): Parser[A] = Parser { in =>
      this(in) match {
        case Accept(v, r)    => Accept(v, r)
        case Reject(_, r, b) => Reject(msg(r), r, b)
      }
    }

    def ^^[B](f: A => B): Parser[B] = map(f)

    def ^?[B](f: A => Either[String, B]): Parser[B] = Parser { in =>
      this(in) match {
        case Accept(value, reminding) =>
          f(value) match {
            case Left(v)  => Reject(v, in)
            case Right(v) => Accept(v, reminding)
          }
        case r: Reject => r
      }
    }

    def ^!(f: A => String): Parser[Nothing] = Parser { in =>
      this(in) match {
        case Accept(value, reminding) => Reject(f(value), reminding)
        case r: Reject                => r
      }
    }

    def ^^^[B](v: => B): Parser[B] = {
      lazy val v0 = v
      map(_ => v0)
    }

    def ? : Parser[Option[A]] = opt(this)

    def * : Parser[List[A]] = rep(this)

    def + : Parser[List[A]] = rep1(this)

    def unary_! : Parser[Unit] = not(this)

    def ~[B](p: => Parser[B]): Parser[A ~ B] = and(this, p)

    def ~>[B](p: => Parser[B]): Parser[B] = {
      lazy val p0 = p
      (for (_ <- this; x <- p0) yield x) label (this.label + "~>" + p0.label)
    }

    def <~[B](p: => Parser[B]): Parser[A] = {
      lazy val p0 = p
      (for (x <- this; _ <- p0) yield x) label (this.label + "<~" + p0.label)
    }

    def |[B >: A](p: => Parser[B]): Parser[B] = or(this, p)
  }

  def Parser[A](f: Input => Result[A]): Parser[A] = new Parser[A] {
    override def apply(input: Input): Result[A] = f(input)
  }

  def attempt[A](p: => Parser[A]): Parser[A] = {
    lazy val p0 = p

    Parser { in =>
      p0(in) match {
        case r: Reject => r.copy(fatal = false)
        case x         => x
      }
    } label ("attempt(" + p0.label + ")")
  }

  def rep[A](p: => Parser[A]): Parser[List[A]] = rep1(p) | success(Nil)

  // alternative: for (x <- p; xs <- rep(p)) yield x :: xs
  def rep1[A](p: => Parser[A]): Parser[List[A]] = {
    lazy val p0 = p

    Parser { in =>
      @tailrec def loop(in2: Input, res: List[A]): Result[List[A]] = p0(in2) match {
        case Accept(v, r)                    => loop(r, v :: res)
        case r: Reject if res.isEmpty        => r
        case r: Reject if !r.fatal           => Accept(res.reverse, in2)
        case r: Reject if in2 != r.reminding => r
        case r: Reject                       => Accept(res.reverse, in2)
      }

      loop(in, Nil)
    } label ("(" + p0.label + ")+")
  }

  def repsep[A, B](p: => Parser[A], s: => Parser[B]): Parser[List[A]] =
    rep1sep(p, s) | success(Nil) label "*,"

  def rep1sep[A](p: => Parser[A], s: => Parser[Any]): Parser[List[A]] =
    p ~ rep(s ~> p) ^^ { case x ~ xs => x :: xs } label "+,"

  def chainl1[A](p: => Parser[A], s: => Parser[(A, A) => A]): Parser[A] = chainl1(p, p, s)

  def chainl1[A, B](first: => Parser[A], p: => Parser[B], s: => Parser[(A, B) => A]): Parser[A] = {
    lazy val p0 = p
    lazy val s0 = s

    first ~ rep(s0 ~ p0) ^^ {
      case x ~ xs => xs.foldLeft(x) { case (a, f ~ b) => f(a, b) }
    } label "chainl1"
  }

  def opt[A](p: => Parser[A]): Parser[Option[A]] = (p ^^ Some.apply) | success(None) label (p.label + "?")

  def and[A, B](p1: => Parser[A], p2: => Parser[B]): Parser[A ~ B] = {
    lazy val p10 = p1
    lazy val p20 = p2

    (for (a <- p10; b <- p20) yield new ~(a, b)) label (p10.label + " ~ " + p20.label)
  }

  def or[A](p1: => Parser[A], p2: => Parser[A]): Parser[A] = {
    lazy val p10 = p1
    lazy val p20 = p2

    Parser { in =>
      p10(in) match {
        case Accept(v, r)                   => Accept(v, r)
        case r: Reject if !r.fatal          => p20(in)
        case r: Reject if in == r.reminding => p20(in)
        case r: Reject                      => r
      }
    } label (p10.label + " | " + p20.label)
  }

  def not(p: => Parser[_]): Parser[Unit] = {
    lazy val p0 = p

    Parser { in =>
      p0(in) match {
        case Accept(_, _) => Reject(s"unexpected ${p.label}", in)
        case _            => Accept((), in)
      }
    }
  }

  def failure(f: Input => String): Parser[Nothing] = Parser(in => Reject(f(in), in)) label "failure"

  def failure(msg: => String): Parser[Nothing] = failure(_ => msg)

  def success[A](v: => A): Parser[A] = Parser(in => Accept(v, in)) label "success"

  def parseAll[A](p: Parser[A], in: Input)(implicit ev: Input => FiniteInput): Result[A] = {
    val x = parse(p, in)
    x match {
      case Accept(v, r) if ev(r).isEmpty => Accept(v, r)
      case Accept(_, r)                  => Reject("end of input expected", r)
      case r: Reject                     => r
    }
  }

  def parse[A](p: Parser[A], in: Input): Result[A] = p(in)

  implicit class IterableFiniteInput(xs: Iterable[_]) extends FiniteInput {
    override def isEmpty: Boolean = xs.isEmpty
  }
}
