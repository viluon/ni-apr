package microc.util.parsing.combinator

import munit.{FunSuite, Location}

class ParserCombinatorTest extends FunSuite with StringParsers {
  test("expression") {
    sealed trait Expr
    case class Add(l: Expr, r: Expr) extends Expr
    case class Mul(l: Expr, r: Expr) extends Expr
    case class Num(n: Int) extends Expr

    def expr: Parser[Expr] = chainl1(term, "+" ^^^ Add) label "expr"
    def term: Parser[Expr] = chainl1(factor, "*" ^^^ Mul) label "term"
    def factor: Parser[Expr] = num | "(" ~> expr <~ ")" label "factor"
    def num: Parser[Expr] = """\d+""".r ^^ (x => Num(x.toInt)) label "num"
    def prg: Parser[Expr] = expr label "prg"

    {
      val Accept(v, r) = parseAll(prg, "1 + 2")
      assertEquals(v, Add(Num(1), Num(2)))
      assertEquals(r.length, 0)
    }

    {
      val Accept(v, r) = parseAll(prg, "1 + 2 * 3")
      assertEquals(v, Add(Num(1), Mul(Num(2), Num(3))))
      assertEquals(r.length, 0)
    }
  }

  test("literal") {
    val p = lit("abc")
    assertAccept(p, "abcd", "abc", "d")
    assertReject(p, "abd", "abd", Some("expected 'abc', got 'abd'"))
  }

  test("regex") {
    val p = regex("""\d+""".r)
    assertAccept(p, "1234|", "1234", "|")
    assertReject(p, "|1234", "|1234", Some("expected regex '\\d+', got '|'"))
  }

  test("~") {
    val p = lit("a") ~ lit("bc")
    assertAccept(p, "abc|", new ~("a", "bc"), "|")
    assertReject(p, "abd|", "bd|", Some("expected 'bc', got 'bd'"))
  }

  test("~>") {
    val p = lit("(") ~> lit("abc")
    assertAccept(p, "(abc|", "abc", "|")
    assertReject(p, "abc|", "abc|", Some("expected '(', got 'a'"))
  }

  test("<~") {
    val p = lit("abc") <~ lit(")")
    assertAccept(p, "abc)|", "abc", "|")
    assertReject(p, "abc|", "|", Some("expected ')', got '|'"))
  }

  test("|") {
    val p = lit("a") | lit("b") withMessage ("expected 'a' | 'b', got '" + _.charAt(0) + "'")
    assertAccept(p, "ab|", "a", "b|")
    assertAccept(p, "ba|", "b", "a|")
    val Reject(msg, loc, backtracking) = parse(p, "ca|")
    assertEquals(msg, "expected 'a' | 'b', got 'c'")
    assertEquals(loc.pos, Position(1, 1))
    assertEquals(backtracking, true)
  }

  test("?") {
    val p = lit("a").?
    assertAccept(p, "a|", Some("a"), "|")
    assertAccept(p, "b|", None, "b|")
  }

  test("*") {
    val p = lit("a").*
    assertAccept(p, "aaab|", List("a", "a", "a"), "b|")
    assertAccept(p, "baaa|", Nil, "baaa|")
  }

  test("+") {
    val p = lit("a").+
    assertAccept(p, "aaab|", List("a", "a", "a"), "b|")
    assertAccept(p, "ab|", List("a"), "b|")
    assertReject(p, "ba|", "ba|", Some("expected 'a', got 'b'"))
  }

  test("+ with |") {
    val p = (lit("a") | lit("b")).+
    assertAccept(p, "abba|", List("a", "b", "b", "a"), "|")
    assertAccept(p, "ba|", List("b", "a"), "|")
    assertReject(p, "cba|", "cba|", Some("expected 'b', got 'c'"))
  }

  test("attempt") {
    val p1 = "a" ~ "b"
    val p2 = "a" ~ "c"

    val p = p1 | p2
    assertReject(p, "ac|", "c|", Some("expected 'b', got 'c'"))

    val q = attempt(p1) | p2
    assertAccept(q, "ac|", new ~("a", "c"), "|")
  }

  test("last failure") {
    val p = repsep(lit("a"), lit(","))
  }

  test("repsep") {
    val p = repsep(lit("a"), lit(","))
    assertAccept(p, "a,a,a|", List("a", "a", "a"), "|")
    assertAccept(p, "a|", List("a"), "|")
    assertAccept(p, "b,|", Nil, "b,|")
    // note: if the separator matches, it will try to get the next one
    // an alternative would be to define using attempt: `p ~ rep(attempt(s ~> p))`
    assertReject(p, "a,a,a,|", "|", Some("expected 'a', got '|'"), all = true)
  }

  test("rep1sep") {
    val p = rep1sep(lit("a"), lit(","))
    assertAccept(p, "a,a,a|", List("a", "a", "a"), "|")
    assertAccept(p, "a|", List("a"), "|")
    assertReject(p, "b,|", "b,|", Some("expected 'a', got 'b'"))
    // cf. note on repsep
    assertReject(p, "a,a,a,|", "|", Some("expected 'a', got '|'"), all = true)
  }

  test("not") {
    val p = !lit("a")
    assertAccept(p, "b|", (), "b|")
    assertReject(p, "a|", "a|", Some("unexpected 'a'"))
  }

  test("failure") {
    val p = lit("a") | lit("b") | failure("expected a or b")
    val Reject(msg, rem, backtrack) = parse(p, "c")
    assertEquals(msg, "expected a or b")
    assertEquals(rem.pos, Position(1, 1))
    assertEquals(backtrack, true)
  }

  test("whitespace") {
    object P extends StringParsers {
      def run(): Unit = {
        val p = cursor ~ lit("abc") ~ cursor ~ lit("abd") ~ cursor

        val Accept(c1 ~ abc ~ c2 ~ abd ~ c3, rem) = parse(p, "  abc abd   ")

        assertEquals(c1, Position(1, 3))
        assertEquals(abc, "abc")
        assertEquals(c2, Position(1, 7))
        assertEquals(abd, "abd")
        assertEquals(c3, Position(1, 13))
        // cursor does not eat any space
        // the reason is that it it would otherwise move the input
        // and stopped any backtracing
        assertEquals(rem.content, "   ")
      }
    }

    P.run()
  }

  test("end of input with whitespace") {
    object P extends StringParsers {
      def run(): Unit = {
        val p1 = cursor ~ "a"
        val Accept(c ~ v, r1) = parseAll(p1, "  a  ")
        assertEquals(c, Position(1, 3))
        assertEquals(v, "a")
        // it is an empty string as it was eaten by the EOI
        assertEquals(r1.content, "")

        val p2 = "b"
        val Accept(v2, r2) = parseAll(p2, "  b  ")
        assertEquals(v2, "b")
        // it is an empty string as it was eaten by the EOI
        assertEquals(r2.content, "")
      }
    }

    P.run()
  }

  test("position") {
    val abc = lit("abc")
    val abd = lit("abd")
    val p = abc ~ cursor ~ abd
    val Accept(a ~ c ~ b, rem) = parse(p, "abcabde")

    assertEquals(a, "abc")
    assertEquals(c, Position(1, 4))
    assertEquals(b, "abd")
    assertEquals(rem.content, "e")
  }

  test("recursion") {
    def d: Parser[String] = ("*" ~ e1) ^^ { case x ~ y => x + "(" + y }
    def e1: Parser[String] = d | "a"
    assertAccept(e1, "**a", "*(*(a", "")
  }

  protected def assertAccept[A](p: Parser[A], in: String, expected: A, expectedRem: String, all: Boolean = false)(
      implicit loc: Location
  ): Unit = {
    val r = if (all) parseAll(p, in) else parse(p, in)
    assert(r.success)
    val Accept(actual, rem) = r
    assertEquals(actual, expected)
    assertEquals(rem.content, expectedRem)
  }

  protected def assertReject[A](p: Parser[A],
                                in: String,
                                expectedRem: String,
                                expectedMsg: Option[String] = None,
                                all: Boolean = false)(
      implicit loc: Location
  ): Unit = {
    val r = if (all) parseAll(p, in) else parse(p, in)
    assert(!r.success)
    val Reject(msg, rem, _) = r
    expectedMsg.foreach(assertEquals(msg, _))
    assertEquals(rem.content, expectedRem)
  }
}
