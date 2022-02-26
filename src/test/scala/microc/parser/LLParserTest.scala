package microc.parser

import microc.ast.{Loc, Program, Span, Stmt}

class LLParserTest extends AbstractParserTest {
  override def parser: Parser = new LLParser

  checkFail[Stmt](
    """|return = 1;
       |""".stripMargin,
    ParseException("expected expression, got 'return'", Span(Loc(1, 1), Loc(1, 1)))
  )

  checkFail[Program](
    """|f() {
       |
       |
       |
       |  = 1;
       |
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected expression, got '='", Span(Loc(5, 3), Loc(5, 3)))
  )

  checkFail[Program](
    """|f(x, ) {
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected identifier, got ')'", Span(Loc(1, 6), Loc(1, 6)))
  )

  checkFail[Program](
    """|f(x) {
       |  return 1+;
       |}
       |""".stripMargin,
    ParseException("expected expression, got ';'", Span(Loc(2, 12), Loc(2, 12)))
  )

  checkFail[Program](
    """|f(x) {
       |  var
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected identifier, got 'return'", Span(Loc(3, 3), Loc(3, 3)))
  )
}
