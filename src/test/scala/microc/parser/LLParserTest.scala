package microc.parser

import microc.ast.{Loc, Program, Stmt}

class LLParserTest extends AbstractParserTest {
  override def parser: Parser = new LLParser

  checkFail[Stmt](
    """|return = 1;
       |""".stripMargin,
    ParseException("expected expression, got 'return'", Loc(1, 1))
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
    ParseException("expected expression, got '='", Loc(5, 3))
  )

  checkFail[Program](
    """|f(x, ) {
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected identifier, got ')'", Loc(1, 6))
  )

  checkFail[Program](
    """|f(x) {
       |  return 1+;
       |}
       |""".stripMargin,
    ParseException("expected expression, got ';'", Loc(2, 12))
  )

  checkFail[Program](
    """|f(x) {
       |  var
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected identifier, got 'return'", Loc(3, 3))
  )
}
