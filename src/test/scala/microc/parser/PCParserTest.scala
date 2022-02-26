package microc.parser

import microc.ast.{Expr, Loc, Program, Span, Stmt}

class PCParserTest extends AbstractParserTest {
  override def parser: Parser = new PCParser

  checkFail[Stmt](
    """|return = 1;
       |""".stripMargin,
    ParseException("expected expression", Span(Loc(1, 1), Loc(1, 1)))
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
    ParseException("expected 'return', got '= 1;'", Span(Loc(5, 3), Loc(5, 3)))
  )

  checkFail[Expr](
    "f(x,y,)",
    ParseException("expected expression", Span(Loc(1, 7), Loc(1, 7)))
  )

  checkFail[Program](
    """|f() { 
       |  return 1; 
       |} 
       |g() { 
       |  return 2 
       |}""".stripMargin,
    ParseException("expected ';', got '}'", Span(Loc(6, 1), Loc(6, 1)))
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
    ParseException("expected expression", Span(Loc(2, 12), Loc(2, 12)))
  )

  checkFail[Program](
    """|f(x) {
       |  var
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected identifier, got keyword 'return'", Span(Loc(3, 3), Loc(3, 3)))
  )
}
