package microc.parser

import microc.ast.{Expr, Loc, Program, Stmt}

class PCParserTest extends AbstractParserTest {
  override def parser: Parser = new PCParser

  checkFail[Stmt](
    """|return = 1;
       |""".stripMargin,
    ParseException("expected expression", Loc(1, 1))
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
    ParseException("expected 'return', got '= 1;'", Loc(5, 3))
  )

  checkFail[Expr](
    "f(x,y,)",
    ParseException("expected expression", Loc(1, 7))
  )

  checkFail[Program](
    """|f() { 
       |  return 1; 
       |} 
       |g() { 
       |  return 2 
       |}""".stripMargin,
    ParseException("expected ';', got '}'", Loc(6, 1))
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
    ParseException("expected expression", Loc(2, 12))
  )

  checkFail[Program](
    """|f(x) {
       |  var
       |  return 1;
       |}
       |""".stripMargin,
    ParseException("expected identifier, got keyword 'return'", Loc(3, 3))
  )
}
