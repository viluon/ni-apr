package microc.parser

import microc.ast.{Expr, Program, Stmt}

class PCParserTest extends AbstractParserTest {
  override def parser: Parser = new PCParser

  checkFail[Stmt](
    "return = 1;",
    """|[1:1]: error: expected expression
       |  return = 1;
       |  ^
       |""".stripMargin
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
    """|[5:3]: error: expected 'return', got '= 1;'
       |    = 1;
       |    ^
       |""".stripMargin
  )

  checkFail[Expr](
    "f(x,y,)",
    """|[1:7]: error: expected expression
       |  f(x,y,)
       |        ^
       |""".stripMargin
  )

  checkFail[Program](
    """|f() { 
       |  return 1; 
       |} 
       |g() { 
       |  return 2 
       |}""".stripMargin,
    """|[6:1]: error: expected ';', got '}'
       |  }
       |  ^
       |""".stripMargin
  )

  checkFail[Program](
    """|f(x, ) {
       |  return 1;
       |}
       |""".stripMargin,
    """|[1:6]: error: expected identifier, got ')'
       |  f(x, ) {
       |       ^
       |""".stripMargin
  )

  checkFail[Program](
    """|f(x) {
       |  return 1+;
       |}
       |""".stripMargin,
    """|[2:12]: error: expected expression
       |    return 1+;
       |             ^
       |""".stripMargin
  )

  checkFail[Program](
    """|f(x) {
       |  var
       |  return 1;
       |}
       |""".stripMargin,
    """|[3:3]: error: expected identifier, got keyword 'return'
       |    return 1;
       |    ^
       |""".stripMargin
  )
}
