package microc.parser

import microc.ast.{Program, Stmt}

class LLParserTest extends AbstractParserTest {
  override def parser: Parser = new LLParser

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
    """|[5:3]: error: expected expression, got '='
       |    = 1;
       |    ^
       |""".stripMargin
  )

  checkFail[Stmt](
    "return = 1;",
    """|[1:1]: error: expected expression, got 'return'
       |  return = 1;
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
    """|[2:12]: error: expected expression, got ';'
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
    """|[3:3]: error: expected identifier, got 'return'
       |    return 1;
       |    ^
       |""".stripMargin
  )

}
