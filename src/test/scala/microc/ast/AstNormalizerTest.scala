package microc.ast

import microc.Parsing
import munit.FunSuite

class AstNormalizerTest extends FunSuite with Parsing {
  test("Arithmetic expressions should be flattened") {
    val program = parseUnsafe(
      """foo(f, y) {
        |  var x;
        |  x = f(y+3)*5;
        |  return x;
        |}
        |""".stripMargin)
    println(AstNormalizer.normalize(program))
  }
}
