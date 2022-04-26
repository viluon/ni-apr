package microc.cfg

import microc.Parsing
import munit.FunSuite

class CfgTest extends FunSuite with Parsing {
  test("CFG generation should correspond to official example") {
    val ast = parseUnsafe(
      """ite(n) {
        |  var f;
        |  f = 1;
        |  while (n > 0) {
        |    f = f * n;
        |    n = n - 1;
        |  }
        |  return f;
        |}
        |""".stripMargin)
    println(Cfg.convert(ast).toDot)
  }
}
