package microc.cfg

import microc.Parsing
import microc.ast.{AstNormalizer, Number, Span}
import munit.FunSuite

class CfgTest extends FunSuite with Parsing {
  test("CFG generation should correspond to the official example") {
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

  test("CFG generation of a normalized AST should correspond to the official example") {
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
    println(Cfg.convert(AstNormalizer.normalize(ast)).toDot)
  }

  test("CFG composition should work") {
    import Cfg.Cfg
    val zero = Cfg.singleton(Number(0, Span.invalid))
    val one = Cfg.singleton(Number(1, Span.invalid))
    val cfg = zero.compose(one)
    println(cfg)
  }
}