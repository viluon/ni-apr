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

  test("Dereferences should be flattened") {
    println(AstNormalizer.normalize(parseUnsafe(
      """foo(f, g, h) {
        |  var x;
        |  x = (**f)(g() + h());
        |  return x;
        |}
        |""".stripMargin)))
  }

  test("Indirect writes should be flattened") {
    println(AstNormalizer.normalize(parseUnsafe(
      """foo(x, y) {
        |  **x = **y;
        |  return **x;
        |}
        |""".stripMargin)))
  }

  test("Outputs and allocations should be flattened") {
    println(AstNormalizer.normalize(parseUnsafe(
      """f(x, g) {
        |  var y, z;
        |  y = alloc x + 2;
        |  output y + 2;
        |  z = g(x + 2, x - 5) + 4;
        |  return z + x;
        |}
        |""".stripMargin)))
  }

  test("While loops should be flattened") {
    println(AstNormalizer.normalize(parseUnsafe(
      """fac(n) {
        |  var f;
        |  f = 1;
        |
        |  while (n>0) {
        |    f = f * n;
        |    n = n - 1;
        |  }
        |
        |  return f;
        |}
        |""".stripMargin)))
  }

  test("Record constructors should be flattened") {
    println(AstNormalizer.normalize(parseUnsafe(
      """f(g) {
        |  var x, y, z;
        |  x = {a: g(2 * 2) + 3, b: 2 * 3 * 4 };
        |  y = {c: &x};
        |  z = &y;
        |  (*(*z).c).b = 2 + 3;
        |  return 0;
        |}
        |""".stripMargin)))
  }
}
