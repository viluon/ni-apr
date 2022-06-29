package microc.ast

import microc.interpreter.Interpreter
import microc.{Examples, Parsing}
import munit.FunSuite

import java.io.{StringReader, StringWriter}

class AstNormalizerTest extends FunSuite with Parsing {
  /**
    * Verify that the result of the execution of a normalized program is equal to
    * the result of the execution of its original.
    *
    * @param code The program to parse, normalize, and run.
    * @param args The arguments to the main function.
    */
  def verify(code: String, args: List[Int] = Nil): String => Unit = {
    val orig = parseUnsafe(code)
    val normalized = AstNormalizer.normalize(orig)
    println(AstNormalizer.compare(orig, normalized))

    { input =>
      val origStdout = new StringWriter()
      val origExitCode = Interpreter(orig, new StringReader(input), origStdout).run(args)
      val normalizedStdout = new StringWriter()
      val normalizedExitCode = Interpreter(normalized, new StringReader(input), normalizedStdout).run(args)

      assertEquals(normalizedExitCode, origExitCode)
      assertEquals(normalizedStdout.toString, origStdout.toString)
    }
  }

  val examples: List[(List[Int], Iterable[String], String)] = List(
    (Nil, (0 to 5).map(_.toString),
      """fac(n) {
        |    var f;
        |
        |    if (n == 0) {
        |      f = 1;
        |    } else {
        |      f = n * fac(n - 1);
        |    }
        |
        |    return f;
        |}
        |
        |main() {
        |    var n;
        |
        |    n = input;
        |    output fac(n);
        |
        |    return 0;
        |}
        |""".stripMargin),
    (Nil, Nil, Examples.ExampleMapInc)
  )

  test("Normalized programs should be operationally equal to their originals") {
    for {
      (args, inputs, code) <- examples
      test = verify(code, args)
      _ = println("-" * 80)
      input <- inputs
    } test(input)
  }

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
