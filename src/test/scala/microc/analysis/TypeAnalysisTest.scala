package microc.analysis

import microc.ast.Decl
import microc.{Examples, Parsing}
import munit.FunSuite

class TypeAnalysisTest extends FunSuite with Parsing with Examples {
  test("basic typechecking") {
    val res = analyze(ExampleTypeCheckingValid)
    assertMatches(res) {
      case Right(types) => true
    }
  }

  // ------------------------------------------------------------------
  // HELPERS
  // ------------------------------------------------------------------

  def assertMatches[A](x: A)(f: PartialFunction[A, Boolean]): Unit =
    assert(if (f.isDefinedAt(x)) f(x) else false)

  def analyze(code: String): Either[List[String], Map[Decl, Type]] = {
    val analysis = new SemanticAnalysis()
    val program = parseUnsafe(code)
    val declarations = analysis.analyze(program)
    val (errs, types) = TypeAnalysis(declarations).analyze(program)
    if (errs.nonEmpty) Left(errs)
    else Right(types)
  }
}
