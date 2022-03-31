package microc.analysis

import microc.ast.Decl
import microc.cli.Reporter
import microc.{Examples, Parsing}
import munit.FunSuite

class TypeAnalysisTest extends FunSuite with Parsing with Examples {
  for (k <- TypeChecking.keys.toList.sorted) test(s"smoke test: $k") {
    val expectSuccess = !k.contains("invalid")
    assertMatches(analyze(k)) {
      case Left(_) => !expectSuccess
      case Right(_) => expectSuccess
    }
  }

  test("recursive types (lists)") {
    assertMatches(analyze("recursive types (list)")) {
      case Right(types) => true
    }
  }

  // TODO check the results properly, not just typecheck/error

  // ------------------------------------------------------------------
  // HELPERS
  // ------------------------------------------------------------------

  type AnalysisResult = Either[List[String], Map[Decl, Type]]

  private def unicodify(typedDecls: Map[Decl, Type]): List[String] = typedDecls.toList.map {
    case (decl, typ) => s"⟦$decl⟧ = $typ"
  }.sorted

  def assertMatches(x: AnalysisResult)(f: PartialFunction[AnalysisResult, Boolean]): Unit =
    assert(if (f.isDefinedAt(x)) f(x) else false, x match {
      case Left(errs) => errs.mkString("unexpected failure:\n", "\n", "")
      case Right(typedDecls) => unicodify(typedDecls).mkString("unexpected success:\n", "\n", "")
    })

  def analyze(sampleName: String): AnalysisResult = {
    val code = TypeChecking(sampleName)
    val analysis = new SemanticAnalysis()
    val program = parseUnsafe(code)
    val reporter = new Reporter(code, Some(s"TypeChecking(\"$sampleName\")"))
    val (declarations, fieldNames) = try {
      analysis.analyze(program)
    } catch {
      case e: SemanticException => throw new IllegalStateException(
        s"semantic error in type analysis test case \"$sampleName\":\n" + e.format(reporter)
      )
    }
    val (errs, types) = TypeAnalysis(declarations, fieldNames).analyze(program)
    println(unicodify(types).mkString("\n"))
    if (errs.nonEmpty) Left(
      reporter.formatErrors(errs.take(1))
        :: (if (errs.size > 1) List(s"${errs.size - 1} additional error(s) won't be displayed") else Nil)
    )
    else Right(types)
  }
}
