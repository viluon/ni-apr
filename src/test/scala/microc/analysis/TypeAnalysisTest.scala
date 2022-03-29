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

  // TODO check the results properly, not just typecheck/error

  // ------------------------------------------------------------------
  // HELPERS
  // ------------------------------------------------------------------

  type AnalysisResult = Either[List[String], Map[Decl, Type]]

  def assertMatches(x: AnalysisResult)(f: PartialFunction[AnalysisResult, Boolean]): Unit =
    assert(if (f.isDefinedAt(x)) f(x) else false, x match {
      case Left(errs) => errs.mkString("unexpected failure:\n", "\n", "")
      case Right(typedDecls) =>
        typedDecls.toList.map {
          case (decl, typ) => s"⟦$decl⟧ = $typ"
        }.sorted.mkString("unexpected success:\n", "\n", "")
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
    if (errs.nonEmpty) Left(errs)
    else Right(types)
  }
}
