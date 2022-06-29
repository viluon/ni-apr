package microc.cfg

import microc.Parsing
import microc.analysis.{Declarations, SemanticAnalysis, SemanticException}
import microc.ast.{AstNormalizer, Number, Program, Span}
import microc.cli.Reporter
import org.intellij.lang.annotations.Language
import org.scalatest.flatspec.AnyFlatSpec

class CfgTest extends AnyFlatSpec with Parsing {

  private def parseAndAnalyze(
                       @Language(value = "JavaScript", prefix = "function ") code: String,
                       normalize: Boolean
                     ): (Declarations, Program) = {
    val parsed = parseUnsafe(code)
    val ast = if (normalize) AstNormalizer.normalize(parsed) else parsed
    val reporter = new Reporter(ast.toString, None)
    try {
      val (decls, _) = new SemanticAnalysis().analyze(ast)
      (decls, ast)
    } catch {
      case e: SemanticException =>
        throw new IllegalStateException("error during semantic analysis:\n" + e.format(reporter))
    }
  }

  private def showDot(@Language(value = "JavaScript", prefix = "function ") code: String, normalize: Boolean = false): Unit = {
    val (decls, ast) = parseAndAnalyze(code, normalize)
    implicit val d: Declarations = decls;
    println(Cfg.convert(ast).toDot)
  }

  "CFG generation" should "correspond to the official example" in {
    showDot(
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
  }

  "CFG generation of a normalized AST" should "correspond to the official example" in {
    showDot(
      """ite(n) {
        |  var f;
        |  f = 1;
        |  while (n > 0) {
        |    f = f * n;
        |    n = n - 1;
        |  }
        |  return f;
        |}
        |""".stripMargin,
      normalize = true
    )
  }

  "CFG composition" should "work" in {
    import Cfg.Cfg
    val fn = null
    val zero = Cfg.singleton(fn, Number(0, Span.invalid))
    val one = Cfg.singleton(fn, Number(1, Span.invalid))
    val cfg = zero.compose(one)
    println(cfg)
  }

  "CFG conversion" should "support ifs" in {
    showDot(
      """main(n) {
        |  var x;
        |  x = 10;
        |  if (n > x) {
        |    n = n - 1;
        |    if (x == 5) {
        |      x = x + 1;
        |      output x;
        |    } else {
        |      x = x * x;
        |      n = 0;
        |    }
        |    n = n + 1;
        |  }
        |  return n;
        |}
        |""".stripMargin,
      normalize = true
    )
  }

  it should "support multiple functions" in {
    showDot(
      """foo(x) {
        |  return x;
        |}
        |bar(y) {
        |  return y;
        |}
        |""".stripMargin
    )
  }

  it should "support mutually recursive functions" in {
    showDot(
      """foo(x) {
        |  return bar(x);
        |}
        |bar(y) {
        |  return foo(y);
        |}
        |""".stripMargin,
      normalize = true
    )
  }
}
