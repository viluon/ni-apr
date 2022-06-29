package microc.analysis.dataflow

import microc.Parsing
import microc.analysis.SemanticAnalysis
import microc.ast.AstNormalizer
import microc.cfg.Cfg
import munit.FunSuite

class DataFlowAnalysisTest extends FunSuite with Parsing {
  test("Constant analysis should produce meaningful results") {
    val ast = AstNormalizer.normalize(parseUnsafe(
      """ite(n) {
        |  var f;
        |  f = 1;
        |  while (n > 0) {
        |    f = f * n;
        |    n = n - 1;
        |  }
        |  return f;
        |}
        |""".stripMargin))
    val (decls, _) = new SemanticAnalysis().analyze(ast)
    val cfg = Cfg.convert(ast)

    val constants = new ConstantAnalysis(decls, cfg).fixpoint()

    println(cfg.toDot(
      constants.values.flatMap(fn => fn.view.mapValues(
        "\\n" + _.map(p => p._1.name + ": " + p._2).mkString("{", ",", "}")
      )).toMap,
      decls
    ))
  }

  test("Constant analysis should preserve scoping") {
    val ast = AstNormalizer.normalize(parseUnsafe(
      """foo(x) {
        |  return x;
        |}
        |bar(x) {
        |  return x;
        |}""".stripMargin
    ))
    val (decls, _) = new SemanticAnalysis().analyze(ast)
    val cfg = Cfg.convert(ast)
    val constants = new ConstantAnalysis(decls, cfg).fixpoint()
    for {
      (_, fnState) <- constants
      (_, env) <- fnState
    } env.map(_._1.name).toList.sorted.sliding(2).collectFirst {
      case List(a, b) if a == b => a
    }.foreach(
      a => throw new IllegalStateException(s"$a is defined more than once in\n" + env.mkString("\n"))
    )
  }

  test("Map lattices should have a correct least upper bound") {
    val variables = List("a", "b", "c")
    val vLat = Lattice.flatLat
    val nodeLat = Lattice.mapLat(variables, (_: String) => vLat)
    println(
      variables.foldLeft(nodeLat.bot)((acc, v) => acc.updated(v, vLat.bot))
    )
  }
}
