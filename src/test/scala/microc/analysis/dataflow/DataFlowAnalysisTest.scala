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
      constants.view.mapValues(
        "\\n" + _.map(p => p._1.name + ": " + p._2).mkString("{", ",", "}")
      ).toMap
    ))
  }

  test("Map lattices should have a correct least upper bound") {
    val variables = List("a", "b", "c")
    val vLat = Lattice.flatLat
    val nodeLat = Lattice.mapLat(variables, vLat)
    println(
      variables.foldLeft(nodeLat.bot)((acc, v) => acc.updated(v, vLat.bot))
    )
  }
}
