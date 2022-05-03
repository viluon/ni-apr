package microc.analysis.dataflow

import microc.Parsing
import microc.analysis.SemanticAnalysis
import microc.ast.{AssignStmt, AstNormalizer, BinaryOp, DirectWrite, Divide, Equal, Expr, GreaterThan, Identifier, Minus, Plus, Times, VarStmt}
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode
import munit.FunSuite

class DataFlowAnalysisTest extends FunSuite with Parsing {
  test("CFG generation should correspond to the official example") {
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
    val semAnl = new SemanticAnalysis()
    val (decls, _) = semAnl.analyze(ast)
    val cfg = Cfg.convert(ast)

    case object ConstantAnalysis extends DataFlowAnalysis with FixpointComputation.Naive {
      override type VariableState = Lattice.FlatLat[Int]
      override val vLat: Lattice[VariableState] = Lattice.flatLat[Int]
      override val nodeLat: Lattice[NodeState] = Lattice.mapLat(decls.values, vLat)
      override val programLat: Lattice[ProgramState] = Lattice.mapLat(cfg.nodes, nodeLat)

      override val dir: DataFlowAnalysis.Direction = DataFlowAnalysis.Direction.Forward
      override val mayMust: DataFlowAnalysis.MayMust = DataFlowAnalysis.MayMust.Must

      private implicit def vl: Lattice[VariableState] = vLat
      def eval(state: NodeState, expr: Expr): VariableState = expr match {
        case microc.ast.Number(k, _) => Lattice.FlatLat.Mid(k)
        case id: Identifier => state(decls(id))
        case BinaryOp(op, left: Identifier, right: Identifier, _) => state(decls(left)) -> state(decls(right)) match {
          case (Lattice.FlatLat.Mid(x), Lattice.FlatLat.Mid(y)) => Lattice.FlatLat.Mid(op match {
            case Plus => x + y
            case Minus => x - y
            case Times => x * y
            case Divide => x / y
            case Equal => if (x == y) 1 else 0
            case GreaterThan => if (x > y) 1 else 0
          })
          case (a, b) => a ⊔ b
        }
        case _ => vLat.top
      }

      override def transfer(node: CfgNode, state: NodeState): NodeState = node match {
        case Right(VarStmt(ids, _)) => ids.foldLeft(state)((acc, id) => acc.updated(id, vLat.bot))
        case Right(AssignStmt(DirectWrite(id, _), rhs, _)) => state.updated(decls(id), eval(state, rhs))
        case _ => state
      }
    }

    val constants = ConstantAnalysis.fixpoint(cfg)
    println(cfg.toDot(constants.view.mapValues("\\n" + _.map(p => p._1.name + ": " + p._2).mkString("{", ",", "}")).toMap))
  }
}
