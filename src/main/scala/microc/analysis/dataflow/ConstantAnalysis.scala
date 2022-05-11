package microc.analysis.dataflow

import microc.analysis.Declarations
import microc.analysis.dataflow.Lattice.⊤
import microc.ast
import microc.ast.{AssignStmt, BinaryOp, DirectWrite, Expr, Identifier, VarStmt}
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode

class ConstantAnalysis(decls: Declarations, cfg: Cfg.Cfg)
  extends DataFlowAnalysis.Builder[Lattice.FlatLat[Int]]()(decls, cfg) {

  implicit val vLat: Lattice[AbstractValue] = Lattice.flatLat[Int]

  def eval(env: AbstractEnv, expr: Expr): AbstractValue = expr match {
    case ast.Number(k, _) => Lattice.FlatLat.Mid(k)
    case id: Identifier => env(decls(id))
    case BinaryOp(op, left, right, _) =>
      def resolve(expr: Expr): AbstractValue = expr match {
        case id: Identifier => env(decls(id))
        case ast.Number(k, _) => Lattice.FlatLat.Mid(k)
        case _ => throw new IllegalStateException()
      }
      (resolve(left), resolve(right)) match {
        case (Lattice.FlatLat.Mid(x), Lattice.FlatLat.Mid(y)) => Lattice.FlatLat.Mid(op.eval(x, y))
        case (a, b) => a ⊔ b
      }
    case _ => ⊤
  }

  def transfer(node: CfgNode, env: AbstractEnv): AbstractEnv = node match {
    case Right(VarStmt(ids, _)) => ids.foldLeft(env)((acc, id) => acc.updated(id, vLat.bot))
    case Right(AssignStmt(DirectWrite(id, _), rhs, _)) =>
      val r = env.updated(decls(id), eval(env, rhs))
      r ⊔ env
    case _ => env
  }
}
