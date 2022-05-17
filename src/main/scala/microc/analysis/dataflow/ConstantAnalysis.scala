package microc.analysis.dataflow

import microc.analysis.Declarations
import microc.analysis.dataflow.Lattice.⊤
import microc.ast
import microc.ast.{AssignStmt, BinaryOp, DirectWrite, Expr, Identifier, VarStmt}
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode

class ConstantAnalysis(decls: Declarations, cfg: Cfg.Interprocedural)
  extends DataFlowAnalysis.Builder(Lattice.flatLat[Int])(decls, cfg)
    with FixpointComputation.Naive {

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
        case (Lattice.FlatLat.Mid(x), Lattice.FlatLat.Mid(y)) => op.eval(x, y) match {
          case Some(r) => Lattice.FlatLat.Mid(r)
          case None => ⊤[AbstractValue] // TODO shouldn't this be bottom?
        }
        case (a, b) => a ⊔ b
      }
    case _ => ⊤[AbstractValue]
  }

  def transfer(node: CfgNode, env: AbstractEnv): AbstractEnv = node match {
    case Right(VarStmt(ids, _)) => ids.foldLeft(env)((acc, id) => acc.updated(id, vLat.bot))
    case Right(AssignStmt(DirectWrite(id, _), rhs, _)) =>
      env.updated(decls(id), env(decls(id)) ⊔ eval(env, rhs))
    case _ => env
  }
}
