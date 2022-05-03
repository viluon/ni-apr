package microc.analysis.dataflow

import microc.ast.Decl
import microc.cfg.Cfg.CfgNode

trait DataFlowAnalysis {
  type VariableState
  type NodeState = Map[Decl, VariableState]
  type ProgramState = Map[CfgNode, NodeState]

  def vLat: Lattice[VariableState]
  def nodeLat: Lattice[NodeState]
  def programLat: Lattice[ProgramState]
  def dir: DataFlowAnalysis.Direction
  def mayMust: DataFlowAnalysis.MayMust
  def transfer(node: CfgNode, state: NodeState): NodeState
}

object DataFlowAnalysis {
  sealed trait Direction
  object Direction {
    case object Forward extends Direction
    case object Backward extends Direction
  }
  sealed trait MayMust
  object MayMust {
    case object May extends MayMust
    case object Must extends MayMust
  }
}
