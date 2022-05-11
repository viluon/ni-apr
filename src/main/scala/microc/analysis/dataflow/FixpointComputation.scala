package microc.analysis.dataflow

import microc.ast.Identifier
import microc.cfg.Cfg

trait FixpointComputation { self: DataFlowAnalysis =>
  def fixpoint(program: Cfg.Cfg): ProgramState
}

object FixpointComputation {
  trait Naive extends FixpointComputation { self: DataFlowAnalysis =>
    private implicit def pl: Lattice[ProgramState] = programLat
    private implicit def nl: Lattice[AbstractEnv] = nodeLat
    import Lattice.LatOps

    def fixpoint(program: Cfg.Cfg): ProgramState = {
      implicit def cfg: Cfg.Cfg = program
      import Cfg.CfgOps

      def join(node: Cfg.CfgNode, state: ProgramState): AbstractEnv = node.pred.foldLeft(nodeLat.bot)(
        (acc, node) => acc ⊔ state(node)
      )

      def step(prevState: ProgramState): ProgramState = program.nodes.foldLeft(prevState)(
        (state, node) => state.updated(node, transfer(node, join(node, prevState)))
      )

      var curr = programLat.bot
      var last = curr

      do {
        last = curr
        curr = step(last)
        assert(last ⊑ curr)
      } while (curr != last)
      curr
    }
  }
}
