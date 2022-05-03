package microc.analysis.dataflow

import microc.analysis.dataflow.DataFlowAnalysis.{Direction, MayMust}
import microc.cfg.Cfg

object FixpointComputation {
  trait Naive { self: DataFlowAnalysis =>
    private implicit def pl: Lattice[ProgramState] = programLat
    private implicit def nl: Lattice[NodeState] = nodeLat
    import Lattice.LatOps

    def fixpoint(program: Cfg.Cfg): ProgramState = {
      implicit def cfg: Cfg.Cfg = program
      import Cfg.CfgOps

      def join(node: Cfg.CfgNode, state: ProgramState): NodeState = {
        // FIXME move these matches out to smart analysis constructors
        val pred = dir match {
          case Direction.Forward => node.pred
          case Direction.Backward => node.succ
        }
        pred.foldLeft(nodeLat.bot)(
          (acc, node) => mayMust match {
            case MayMust.May => acc ⊓ state(node)
            case MayMust.Must => acc ⊔ state(node)
          }
        )
      }

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
