package microc.analysis.dataflow

import microc.cfg.Cfg

trait FixpointComputation { self: DataFlowAnalysis =>
  def fixpoint(program: Cfg.Interprocedural, initialState: ProgramState): ProgramState
}

object FixpointComputation {
  trait Naive extends FixpointComputation { self: DataFlowAnalysis =>
    private implicit def pl: Lattice[ProgramState] = programLat
    private implicit def fl: Lattice[FunctionState] = functionLat
    private implicit def nl: Lattice[AbstractEnv] = nodeLat
    import Lattice.LatOps

    def fixpoint(program: Cfg.Interprocedural, initialState: ProgramState = programLat.bot): ProgramState = {
      import Cfg.CfgOps

      def join(node: Cfg.CfgNode, state: FunctionState)(implicit cfg: Cfg.Cfg): AbstractEnv = node.pred.foldLeft(nodeLat.bot)(
        (acc, node) => acc ⊔ state(node)
      )

      def step(prevProgState: ProgramState): ProgramState = program.fns.foldLeft(prevProgState) {
        case (progState, (fn, cfg)) =>
          progState.updated(fn, cfg.nodes.foldLeft(progState(fn))(
            (state, node) => state.updated(node, state(node) ⊔ transfer(node, join(node, state)(cfg)))
          ))
      }

      var curr = initialState
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
