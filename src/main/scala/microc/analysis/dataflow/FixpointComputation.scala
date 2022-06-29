package microc.analysis.dataflow

import microc.cfg.Cfg

trait FixpointComputation { self: DataFlowAnalysis =>
  def fixpoint(program: Cfg.Interprocedural, initialState: ProgramState): ProgramState
  def fixpoint(): ProgramState
}

object FixpointComputation {
  trait Naive extends FixpointComputation { self: DataFlowAnalysis =>
    private implicit def pl: Lattice[ProgramState] = programLat
    import Lattice.{LatOps, ⊥}

    def fixpoint(program: Cfg.Interprocedural, initialState: ProgramState = programLat.bot): ProgramState = {
      import Cfg.CfgOps

      def join(node: Cfg.CfgNode, state: FunctionState)(implicit cfg: Cfg.Cfg, nl: Lattice[AbstractEnv]): AbstractEnv =
        node.pred.foldLeft(⊥[AbstractEnv])(_ ⊔ state(_))

      def step(prevProgState: ProgramState): ProgramState = program.fns.foldLeft(prevProgState) {
        case (progState, (fn, cfg)) =>
          progState.updated(fn, cfg.nodes.foldLeft(progState(fn)) { (state, node) =>
            implicit val c: Cfg.Cfg = cfg
            implicit val nl: Lattice[AbstractEnv] = nodeLat(node)
            state.updated(node, state(node) ⊔ transfer(node, join(node, state)))
          })
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
