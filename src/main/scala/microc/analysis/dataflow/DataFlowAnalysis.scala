package microc.analysis.dataflow

import microc.analysis.Declarations
import microc.ast.Decl
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode

trait DataFlowAnalysis {
  type AbstractValue
  type AbstractEnv = Map[Decl, AbstractValue]
  type ProgramState = Map[CfgNode, AbstractEnv]

  def vLat: Lattice[AbstractValue]
  def nodeLat: Lattice[AbstractEnv]
  def programLat: Lattice[ProgramState]
  def forward: Boolean
  def must: Boolean
  def transfer(node: CfgNode, state: AbstractEnv): AbstractEnv
}

object DataFlowAnalysis {
  abstract class Builder[E](lat: Lattice[E],
                            _forward: Boolean = true,
                            _must: Boolean = true
                           )(decls: Declarations,
                             cfg: Cfg.Cfg
                           )
    extends DataFlowAnalysis with FixpointComputation {

    implicit val vLat: Lattice[AbstractValue] = if (must) lat else Lattice.invLat(lat)

    override type AbstractValue = E
    override implicit lazy val nodeLat: Lattice[AbstractEnv] = Lattice.mapLat(decls.values, vLat)
    override implicit lazy val programLat: Lattice[ProgramState] = Lattice.mapLat(cfg.nodes, nodeLat)
    override def forward: Boolean = _forward
    override def must: Boolean = _must

    def fixpoint(): ProgramState = fixpoint(if (forward) cfg else cfg.inverted, {
      val initState = nodeLat.bot ++ (for (param <- cfg.params) yield param -> vLat.top)
      programLat.bot.updated(Left(Cfg.Source), initState)
    })
  }
}
