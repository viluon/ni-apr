package microc.analysis.dataflow

import microc.analysis.Declarations
import microc.ast.{AstNode, Decl, FunDecl}
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode

trait DataFlowAnalysis {
  type AbstractValue
  type AbstractEnv = Map[Decl, AbstractValue]
  type FunctionState = Map[CfgNode, AbstractEnv]
  type ProgramState = Map[FunDecl, FunctionState]

  def vLat: Lattice[AbstractValue]
  def nodeLat(node: CfgNode): Lattice[AbstractEnv]
  def functionLat(fn: FunDecl): Lattice[FunctionState]
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
                             _cfg: Cfg.Interprocedural
                           )
    extends DataFlowAnalysis with FixpointComputation {
    import scala.language.implicitConversions

    implicit val vLat: Lattice[AbstractValue] = if (must) lat else Lattice.invLat(lat)

    override type AbstractValue = E
    private lazy val cfg: Cfg.Interprocedural = if (forward) _cfg else _cfg.inverted
    private lazy val declsByNode: Map[Cfg.CfgNode, Declarations] = cfg.fns.toList.map {
      case (fn, cfg) => cfg.nodes -> decls.toList.filter {
        case (_, (Some(`fn`) | None, _)) => true
        case _ => false
      }.toMap
    }.flatMap(p => p._1.toList.map(_ -> p._2)).toMap

    override implicit def nodeLat(node: CfgNode): Lattice[AbstractEnv] = Lattice.mapLat(declsByNode(node).values.map(_._2), _ => vLat)
    override implicit def functionLat(fn: FunDecl): Lattice[FunctionState] = Lattice.mapLat(cfg.fns(fn).nodes, nodeLat)
    override implicit lazy val programLat: Lattice[ProgramState] = Lattice.mapLat(cfg.fns.keys, functionLat)
    override def forward: Boolean = _forward
    override def must: Boolean = _must

    def fixpoint(): ProgramState = {
      // FIXME addition of parameters should happen elsewhere
      import cats.syntax.bifunctor._
      def addParamsToScope(node: Left[Cfg.CfgSpecialNode, AstNode], fn: FunDecl, env: AbstractEnv) = {
        // bind each param to ⊤
        (node, env ++ cfg.fns(fn).params.map(_ -> vLat.top))
      }

      fixpoint(cfg, programLat.bot.map(_.bimap(identity, _.map {
        case (node@Left(Cfg.Sink(fn)), env) => addParamsToScope(node, fn, env)
        case (node@Left(Cfg.Source(fn)), env) => addParamsToScope(node, fn, env)
        case x => x
      })))
    }
  }
}
