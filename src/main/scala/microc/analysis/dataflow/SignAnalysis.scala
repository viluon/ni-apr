package microc.analysis.dataflow

import microc.analysis.Declarations
import microc.analysis.dataflow.Lattice.FlatLat
import microc.ast
import microc.ast.{Alloc, AssignStmt, BinaryOp, BinaryOperator, CallFuncExpr, Deref, DirectWrite, Expr, FieldAccess, Identifier, Input, VarRef, VarStmt}
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode

class SignAnalysis(decls: Declarations, cfg: Cfg.Cfg)
  extends DataFlowAnalysis.Builder(SignAnalysis.signLat)(decls, cfg)
  with FixpointComputation.Naive {
  def eval(env: AbstractEnv, expr: Expr): AbstractValue = expr match {
    case ast.Null(span) => ???
    case ast.Number(value, span) => ???
    case Identifier(name, span) => ???
    case BinaryOp(operator, left, right, span) => ???
    case CallFuncExpr(targetFun, args, span) => ???
    case Input(span) => ???
    case Alloc(expr, span) => ???
    case VarRef(id, span) => ???
    case Deref(pointer, span) => ???
    case ast.Record(fields, span) => ???
    case FieldAccess(record, field, span) => ???
  }

  override def transfer(node: CfgNode, env: AbstractEnv): AbstractEnv = node match {
    case Right(VarStmt(ids, _)) => ids.foldLeft(env)((acc, id) => acc.updated(id, vLat.bot))
    case Right(AssignStmt(DirectWrite(id, _), rhs, _)) =>
      env.updated(decls(id), env(decls(id)) ⊔ eval(env, rhs))
    case _ => env
  }
}

object SignAnalysis {
  sealed trait Sign
  case class Positive() extends Sign {
    override def toString: String = "+"
  }
  case class Zero() extends Sign {
    override def toString: String = "0"
  }
  case class Negative() extends Sign {
    override def toString: String = "-"
  }

  val signLat: Lattice[FlatLat[Sign]] = Lattice.flatLat

  def signOf(x: Int): Sign = x match {
    case _ if x > 0 => Positive()
    case 0 => Zero()
    case _ if x < 0 => Negative()
  }

  lazy val opTable: Map[(BinaryOperator, FlatLat[Sign], FlatLat[Sign]), FlatLat[Sign]] = {
    def mid(x: Sign): FlatLat[Sign] = FlatLat.Mid(x)
    val tbl = for {
      op <- BinaryOperator.all.toList
    } yield {
      val samples = for {
        l <- (-2 to 2).toList
        r <- (-2 to 2).toList
      } yield {
        // sample the operator semantics over -2 to 2 in both dimensions
        // TODO division by zero should return ⊥
        (signOf(l), signOf(r)) -> signOf(op.eval(l, r))
      }
      samples.groupMapReduce {
        case ((lSign, rSign), _) =>
          // group by the input signs
          mid(lSign) -> mid(rSign)
      }(kv => /*use the eval outputs as RHS's*/ mid(kv._2))(
        // and take the least upper bound of all the samples
        signLat.lub
      )
      // the above leaves us with a map covering all signs but not top/bottom inputs
      // e.g. for -, the samples contain Pos - Pos = Pos (2 - 1 = 1) but also Pos - Pos = Neg (1 - 2 = -1),
      // therefore, (Pos, Pos) will map to Top (least upper bound of the individual outputs).
    }
    tbl.toMap
  }
}
