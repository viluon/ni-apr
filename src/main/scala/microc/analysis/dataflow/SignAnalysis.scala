package microc.analysis.dataflow

import microc.analysis.Declarations
import microc.analysis.dataflow.Lattice.FlatLat
import microc.analysis.dataflow.SignAnalysis.signOf
import microc.ast
import microc.ast.{Alloc, AssignStmt, BinaryOp, BinaryOperator, CallFuncExpr, Deref, DirectWrite, Expr, FieldAccess, Identifier, Input, VarRef, VarStmt}
import microc.cfg.Cfg
import microc.cfg.Cfg.CfgNode

class SignAnalysis(decls: Declarations, cfg: Cfg.Interprocedural)
  extends DataFlowAnalysis.Builder(SignAnalysis.signLat)(decls, cfg)
  with FixpointComputation.Naive {
  def eval(env: AbstractEnv, expr: Expr): AbstractValue = expr match {
    case ast.Null(_) => ???
    case ast.Number(n, _) => FlatLat.Mid(signOf(n))
    case Identifier(name, _) => ???
    case BinaryOp(operator, left, right, _) =>
      def resolve(expr: Expr): AbstractValue = expr match {
        case id: Identifier => env(decls(id)._2)
        case ast.Number(k, _) => FlatLat.Mid(signOf(k))
        case _ => throw new IllegalStateException()
      }
      SignAnalysis.opTable(operator)((resolve(left), resolve(right)))
    case CallFuncExpr(targetFun, args, _) => ???
    case Input(_) => ???
    case Alloc(expr, _) => ???
    case VarRef(id, _) => ???
    case Deref(pointer, _) => ???
    case ast.Record(fields, _) => ???
    case FieldAccess(record, field, _) => ???
  }

  override def transfer(node: CfgNode, env: AbstractEnv): AbstractEnv = node match {
    case Right(VarStmt(ids, _)) => ids.foldLeft(env)((acc, id) => acc.updated(id, vLat.bot))
    case Right(AssignStmt(DirectWrite(id, _), rhs, _)) =>
      env.updated(decls(id)._2, env(decls(id)._2) ⊔ eval(env, rhs))
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

  lazy val opTable: Map[BinaryOperator, Map[(FlatLat[Sign], FlatLat[Sign]), FlatLat[Sign]]] = {
    import microc.analysis.dataflow.Lattice.LatOps
    implicit val sl: Lattice[FlatLat[Sign]] = signLat
    val intLat: Lattice[FlatLat[Int]] = Lattice.flatLat
    def mid(x: Sign): FlatLat[Sign] = FlatLat.Mid(x)

    val tbl = for {
      op <- BinaryOperator.all.toList
    } yield op -> {
      val range = intLat.top :: intLat.bot :: (-2 to 2).toList.map(x => FlatLat.Mid(x))
      // sample the operator semantics over -2 to 2 and top & bottom in both dimensions
      val samples = for (l <- range; r <- range) yield (l.map(signOf), r.map(signOf)) -> ((l, r) match {
        case (FlatLat.Bot(), _) | (_, FlatLat.Bot()) => signLat.bot
        case (FlatLat.Mid(l), FlatLat.Mid(r)) =>
          op.eval(l, r) match {
            case Some(x) => mid(signOf(x))
            case None => signLat.bot
          }
        case (FlatLat.Top(), _) | (_, FlatLat.Top()) => intLat.lub(l, r).map(signOf) // this won't work! the lub is always ⊤
      })
      samples.groupMapReduce(_._1 // group by the input signs
      )(_._2 // use the eval outputs as RHS's
      )(_ ⊔ _) // and take the least upper bound of all the samples
      // the above leaves us with a map covering all signs, including top & bottom inputs
      // e.g. for -, the samples contain Pos - Pos = Pos (2 - 1 = 1) but also Pos - Pos = Neg (1 - 2 = -1),
      // therefore, (Pos, Pos) will map to Top (least upper bound of the individual outputs).
    }

    tbl.toMap
  }

  def renderTables(tbl: Map[BinaryOperator, Map[(FlatLat[Sign], FlatLat[Sign]), FlatLat[Sign]]]): String = {
    def showFlatLat[A](x: FlatLat[A]): String = x match {
      case FlatLat.Mid(x) => x.toString
      case other => other.toString
    }

    (for ((op, table) <- tbl) yield {
      val keys = None :: table.keys.map(_._1).toList.sortBy {
        case FlatLat.Top() => "0"
        case FlatLat.Mid(x) => s"1$x"
        case FlatLat.Bot() => "2"
      }.map(Some(_))
      val formatted = keys.map(a => keys.map(b => (a, b) match {
        case (None, None) => io.AnsiColor.BOLD + io.AnsiColor.GREEN + ("%3s" formatted op.toString) + io.AnsiColor.RESET
        case (Some(l), Some(r)) => "%3s" formatted showFlatLat(table((l, r)))
        case (ml, mr) => io.AnsiColor.RED + ("%3s" formatted showFlatLat(ml.orElse(mr).get)) + io.AnsiColor.RESET
      }) mkString " ")
      formatted.mkString("\n")
    }).mkString("\n\n")
  }
}
