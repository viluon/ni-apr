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
  def eval(env: AbstractEnv, expr: Expr): AbstractValue = {
    val ⊤ = Lattice.⊤[AbstractValue]
    expr match {
      case _: ast.Null => ⊤
      case ast.Number(n, _) => FlatLat.Mid(signOf(n))
      case id: Identifier => env(decls(id)._2)
      case BinaryOp(operator, left, right, _) =>
        def resolve(expr: Expr): AbstractValue = expr match {
          case id: Identifier => env(decls(id)._2)
          case ast.Number(k, _) => FlatLat.Mid(signOf(k))
          case _ => throw new IllegalStateException()
        }
        SignAnalysis.opTable(operator)((resolve(left), resolve(right)))
      case _: CallFuncExpr => ⊤
      case _: Input => ⊤
      case _: Alloc => ⊤
      case _: VarRef => ⊤
      case _: Deref => ⊤
      case _: ast.Record => ⊤
      case _: FieldAccess => ⊤
    }
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

  val intLat: Lattice[FlatLat[Int]] = Lattice.flatLat

  type SignLat = FlatLat[Sign]

  lazy val opTable: Map[BinaryOperator, Map[(SignLat, SignLat), SignLat]] = {
    val tbl = for {
      op <- BinaryOperator.all.toList
    } yield op -> tableForOp(op)
    tbl.toMap
  }

  private def tableForOp(op: BinaryOperator): Map[(SignLat, SignLat), SignLat] = {
    implicit val sl: Lattice[SignLat] = signLat
    // sample the operator semantics over -2 to 2 in both dimensions
    val range = (-2 to 2).toList.map(x => FlatLat.Mid(x))
    val samples = for (l <- range; r <- range) yield (l.map(signOf), r.map(signOf)) -> ((l, r) match {
      case (FlatLat.Mid(l), FlatLat.Mid(r)) =>
        op.eval(l, r) match {
          case Some(x) => FlatLat.Mid(signOf(x))
          case None => signLat.bot
        }
    })

    extendWithExtremes(range, lubOverSamples(samples))
  }

  /**
    * Create an abstract lookup table from a collection of samples by grouping them by keys and taking the least upper
    * bound of their values.
    */
  private def lubOverSamples[K, A](samples: List[(K, A)])(implicit lat: Lattice[A]): Map[K, A] = {
    samples.groupMapReduce(_._1 // group by the input signs
    )(_._2 // use the eval outputs as RHS's
    )((l, r) => lat.lub(l, r)) // and take the least upper bound of all the samples
    // the above leaves us with a map covering all signs, including top & bottom inputs
    // e.g. for -, the samples contain Pos - Pos = Pos (2 - 1 = 1) but also Pos - Pos = Neg (1 - 2 = -1),
    // therefore, (Pos, Pos) will map to Top (least upper bound of the individual outputs).
  }

  /**
    * Take a table of abstract values at integer points and extend it to the entire abstract domain.
    *
    * Tops map to the least upper bound of all existing samples for the given input, bottoms map simply to bottom.
    *
    * @param range The range of integers over which the operator was sampled
    * @param midTable The table of abstract values of the operator at concrete points (neither ⊤ nor ⊥)
    */
  private def extendWithExtremes(range: List[FlatLat.Mid[Int]], midTable: Map[(SignLat, SignLat), SignLat]) = {
    implicit val sl: Lattice[SignLat] = signLat

    def getAxisForInput(selector: ((SignLat, SignLat)) => SignLat, input: FlatLat[Int]): SignLat = {
      val set = midTable.filter(p => selector(p._1) == input.map(signOf)).values.toSet
      set.reduceOption(_ ⊔ _).getOrElse(signLat.top)
    }

    val extremes = List(intLat.top, intLat.bot)
    midTable ++ lubOverSamples(
      for (a <- range ++ extremes; b <- extremes; (l, r) <- List((a, b), (b, a)))
        yield (l.map(signOf), r.map(signOf)) -> ((l, r) match {
          case (FlatLat.Bot(), _) | (_, FlatLat.Bot()) => signLat.bot
          case (lx@FlatLat.Mid(_), FlatLat.Top()) => getAxisForInput(_._1, lx)
          case (FlatLat.Top(), ly@FlatLat.Mid(_)) => getAxisForInput(_._2, ly)
          case (x, y) => x.map(signOf) ⊔ y.map(signOf)
        })
      )
  }

  def renderTables(tbl: Map[BinaryOperator, Map[(SignLat, SignLat), SignLat]]): String = {
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
        case (None, None) => io.AnsiColor.BOLD + io.AnsiColor.GREEN + f"$op%3s" + io.AnsiColor.RESET
        case (Some(l), Some(r)) => f"${showFlatLat(table((l, r)))}%3s"
        case (ml, mr) => io.AnsiColor.RED + f"${showFlatLat(ml.orElse(mr).get)}%3s" + io.AnsiColor.RESET
      }) mkString " ")
      formatted.mkString("\n")
    }).mkString("\n\n")
  }
}
