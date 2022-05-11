package microc.cfg

import microc.ast.{AssignStmt, AstNode, Block, Decl, FunDecl, IfStmt, NestedBlockStmt, OutputStmt, Program, ReturnStmt, Stmt, StmtInNestedBlock, VarStmt, WhileStmt}

import scala.annotation.tailrec
import scala.collection.mutable

object Cfg {
  def swapSourceSink(n: CfgNode): CfgNode = n match {
    case Left(Sink) => Left(Source)
    case Left(Source) => Left(Sink)
    case x => x
  }

  implicit class CfgOps(val node: CfgNode)(implicit cfg: Cfg) {
    def pred: Set[CfgNode] = cfg.inverted.graph.withDefaultValue(Set())(swapSourceSink(node)).map(swapSourceSink)
    def succ: Set[CfgNode] = cfg.graph.withDefaultValue(Set())(node)
  }

  sealed trait CfgSpecialNode
  case object Source extends CfgSpecialNode
  case object Sink extends CfgSpecialNode
  type CfgNode = Either[CfgSpecialNode, AstNode]

  case class Cfg(graph: Map[CfgNode, Set[CfgNode]], params: List[Decl]) {
    if (graph.contains(Left(Sink))) throw new IllegalStateException("the sink isn't a sink!")
    if (graph.values.exists(_.contains(Left(Source)))) throw new IllegalStateException("the source isn't a source!")

    var invertedCache: Option[Cfg] = None
    lazy val nodes: Set[CfgNode] = invertedCache.map(_.nodes).getOrElse(graph.keySet ++ graph.values.reduce(_ ++ _))
    // TODO scalacheck that cfg.inverted.inverted == cfg
    lazy val inverted: Cfg = {
      val inv = invertedCache.getOrElse(
        copy(graph =
          (for ((k, vs) <- graph.toSeq; v <- vs) yield (swapSourceSink(v), swapSourceSink(k)))
          .groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
        )
      )
      invertedCache = Some(inv)
      inv.invertedCache = Some(this)
      inv
    }

    def compose(other: Cfg): Cfg = compose(List(other))

    def compose(others: List[Cfg]): Cfg = {
      val keptHere = graph.toSeq.map(p => (p._1, p._2.filterNot(_ == Left(Sink))))
      val keptThere = others.flatMap(_.graph.toSeq.filterNot(_._1 == Left(Source)))
      val bridged = for (other <- others; (k, vs) <- graph.toSeq; if vs contains Left(Sink))
        yield (k, other.graph(Left(Source)))

      Cfg((keptHere ++ keptThere ++ bridged).foldLeft(Map[CfgNode, Set[CfgNode]]()) {
        case (acc, (k, vs)) => acc.updatedWith(k) {
          case Some(set) => Some(vs ++ set)
          case None => Some(vs)
        }
      }, params ++ others.flatMap(_.params)) // TODO correct?
    }

    def redirect(to: CfgNode): Cfg = copy(graph =
      graph.toSeq.map(p => (p._1, p._2.map {
        case Left(Sink) => to
        case x => x
      })).toMap
    )

    def add(from: CfgNode, to: CfgNode): Cfg = copy(graph =
      graph.updatedWith(from) {
        case Some(set) => Some(set.incl(to))
        case None => Some(Set(to))
      }
    )

    def toDot: String = toDot(Map())

    def toDot(extras: Map[CfgNode, String]): String = {
      val sb = new mutable.StringBuilder
      val numbering = mutable.Map[CfgNode, Int]()
      def name(node: CfgNode): String = "n_" + numbering.getOrElseUpdate(node, numbering.size)

      sb.append("digraph CFG {\n")
      for (node <- (graph.values.flatten ++ graph.keys).toSet[CfgNode]) {
        val label = (node match {
          case Left(Source) => "source"
          case Left(Sink) => "sink"
          case Right(node) => node.toString
        }) + " " + extras.getOrElse(node, "")
        sb.append(s"${name(node)}[label=\"$label\"]\n")
        for (s <- graph.withDefaultValue(Set())(node)) sb.append(name(node) + "->" + name(s) + "\n")
        sb.append('\n')
      }
      sb.append("\n}\n")
      sb.toString()
    }
  }

  object Cfg {
    val empty: Cfg = Cfg(Map(Left(Source) -> Set(Left(Sink))), Nil)
    def singleton(node: AstNode): Cfg = Cfg(Map(Left(Source) -> Set(Right(node)), Right(node) -> Set(Left(Sink))), Nil)
  }

  def convert(program: Program): Cfg = program.funs.foldLeft(Cfg.empty) {
    case (acc, FunDecl(_, params, block, _)) =>
      val cfg = acc.compose(convert(block))
      cfg.copy(params = params ++ cfg.params)
  }

  def convert(stmts: List[Stmt]): Cfg = stmts.foldLeft(Cfg.empty)((acc, stmt) => acc.compose(convert(stmt)))

  @tailrec
  def first(stmt: StmtInNestedBlock): StmtInNestedBlock = stmt match {
    case NestedBlockStmt(body, _) => first(body.head)
    case _ => stmt
  }

  @tailrec
  def last(stmt: StmtInNestedBlock): StmtInNestedBlock = stmt match {
    case NestedBlockStmt(body, _) => last(body.last)
    case _ => stmt
  }

  def convert(stmt: Stmt): Cfg = stmt match {
    case block: Block => convert(block.body)
    case WhileStmt(guard, block, _) =>
      Cfg.singleton(guard)
        .compose(convert(block).redirect(Right(guard)))
        .add(Right(last(block)), Right(guard))
        .add(Right(guard), Left(Sink))
    case IfStmt(guard, thenBranch, elseBranch, _) =>
      Cfg.singleton(guard)
        .compose((thenBranch :: elseBranch.toList map convert) :+ Cfg.empty take 2)
    case _: ReturnStmt | _: VarStmt | _: AssignStmt | _: OutputStmt => Cfg.singleton(stmt)
  }
}
