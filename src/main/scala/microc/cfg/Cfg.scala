package microc.cfg

import microc.ast.{AssignStmt, AstNode, Block, FunDecl, IfStmt, NestedBlockStmt, OutputStmt, Program, ReturnStmt, Stmt, StmtInNestedBlock, VarStmt, WhileStmt}

import scala.annotation.tailrec
import scala.collection.mutable

object Cfg {
  implicit class CfgOps(val node: AstNode)(implicit cfg: Cfg) {
    def pred: Set[AstNode] = ???
    def succ: Set[AstNode] = cfg.graph(Right(node)).flatMap {
      case Left(_) => Set()
      case Right(node) => Set(node)
    }
  }

  sealed trait CfgSpecialNode
  case object Source extends CfgSpecialNode
  case object Sink extends CfgSpecialNode
  type CfgNode = Either[CfgSpecialNode, AstNode]

  case class Cfg(graph: Map[CfgNode, Set[CfgNode]]) {
    if (graph.contains(Left(Sink))) throw new IllegalStateException("the sink isn't a sink!")
    if (graph.values.exists(_.contains(Left(Source)))) throw new IllegalStateException("the source isn't a source!")

    def compose(other: Cfg): Cfg = {
      val keptHere = graph.toSeq.map(p => (p._1, p._2.filterNot(_ == Left(Sink))))
      val keptThere = other.graph.toSeq.filterNot(_._1 == Left(Source))
      val bridged = for ((k, vs) <- graph.toSeq; if vs contains Left(Sink))
        yield (k, other.graph(Left(Source)))

      Cfg((keptHere ++ keptThere ++ bridged).foldLeft(Map[CfgNode, Set[CfgNode]]()) {
        case (acc, (k, vs)) => acc.updatedWith(k) {
          case Some(set) => Some(vs ++ set)
          case None => Some(vs)
        }
      })
    }

    def redirect(to: CfgNode): Cfg =
      Cfg(graph.toSeq.map(p => (p._1, p._2.map {
        case Left(Sink) => to
        case x => x
      })).toMap)

    def add(from: CfgNode, to: CfgNode): Cfg = Cfg(
      graph.updatedWith(from) {
        case Some(set) => Some(set.incl(to))
        case None => Some(Set(to))
      }
    )

    def toDot: String = {
      val sb = new mutable.StringBuilder
      val numbering = mutable.Map[CfgNode, Int]()
      def name(node: CfgNode): String = "n_" + numbering.getOrElseUpdate(node, numbering.size)

      sb.append("digraph CFG {\n")
      for (node <- (graph.values.flatten ++ graph.keys).toSet[CfgNode]) {
        val label = node match {
          case Left(Source) => "source"
          case Left(Sink) => "sink"
          case Right(node) => node.toString
        }
        sb.append(s"${name(node)}[label=\"$label\"]\n")
        for (s <- graph.withDefaultValue(Set())(node)) sb.append(name(node) + "->" + name(s) + "\n")
        sb.append('\n')
      }
      sb.append("\n}\n")
      sb.toString()
    }
  }
  object Cfg {
    val empty: Cfg = Cfg(Map(Left(Source) -> Set(Left(Sink))))
    def singleton(node: AstNode): Cfg = Cfg(Map(Left(Source) -> Set(Right(node)), Right(node) -> Set(Left(Sink))))
  }

  def convert(program: Program): Cfg = program.funs.foldLeft(Cfg.empty) {
    case (acc, FunDecl(_, _, block, _)) => acc.compose(convert(block))
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
    case IfStmt(guard, thenBranch, elseBranch, span) => ???
    case _: ReturnStmt | _: VarStmt | _: AssignStmt | _: OutputStmt => Cfg.singleton(stmt)
  }
}
