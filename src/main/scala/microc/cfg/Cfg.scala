package microc.cfg

import microc.ast.{AssignStmt, AstNode, Expr, FunBlockStmt, FunDecl, IdentifierDecl, IfStmt, NestedBlockStmt, OutputStmt, Program, RecordField, ReturnStmt, Stmt, VarStmt, WhileStmt}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Cfg {
  case class Cfg(graph: Map[AstNode, (Set[AstNode], Set[AstNode])]) {
    def toDot: String = {
      val sb = new mutable.StringBuilder
      val numbering = mutable.Map[AstNode, Int]()
      def name(node: AstNode): String = "n_" + numbering.getOrElseUpdate(node, numbering.size)

      sb.append("digraph CFG {\n")
      for ((node, (_, succ)) <- graph) {
        sb.append(s"${name(node)}[label=\"${node.toString}\"]\n")
        for (s <- succ) sb.append(name(node) + "->" + name(s) + "\n")
        sb.append('\n')
      }
      sb.append("\n}\n")
      sb.toString()
    }
  }

  implicit class CfgOps(val node: AstNode)(implicit cfg: Cfg) {
    def pred: Set[AstNode] = cfg.graph(node)._1
    def succ: Set[AstNode] = cfg.graph(node)._2
  }

  def convert(program: Program): Cfg = {
    val visited = mutable.Set[AstNode]()
    val cfg = mutable.Map[AstNode, (Set[AstNode], Set[AstNode])]()
    val queue = ListBuffer.from(program.children)

    @tailrec
    def first(node: AstNode): AstNode = node match {
      case IfStmt(guard, _, _, _) => guard
      case WhileStmt(guard, _, _) => guard
      case NestedBlockStmt(stmts, _) if stmts.nonEmpty => first(stmts.head)
      case _ => node
    }

    @tailrec
    def last(node: AstNode): AstNode = node match {
      case NestedBlockStmt(stmts, _) if stmts.nonEmpty => last(stmts.last)
      case _ => first(node)
    }

    def merge(node: AstNode, pred: Set[AstNode], succ: Set[AstNode]): Unit = cfg.updateWith(node) {
      case Some((existingPred, existingSucc)) => Some((existingPred ++ pred.map(last), existingSucc ++ succ.map(first)))
      case None => Some((pred.map(last), succ.map(first)))
    }

    implicit class AstNodeOps(node: AstNode) {
      def ~>(other: AstNode): Unit = {
        merge(node, Set(), Set(other))
        merge(other, Set(node), Set())
      }
    }

    def enqueueList(stmts: List[Stmt]): Unit = {
      queue.addAll(stmts)
      for (List(a, b) <- stmts.sliding(2)) a ~> b
    }

    while (queue.nonEmpty) {
      val node = queue.remove(0)
      visited.add(node)
      val successors: Iterable[AstNode] = node match {
        case IfStmt(guard, thn, els, _) =>
          guard ~> thn
          els.foreach(guard ~> _)
          List(guard, thn) ++ els
        case WhileStmt(guard, body, _) =>
          guard ~> body
          body ~> guard
          List(guard, body)
        case NestedBlockStmt(stmts, _) =>
          enqueueList(stmts)
          stmts.headOption
        case FunBlockStmt(vars, stmts, ret, _) =>
          enqueueList(vars ++ stmts :+ ret)
          vars
        case FunDecl(_, _, block, _) => List(block)
        case _: IdentifierDecl | _: Expr | _: RecordField | _: AssignStmt
             | _: OutputStmt | _: ReturnStmt | _: VarStmt => List()
        case _: Program => throw new IllegalStateException()
      }
      queue.addAll(successors.filterNot(visited.apply).filterNot(queue.contains))
    }

    Cfg(cfg.toMap)
  }
}
