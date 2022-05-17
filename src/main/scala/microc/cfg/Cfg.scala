package microc.cfg

import microc.analysis.Declarations
import microc.ast._
import microc.util.Digraph
import microc.util.StringExtensions.StringOps

import scala.annotation.tailrec
import scala.collection.mutable

object Cfg {
  def swapSourceSink(n: CfgNode): CfgNode = n match {
    case Left(Sink(fn)) => Left(Source(fn))
    case Left(Source(fn)) => Left(Sink(fn))
    case x => x
  }

  implicit class CfgOps(val node: CfgNode)(implicit cfg: Cfg) {
    def pred: Set[CfgNode] = cfg.inverted.graph.withDefaultValue(Set())(swapSourceSink(node)).map(swapSourceSink)
    def succ: Set[CfgNode] = cfg.graph.withDefaultValue(Set())(node)
  }

  sealed trait CfgSpecialNode
  case class Source(fn: FunDecl) extends CfgSpecialNode
  case class Sink(fn: FunDecl) extends CfgSpecialNode
  type CfgNode = Either[CfgSpecialNode, AstNode]

  case class Interprocedural(fns: Map[FunDecl, Cfg]) {
    lazy val inverted: Interprocedural = copy(fns = fns.map {
      case (decl, cfg) => decl -> cfg.inverted
    })

    def toDot(implicit decls: Declarations): Digraph = toDot(Map(), decls)

    def toDot(extras: Map[CfgNode, String], decls: Declarations): Digraph = {
      val numbering = mutable.Map[CfgNode, Int]()
      def name(node: CfgNode): String = "n_" + numbering.getOrElseUpdate(node, numbering.size)

      Digraph("CFG", fns.map {
        case (decl, cfg) =>
          s"""subgraph cluster_${decl.name} {
             |  label = "${decl.name}(${decl.params.map(_.name).mkString(",")})"
             |${cfg.toDot(extras, name, decls).body.indented}
             |}""".stripMargin
      }.toSet, Set())
    }
  }

  case class Cfg(fn: FunDecl, graph: Map[CfgNode, Set[CfgNode]], params: List[Decl]) {
    if (graph.keys.exists { case Left(Sink(_)) => true; case _ => false })
      throw new IllegalStateException("the sink isn't a sink!")
    if (graph.values.exists(_.exists { case Left(Source(_)) => true; case _ => false }))
      throw new IllegalStateException("the source isn't a source!")

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
      import cats.syntax.bifunctor._
      val updateFn: CfgNode => CfgNode = {
        case Left(Sink(_)) => Left(Sink(fn))
        case Left(Source(_)) => Left(Source(fn))
        case x => x
      }
      val keptHere = graph.toSeq
        .map(_.bimap(updateFn, _.map(updateFn)))
        .map(_.bimap(identity, _.filterNot { case Left(Sink(_)) => true; case _ => false }))
      val keptThere = others.flatMap(_.graph.toSeq
        .map(_.bimap(updateFn, _.map(updateFn)))
        .filterNot { case (Left(Source(_)), _) => true; case _ => false }
      )
      val bridged = for {
        other <- others
        (k, vs) <- graph.toSeq
        if vs exists { case Left(Sink(_)) => true; case _ => false }
      } yield (k, other.graph(Left(Source(other.fn))))

      Cfg(fn, (keptHere ++ keptThere ++ bridged).foldLeft(Map[CfgNode, Set[CfgNode]]()) {
        case (acc, (k, vs)) => acc.updatedWith(k) {
          case Some(set) => Some(vs ++ set)
          case None => Some(vs)
        }
      }, params ++ others.flatMap(_.params)) // TODO correct?
    }

    def redirect(to: CfgNode): Cfg = copy(graph =
      graph.toSeq.map(p => (p._1, p._2.map {
        case Left(Sink(_)) => to
        case x => x
      })).toMap
    )

    def add(from: CfgNode, to: CfgNode): Cfg = copy(graph =
      graph.updatedWith(from) {
        case Some(set) => Some(set.incl(to))
        case None => Some(Set(to))
      }
    )

    def toDot(decls: Declarations): Digraph = {
      val numbering = mutable.Map[CfgNode, Int]()
      def name(node: CfgNode): String = "n_" + numbering.getOrElseUpdate(node, numbering.size)
      toDot(Map(), name, decls)
    }

    def toDot(extras: Map[CfgNode, String], name: CfgNode => String, decls: Declarations): Digraph = {
      val nodes = mutable.Set[String]()
      val edges = mutable.Set[(String, String, String)]()

      val graphNodes = (graph.values.flatten ++ graph.keys).toSet
      for (node <- graphNodes) {
        val label = (node match {
          case Left(Source(_)) => "source"
          case Left(Sink(_)) => "sink"
          case Right(node) => node.toString
        }) + " " + extras.getOrElse(node, "")
        nodes.addOne(name(node) + s"[label=\"$label\"]")
        node match {
          case Right(AssignStmt(_, CallFuncExpr(fn: Identifier, _, _), _)) =>
            val decl = decls(fn).asInstanceOf[FunDecl]
            edges addOne (name(node) + ":ne", name(Left(Source(decl))) + ":n", "[constraint=false, color=lightslateblue]")
            edges addOne (name(Left(Sink(decl))) + ":s", name(node) + ":se", "[constraint=false, color=deeppink]")
          case _ => ()
        }
        for (target <- graph.withDefaultValue(Set())(node)) edges addOne (name(node), name(target), "")
      }

      Digraph("CFG", nodes.toSet, edges.toSet)
    }
  }

  object Cfg {
    def empty(fn: FunDecl): Cfg = Cfg(fn, Map(Left(Source(fn)) -> Set(Left(Sink(fn)))), Nil)
    def singleton(fn: FunDecl, node: AstNode): Cfg = Cfg(fn, Map(Left(Source(fn)) -> Set(Right(node)), Right(node) -> Set(Left(Sink(fn)))), Nil)
  }

  // TODO link calls
  def convert(program: Program): Interprocedural = Interprocedural(program.funs.map {
    case decl@FunDecl(_, params, block, _) =>
      val cfg = convert(decl, block)
      decl -> cfg.copy(params = params ++ cfg.params)
  }.toMap)

  def convert(fn: FunDecl, stmts: List[Stmt]): Cfg = stmts.foldLeft(Cfg.empty(fn))((acc, stmt) => acc.compose(convert(fn, stmt)))

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

  def convert(fn: FunDecl, stmt: Stmt): Cfg = stmt match {
    case block: Block => convert(fn, block.body)
    case WhileStmt(guard, block, _) =>
      Cfg.singleton(fn, guard)
        .compose(convert(fn, block).redirect(Right(guard)))
        .add(Right(last(block)), Right(guard))
        .add(Right(guard), Left(Sink(fn)))
    case IfStmt(guard, thenBranch, elseBranch, _) =>
      Cfg.singleton(fn, guard)
        .compose((thenBranch :: elseBranch.toList).map(convert(fn, _)) :+ Cfg.empty(fn) take 2)
    case _: ReturnStmt | _: VarStmt | _: AssignStmt | _: OutputStmt => Cfg.singleton(fn, stmt)
  }
}
