package microc.analysis

import microc.ast.{AssignStmt, AstNode, Block, Decl, FunBlockStmt, IfStmt, NestedBlockStmt, OutputStmt, Program, ReturnStmt, Stmt, StmtInNestedBlock, VarStmt, WhileStmt}
import microc.util.{Monoid, WriterState}

object TypeAnalysis {
  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def concat(l: List[A], r: List[A]): List[A] = l ++ r
    override def zero: List[A] = Nil
  }

  case class UFNode[A](x: A, var parent: UFNode[A]) {
    def find(): UFNode[A] = if (parent eq this) this else {
      val p = parent.find()
      parent = p
      p
    }
  }

  case class UnionFind[A](forest: Map[A, UFNode[A]]) {
    def makeSet(x: A): UnionFind[A] = forest.get(x).map(_ => this).getOrElse({
      val eert: UFNode[A] = UFNode(x, null)
      eert.parent = eert
      UnionFind(forest + (x -> eert))
    })

    def union(x: A, y: A): Unit = {
      val (a, b) = (forest(x), forest(y))
      val ra = a.find()
      val rb = b.find()
      if (ra != rb) {
        ra.parent = rb
      }
    }
  }

  case class TAState(unionFind: UnionFind[Type])
  type Analysis[A] = WriterState[List[String], TAState, A]

  def updateConstraints(f: UnionFind[Type] => UnionFind[Type]): Analysis[Unit] = s => (Nil, s.copy(unionFind = f(s.unionFind)), ())
  def makeSet(x: Type): Analysis[Type] = for (() <- updateConstraints(_.makeSet(x))) yield x
  def unify(a: Type, b: Type): Analysis[Boolean] = for (() <- updateConstraints(uf => {
    uf.union(a, b)
    uf
  })) yield true

  sealed trait Type
  object Type {
    case class Pointer(t: Type) extends Type
    case class Function(params: List[Type], ret: Type) extends Type
    case class Var(node: AstNode) extends Type
    case object Int extends Type
  }

  def go(nestedStmt: StmtInNestedBlock): Analysis[Unit] = nestedStmt match {
    case AssignStmt(left, right, span) => ???
    case NestedBlockStmt(body, span) => ???
    case IfStmt(guard, thenBranch, elseBranch, span) => ???
    case WhileStmt(guard, block, span) => ???
    case OutputStmt(expr, span) => ???
  }

  def go(block: Block): Analysis[Unit] = block match {
    case NestedBlockStmt(body, span) => ???
    case FunBlockStmt(vars, stmts, ret, span) => ???
  }

  def go(stmt: Stmt): Analysis[Unit] = stmt match {
    case block: StmtInNestedBlock => go(block)
    case block: Block => go(block)
    case ReturnStmt(expr, span) => ???
    case VarStmt(decls, span) => ???
  }

  def analyze(program: Program): Map[Decl, Type] = {
    val constraints: Analysis[Unit] = WriterState.foldLeft(program.funs)(())((_, fn) => for (
      _fn_ <- makeSet(Type.Var(fn));
      paramsTo_ret_ <- makeSet(Type.Function(fn.params.map(Type.Var), Type.Var(fn.block.ret)));
      true <- unify(_fn_, paramsTo_ret_)
    ) yield ())

    ???
  }
}
