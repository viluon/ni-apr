package microc.analysis

import microc.ast
import microc.ast.{Alloc, AssignStmt, AstNode, BinaryOp, Block, CallFuncExpr, Decl, Deref, Equal, Expr, FieldAccess, FunBlockStmt, Identifier, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, ReturnStmt, Stmt, StmtInNestedBlock, VarRef, VarStmt, WhileStmt}
import microc.util.WriterState.pure
import microc.util.{Monoid, WriterState}

case class TypeAnalysis(declarations: Declarations) {
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
      val node = UFNode(x, null)
      node.parent = node
      UnionFind(forest + (x -> node))
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

  case class TAState(typeVars: Map[AstNode, Int], unionFind: UnionFind[Type])
  type Analysis[A] = WriterState[List[String], TAState, A]

  def fresh(expr: AstNode): Analysis[Type] = expr match {
    case id: Identifier => fresh(declarations(id))
    case _ => s =>
      s.typeVars.get(expr) match {
        case Some(n) => (Nil, s, Type.Var(n))
        case None =>
          val n = s.typeVars.size
          val t = Type.Var(n)
          (Nil, s.copy(
            typeVars = s.typeVars + (expr -> n),
            unionFind = s.unionFind.makeSet(t)
          ), t)
      }
  }

  def updateConstraints(f: UnionFind[Type] => UnionFind[Type]): Analysis[Unit] = s => (Nil, s.copy(unionFind = f(s.unionFind)), ())
  def makeSet(x: Type): Analysis[Type] = for (() <- updateConstraints(_.makeSet(x))) yield x
  // TODO actual unification
  def unify(a: Type, b: Type): Analysis[Unit] = for (() <- updateConstraints(uf => {
    val uf2 = uf.makeSet(a).makeSet(b)
    uf2.union(a, b)
    uf2
  })) yield ()

  def constrain(expr: Expr, f: Type => Type): Analysis[Type] = for (
    _expr_ <- fresh(expr);
    () <- unify(_expr_, f(_expr_)) // TODO smells like a fixpoint
  ) yield _expr_

  def go(expr: Expr): Analysis[Type] = expr match {
    case nil: ast.Null => ???
    case num: ast.Number => constrain(num, _ => Type.Int)
    case id: Identifier => fresh(id)
    case BinaryOp(Equal, left, right, _) => for (
      _expr_ <- fresh(expr);
      () <- unify(_expr_, Type.Int);
      lt <- go(left);
      rt <- go(right);
      () <- unify(lt, rt)
    ) yield Type.Int
    case BinaryOp(_, left, right, _) => for (
      _expr_ <- fresh(expr);
      lt <- go(left);
      rt <- go(right);
      () <- unify(lt, rt);
      () <- unify(rt, _expr_);
      () <- unify(_expr_, Type.Int)
    ) yield Type.Int
    case CallFuncExpr(targetFun, args, _) => for (
      _expr_ <- fresh(expr);
      _fn_ <- go(targetFun);
      argTypes <- WriterState.foldLeft(args)(List[Type]())((acc, arg) => go(arg).map(_ :: acc));
      () <- unify(_fn_, Type.Function(argTypes, _expr_))
    ) yield _expr_
    case Input(_) => constrain(expr, _ => Type.Int)
    case Alloc(expr, _) => constrain(expr, Type.Pointer) // TODO correct?
    case VarRef(id, _) => for (
      _expr_ <- fresh(expr);
      _id_ <- go(id);
      () <- unify(_expr_, Type.Pointer(_id_))
    ) yield _expr_
    case Deref(pointer, _) => for (
      _deref_ <- fresh(expr);
      ptr <- go(pointer);
      () <- unify(Type.Pointer(_deref_), ptr)
    ) yield _deref_
    case ast.Record(fields, span) => ???
    case FieldAccess(record, field, span) => ???
  }

  def go(nestedStmt: StmtInNestedBlock): Analysis[Unit] = nestedStmt match {
    case AssignStmt(lhs, rhs, _) => for (
      lt <- go(lhs);
      rt <- go(rhs);
      () <- unify(lt, rt)
    ) yield ()
    case NestedBlockStmt(body, _) => WriterState.foldLeft(body)(())((_, stmt) => go(stmt))
    case IfStmt(guard, thenBranch, elseBranch, _) => for (
      _guard_ <- go(guard);
      () <- unify(_guard_, Type.Int);
      () <- go(thenBranch);
      () <- elseBranch.map(go).getOrElse[Analysis[Unit]](pure(()))
    ) yield ()
    case WhileStmt(guard, block, span) => ???
    case OutputStmt(expr, _) => for (
      _expr_ <- go(expr);
      () <- unify(_expr_, Type.Int)
    ) yield ()
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
    val (log, TAState(typeVars, constraints), ()) = WriterState.foldLeft(program.funs)(())((_, fn) => for (
      _fn_ <- fresh(fn);
      paramTypes <- WriterState.foldLeft(fn.params)(List[Type]())((acc, param) => fresh(param).map(_ :: acc));
      _ret_ <- fresh(fn.block.ret);
      () <- unify(_fn_, Type.Function(paramTypes, _ret_));
      () <- WriterState.foldLeft(fn.block.stmts)(())((_, stmt) => go(stmt))
    ) yield ())(implicitly)(TAState(Map(), UnionFind(Map())))

    declarations.values.map(decl => {
      decl -> constraints.forest(Type.Var(typeVars(decl))).find().x
    }).toMap
  }
}
