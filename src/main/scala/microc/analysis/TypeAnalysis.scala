package microc.analysis

import microc.ast
import microc.ast.{Alloc, AssignStmt, AstNode, BinaryOp, CallFuncExpr, Decl, Deref, Equal, Expr, FieldAccess, Identifier, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, StmtInNestedBlock, VarRef, WhileStmt}
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

  def unify(a: Type, b: Type): Analysis[Unit] = s => {
    val uf = s.unionFind.makeSet(a).makeSet(b)
    val nextState = s.copy(unionFind = uf)
    val (ta, tb) = (uf.forest(a).find(), uf.forest(b).find())
    if (ta == tb) (Nil, nextState, ())
    else {
      def union(a: Type, b: Type) = {
        uf.union(a, b)
        (Nil, nextState, ())
      }

      (ta.x, tb.x) match {
        case (v: Type.Var, t) => union(v, t)
        case (t, v: Type.Var) => union(v, t)
        case (t1, t2) if t1.getClass == t2.getClass =>
          uf.union(t1, t2)
          val paramPairs = (t1.productIterator zip t2.productIterator).asInstanceOf[Iterator[(Type, Type)]]
          WriterState.foldLeft(paramPairs.toVector)(()) {
            case (_, (p1, p2)) => unify(p1, p2)
          }(implicitly)(nextState)
        case (t1, t2) => (List(s"cannot unify $t1 with $t2"), nextState, ())
      }
    }
  }

  def constrain(expr: Expr, f: Type => Type): Analysis[Type] = for (
    _expr_ <- fresh(expr);
    () <- unify(_expr_, f(_expr_)) // TODO smells like a fixpoint
  ) yield _expr_

  def go(expr: Expr): Analysis[Type] = fresh(expr).flatMap(_expr_ => ((expr match {
    case nil: ast.Null => ???
    case _: ast.Number => unify(_expr_, Type.Int)
    case _: Identifier => pure(())
    case BinaryOp(Equal, left, right, _) => for (
      () <- unify(_expr_, Type.Int);
      lt <- go(left);
      rt <- go(right);
      () <- unify(lt, rt)
    ) yield ()
    case BinaryOp(_, left, right, _) => for (
      lt <- go(left);
      rt <- go(right);
      () <- unify(lt, rt);
      () <- unify(rt, _expr_);
      () <- unify(_expr_, Type.Int)
    ) yield ()
    case CallFuncExpr(targetFun, args, _) => for (
      _fn_ <- go(targetFun);
      argTypes <- WriterState.foldLeft(args)(List[Type]())((acc, arg) => go(arg).map(_ :: acc));
      () <- unify(_fn_, Type.Function(argTypes, _expr_))
    ) yield ()
    case _: Input => unify(_expr_, Type.Int)
    case Alloc(obj, _) => go(obj).flatMap(_obj_ => unify(_expr_, Type.Pointer(_obj_)))
    case VarRef(id, _) => go(id).flatMap(_id_ => unify(_expr_, Type.Pointer(_id_)))
    case Deref(pointer, _) => go(pointer).flatMap(_pointer_ => unify(Type.Pointer(_expr_), _pointer_))
    case ast.Record(fields, span) => ???
    case FieldAccess(record, field, span) => ???
  }): Analysis[Unit]).map(_ => _expr_))

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
    case WhileStmt(guard, block, _) => for (
      _guard_ <- go(guard);
      () <- unify(_guard_, Type.Int);
      () <- go(block)
    ) yield ()
    case OutputStmt(expr, _) => for (
      _expr_ <- go(expr);
      () <- unify(_expr_, Type.Int)
    ) yield ()
  }

  def analyze(program: Program): (List[String], Map[Decl, Type]) = {
    val (log, TAState(typeVars, constraints), ()) = WriterState.foldLeft(program.funs)(())((_, fn) => for (
      _fn_ <- fresh(fn);
      paramTypes <- WriterState.foldLeft(fn.params)(List[Type]())((acc, param) => fresh(param).map(_ :: acc));
      _ret_ <- fresh(fn.block.ret);
      () <- unify(_fn_, Type.Function(paramTypes, _ret_));
      () <- WriterState.foldLeft(fn.block.stmts)(())((_, stmt) => go(stmt))
    ) yield ())(implicitly)(TAState(Map(), UnionFind(Map())))

    def resolve(x: Type): Type = x match {
      case Type.Pointer(t) => Type.Pointer(resolve(t))
      case Type.Function(params, ret) => Type.Function(params.map((x: Type) => resolve(x)), resolve(ret))
      case v: Type.Var => constraints.forest(v).find().x
      case Type.Int => x
    }

    log -> declarations.values.map(decl => {
      decl -> resolve(constraints.forest(Type.Var(typeVars(decl))).find().x)
    }).toMap
  }
}
