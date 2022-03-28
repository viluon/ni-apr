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

  case class TAState(varCounter: Int, typeVars: Map[AstNode, Int], unionFind: UnionFind[Type])
  type Analysis[A] = WriterState[List[String], TAState, A]

  def fresh(expr: AstNode): Analysis[Type] = expr match {
    case id: Identifier => fresh(declarations(id))
    case _ => s =>
      s.typeVars.get(expr) match {
        case Some(n) => (Nil, s, Type.Var(n))
        case None =>
          val n = s.varCounter
          val t = Type.Var(n)
          (Nil, s.copy(
            varCounter = n + 1,
            typeVars = s.typeVars + (expr -> n),
            unionFind = s.unionFind.makeSet(t)
          ), t)
      }
  }

  def fresh: Analysis[Type] = s => (Nil, s.copy(varCounter = s.varCounter + 1), Type.Var(s.varCounter))

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
        case (t1, t2) if t1 canEqual t2 =>
          uf.union(t1, t2)
          val paramPairs = (t1.productIterator zip t2.productIterator).asInstanceOf[Iterator[(Type, Type)]]
          WriterState.foldLeft(paramPairs.toVector)(()) {
            case (_, (p1, p2)) => unify(p1, p2)
          }(implicitly)(nextState)
        case (t1, t2) => (List(s"cannot unify $t1 with $t2"), nextState, ())
      }
    }
  }

  def go(expr: Expr): Analysis[Type] = fresh(expr).flatMap(_expr_ => ((expr match {
    case _: ast.Null => fresh.flatMap(_fresh_ => unify(_expr_, Type.Pointer(_fresh_)))
    case _: ast.Number => unify(_expr_, Type.Int)
    case _: Identifier => pure(())
    case BinaryOp(op, left, right, _) => for (
      lt <- go(left);
      rt <- go(right);
      () <- unify(lt, rt);
      () <- if (op == Equal) unify(rt, _expr_) else pure(()): Analysis[Unit];
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

  private def readSolution(log: List[String], typeVars: Map[AstNode, Int], constraints: UnionFind[Type]): (List[String], Map[Decl, Type]) = {
    def resolve(x: Type): Option[Type] = x match {
      case Type.Pointer(t) => resolve(t).map(Type.Pointer)
      case Type.Function(params, ret) => for (
        // TODO probs reverse
        resolvedParams <- params.map(resolve).foldLeft(Option(List[Type]()))(
          (acc, opt) => for (t <- opt; ts <- acc) yield t :: ts
        );
        resolvedRet <- resolve(ret)
      ) yield Type.Function(resolvedParams, resolvedRet)
      case v: Type.Var if constraints.forest(v).find().x != v => resolve(constraints.forest(v).find().x)
      case _: Type.Var => None
      case Type.Int => Some(x)
    }

    val logBuf = log.toBuffer
    logBuf.toList -> typeVars.keys.flatMap {
      case decl: Decl =>
        val tVar = Type.Var(typeVars(decl))
        resolve(tVar) match {
          case Some(typ) => List(decl -> typ)
          case None =>
            logBuf += s"failure during resolution of $tVar (for declaration $decl)"
            Nil
        }
      case _ => Nil
    }.toMap
  }

  def analyze(program: Program): (List[String], Map[Decl, Type]) = {
    val (log, TAState(_, typeVars, constraints), ()) = WriterState.foldLeft(program.funs)(())((_, fn) => for (
      _fn_ <- fresh(fn);
      paramTypes <- WriterState.foldLeft(fn.params)(List[Type]())((acc, param) => fresh(param).map(_ :: acc));
      _ret_ <- fresh(fn.block.ret);
      () <- unify(_fn_, Type.Function(paramTypes, _ret_));
      () <- if (fn.name == "main") {
        for (
          // main's arguments and return type are all integers
          () <- WriterState.foldLeft(paramTypes)(())((_, _param_) => unify(_param_, Type.Int));
          () <- unify(_ret_, Type.Int)
        ) yield ()
      } else pure(()): Analysis[Unit];
      () <- WriterState.foldLeft(fn.block.stmts)(())((_, stmt) => go(stmt))
    ) yield ())(implicitly)(TAState(0, Map(), UnionFind(Map())))

    readSolution(log, typeVars, constraints)
  }
}
