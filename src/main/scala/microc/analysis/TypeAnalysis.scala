package microc.analysis

import microc.analysis.Type.AbsentField
import microc.ast
import microc.ast.{Alloc, AssignStmt, AstNode, BinaryOp, CallFuncExpr, Decl, Deref, Equal, Expr, FieldAccess, Identifier, IfStmt, Input, NestedBlockStmt, OutputStmt, Program, Span, StmtInNestedBlock, VarRef, WhileStmt}
import microc.util.WriterState.pure
import microc.util.{Monoid, WriterState}

case class TypeAnalysis(declarations: Declarations, fieldNames: Set[String]) {
  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def concat(l: List[A], r: List[A]): List[A] = l ++ r
    override def zero: List[A] = Nil
  }

  private val allFields = fieldNames.map(_.->(AbsentField))

  case class UFNode[A](x: A, var parent: UFNode[A]) {
    override def toString: String = s"UFNode($x, ${if (this eq parent) "loop" else parent})"

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
  case class TypeError(msg: String, span: Span)
  type Analysis[A] = WriterState[List[TypeError], TAState, A]

  def fresh(expr: AstNode): Analysis[Type] = expr match {
    case id: Identifier => fresh(declarations(id)._2)
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

  def fresh: Analysis[Type] = s => {
    val tVar = Type.Var(s.varCounter)
    (Nil, s.copy(
      varCounter = s.varCounter + 1,
      unionFind = s.unionFind.makeSet(tVar)
    ), tVar)
  }

  def unify(a: Type, b: Type, span: Span): Analysis[Unit] = s => {
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
            case (_, (p1, p2)) => unify(p1, p2, span)
          }(implicitly)(nextState)
        case (t1, t2) => (List(TypeError(s"cannot unify $a with $b (representatives $t1, $t2)", span)), nextState, ())
      }
    }
  }

  def go(expr: Expr): Analysis[Type] = fresh(expr).flatMap(_expr_ => ((expr match {
    case _: ast.Null => fresh.flatMap(_fresh_ => unify(_expr_, Type.Pointer(_fresh_), expr.span))
    case _: ast.Number => unify(_expr_, Type.Int, expr.span)
    case _: Identifier => pure(())
    case BinaryOp(op, left, right, s) => for (
      _left_ <- go(left);
      _right_ <- go(right);
      () <- unify(_left_, _right_, s);
      () <- if (op == Equal()) unify(_right_, _expr_, s) else pure(()): Analysis[Unit];
      () <- unify(_expr_, Type.Int, s)
    ) yield ()
    case CallFuncExpr(targetFun, args, s) => for (
      _fn_ <- go(targetFun);
      argTypes <- WriterState.foldLeft(args)(List[Type]())((acc, arg) => go(arg).map(_ :: acc));
      () <- unify(_fn_, Type.Function(argTypes, _expr_), s)
    ) yield ()
    case _: Input => unify(_expr_, Type.Int, expr.span)
    case Alloc(obj, s) => go(obj).flatMap(_obj_ => unify(_expr_, Type.Pointer(_obj_), s))
    case VarRef(id, s) => go(id).flatMap(_id_ => unify(_expr_, Type.Pointer(_id_), s))
    case Deref(pointer, s) => go(pointer).flatMap(_pointer_ => unify(Type.Pointer(_expr_), _pointer_, s))
    case ast.Record(fields, s) => for (
      typedFields <- WriterState.foldLeft(fields)(List[(String, Type)]())((acc, field) =>
        go(field.expr).map(field.name -> _ :: acc)
      );
      () <- unify(_expr_, Type.Record(allFields.concat(typedFields).toMap), s)
    ) yield ()
    case FieldAccess(record, field, s) => for (
      varsForFields <- WriterState.foldLeft(fieldNames)(Map[String, Type]()) {
        case (acc, name) => fresh.map(freshVar => acc + (name -> freshVar))
      };
      _record_ <- go(record);
      () <- unify(_record_, Type.Record(varsForFields + (field -> _expr_)), s)
    ) yield ()
  }): Analysis[Unit]).map(_ => _expr_))

  def go(nestedStmt: StmtInNestedBlock): Analysis[Unit] = nestedStmt match {
    case AssignStmt(lhs, rhs, s) => for (
      lt <- go(lhs);
      rt <- go(rhs);
      () <- unify(lt, rt, s)
    ) yield ()
    case NestedBlockStmt(body, _) => WriterState.foldLeft(body)(())((_, stmt) => go(stmt))
    case IfStmt(guard, thenBranch, elseBranch, s) => for (
      _guard_ <- go(guard);
      () <- unify(_guard_, Type.Int, s);
      () <- go(thenBranch);
      () <- elseBranch.map(go).getOrElse[Analysis[Unit]](pure(()))
    ) yield ()
    case WhileStmt(guard, block, s) => for (
      _guard_ <- go(guard);
      () <- unify(_guard_, Type.Int, s);
      () <- go(block)
    ) yield ()
    case OutputStmt(expr, s) => for (
      _expr_ <- go(expr);
      () <- unify(_expr_, Type.Int, s)
    ) yield ()
  }

  private def readSolution(log: List[TypeError], typeVars: Map[AstNode, Int], constraints: UnionFind[Type]): (List[TypeError], Map[Decl, Type]) = {
    // FIXME this entire thing is just a simple traversal and should be generalised (define sequence/traverse on Type)
    def resolve(x: Type, open: Set[Type]): Option[(Type, Boolean)] = x match {
      case Type.Pointer(t) => resolve(t, open).map(r => (Type.Pointer(r._1), r._2))
      case Type.Function(params, ret) => for (
        // TODO probs reverse
        (resolvedParams, flag) <- params.map(resolve(_, open)).foldLeft(Option((List[Type](), false)))(
          (acc, opt) => for ((t, flag) <- opt; (ts, flags) <- acc) yield (t :: ts, flag || flags)
        );
        (resolvedRet, retFlag) <- resolve(ret, open)
      ) yield (Type.Function(resolvedParams, resolvedRet), flag || retFlag)
      case v: Type.Var if open contains v => Some((v, true)) // return, indicating recursive type
      case v: Type.Var => constraints.forest.get(v) match {
        case Some(node) =>
          val canonical = node.find().x
          if (canonical != v)
            resolve(canonical, open + v) match {
              case Some((sln, false)) => Some((sln, false))
              case Some((typ, true)) => Some((Type.Mu(v, typ), false))
              case None => None
            }
          else Some((v, false))
        case None => Some((v, false))
      }
      case Type.Int => Some((x, false))
      case Type.AbsentField => Some((Type.AbsentField, false))
      case Type.Mu(tVar, t) => ???
      case Type.Record(fields) => fields.map(f => f._1 -> resolve(f._2, open))
        .foldLeft(Option((List[(String, Type)](), false))) {
          case (acc, (name, opt)) => for ((t, flag) <- opt; (ts, flags) <- acc) yield ((name -> t) :: ts, flag || flags)
        }.map(r => (r._1.toMap, r._2)).map(r => (Type.Record(r._1), r._2))
    }

    val declToType = typeVars.keys.toList.flatMap {
      case decl: Decl =>
        val tVar = Type.Var(typeVars(decl))
        resolve(tVar, Set()) match {
          case Some((typ, false)) => List(decl -> typ)
          case Some((t, true)) =>
            throw new IllegalStateException(s"failure during resolution of $tVar: did not close the recursive type $t")
          case None =>
            throw new IllegalStateException(s"failure during resolution of $tVar (for declaration $decl)")
        }
      case _ => Nil
    }
    log -> declToType.toMap
  }

  def analyze(program: Program): (List[TypeError], Map[Decl, Type]) = {
    val (log, TAState(_, typeVars, constraints), ()) = WriterState.foldLeft(program.funs)(())((_, fn) => for (
      _fn_ <- fresh(fn);
      paramTypes <- WriterState.foldLeft(fn.params)(List[(Type, Span)]())(
        (acc, param) => fresh(param).map(_param_ => (_param_, param.span) :: acc)
      );
      _ret_ <- fresh(fn.block.ret);
      () <- unify(_fn_, Type.Function(paramTypes.map(_._1), _ret_), fn.span);
      () <- if (fn.name == "main") {
        for (
          // main's arguments and return type are all integers
          () <- WriterState.foldLeft(paramTypes)(()) {
            case (_, (_param_, s)) => unify(_param_, Type.Int, s)
          };
          () <- unify(_ret_, Type.Int, fn.block.ret.span)
        ) yield ()
      } else pure(()): Analysis[Unit];
      () <- WriterState.foldLeft(fn.block.stmts)(())((_, stmt) => go(stmt))
    ) yield ())(implicitly)(TAState(0, Map(), UnionFind(Map())))

    readSolution(log, typeVars, constraints)
  }
}
