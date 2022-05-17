package microc.ast

import cats.data.{State, WriterT}
import cats.syntax.bifunctor._
import cats.{Monad, Traverse}
import shapeless.prism

object AstNormalizer {
  def normalize(program: Program): Program = Program(
    Traverse[List].traverse(program.funs)(normalizeFun).runA(FreshStore(0)).value, program.span
  )

  def compare(orig: Program, normalized: Program): String = {
    val after = ("normalized\n" + normalized.toString).linesIterator.toList
    val before = LazyList.from(("original\n" + orig.toString).linesIterator).padTo(after.length, "").map("%-30s".formatted(_))
    (before zip after).map(_.productIterator.mkString).mkString("\n")
  }

  private case class FreshStore(k: Int)
  private type VariableStore[A] = State[FreshStore, A]
  private type Normalizing[A] = WriterT[VariableStore, List[(
    Boolean /*true if the statement should be emitted*/,
      StmtInNestedBlock
    )], A]

  private def fresh: Normalizing[Identifier] = WriterT.liftF(for {
    store <- State.get
    _ <- State.set(store.copy(k = store.k + 1))
  } yield Identifier(s"_t${store.k}", Span.invalid))

  private def normalizeFun(decl: FunDecl): VariableStore[FunDecl] = decl match {
    case FunDecl(name, params, block, span) =>
      val normalized = for {
        body <- Traverse[List].traverse(block.stmts)(normalizeStmt)
        ret <- bind(block.ret.expr)
      } yield (body, ret)
      for {
        result <- normalized.run
        (newVars, (originalStmts, ret)) = result.leftMap(_.flatMap {
          case (_, AssignStmt(DirectWrite(Identifier(name, idSpan), _), _, _)) =>
            List(IdentifierDecl(name, idSpan))
          case _ => throw new IllegalStateException(
            "normalization wrote a statement for which variable extraction logic isn't implemented"
          )
        })
        body = FunBlockStmt(
          block.vars :+ VarStmt(newVars, Span.invalid),
          result._1.filter(_._1).map(_._2) ++ originalStmts,
          block.ret.copy(expr = ret),
          block.span
        )
      } yield FunDecl(name, params, body, span)
  }

  private def normalizeAssignment(stmt: AssignStmt): Normalizing[StmtInNestedBlock] = stmt.left match {
    case DirectWrite(_, _) | DirectFieldWrite(_, _, _) => bind(stmt.right).map(AssignStmt(stmt.left, _, stmt.span))
    case IndirectWrite(_, _) | IndirectFieldWrite(_, _, _) => for {
      lhs <- bind(stmt.left)
      rhs <- bind(stmt.right)
    } yield AssignStmt(lhs, rhs, stmt.span)
    case _ => throw new IllegalStateException()
  }

  /**
    * Bake statements created by a normalizing operation.
    * Empties the writer's log and returns it.
    */
  private def bake[A](x: Normalizing[A]): Normalizing[(A, List[StmtInNestedBlock])] =
    WriterT.listen(x).mapWritten(_.map(p => (false, p._2))).map(p => (p._1, p._2.filter(_._1).map(_._2)))

  /**
    * Nest statements in blocks, if necessary.
    * Should be applied pairwise to combine adjacent blocks and inline statements directly following a block into it.
    */
  private def nest(stmts: List[StmtInNestedBlock], span: Span): StmtInNestedBlock = stmts match {
    case List(stmt) => stmt
    case List(n: NestedBlockStmt, m: NestedBlockStmt) => NestedBlockStmt(n.body ++ m.body, span)
    case List(n: NestedBlockStmt, x) => NestedBlockStmt(n.body :+ x, span)
    case _ => NestedBlockStmt(stmts, span)
  }

  private def normalizeStmt(stmt: StmtInNestedBlock): Normalizing[StmtInNestedBlock] = stmt match {
    case stmt: AssignStmt => normalizeAssignment(stmt)
    case NestedBlockStmt(body, span) => Traverse[List].traverse(body)(stmt =>
      for {
        _s <- bake(normalizeStmt(stmt))
        (s, log) = _s
      } yield nest(log :+ s, span)
    ).map(nest(_, span))
    case IfStmt(guard, thenBranch, elseBranch, span) => for {
      _g <- bake(bind(guard))
      (g, guardLog) = _g
      _thn <- bake(normalizeStmt(thenBranch))
      (thn, thnLog) = _thn
      _els <- bake(Traverse[Option].traverse(elseBranch)(normalizeStmt))
      (els, elsLog) = _els
    } yield
        nest(guardLog :+ IfStmt(
          g,
          nest(thnLog :+ thn, thenBranch.span),
          els.map(e => nest(elsLog :+ e, e.span)),
          span
        ), span)
    case WhileStmt(guard, block, span) => for {
      _g <- bake(bind(guard, force = true))
      (g, gLog) = _g
      _b <- bake(normalizeStmt(block))
      (b, bLog) = _b
    } yield nest(gLog :+ WhileStmt(g, nest((bLog :+ b) ++ gLog.map(cloneStmt(span)), block.span), span), span)
    case OutputStmt(expr, span) => for {
      _e <- bake(bind(expr))
      (e, eLog) = _e
    } yield nest(eLog :+ OutputStmt(e, span), span)
  }

  private def cloneStmt(span: Span)(stmt: StmtInNestedBlock): StmtInNestedBlock =
    prism[StmtInNestedBlock].span.modify(stmt)((s: Span) => span ++ s)

  private def addStmt(stmt: StmtInNestedBlock): Normalizing[Unit] = WriterT.tell(List((true, stmt)))

  private object SimpleExpr {
    def unapply(expr: Expr): Boolean = expr match {
      case _: Identifier | _: Input | _: Number | _: Null => true
      case _ => false
    }
  }

  /**
    * Simplify an expression and bind its result to a (usually fresh) variable.
    * The expression is returned as-is if it is considered simple enough.
    * @param expr The expression to simplify.
    * @param force Force a simplification of simple binary operations (single level with simple operands).
    * @return The simplified expression.
    */
  private def bind(expr: Expr, force: Boolean = false): Normalizing[Expr] = expr match {
    case SimpleExpr() => Monad[Normalizing].pure(expr)
    case BinaryOp(_, SimpleExpr(), SimpleExpr(), _) if !force => Monad[Normalizing].pure(expr)
    case _ => for {
      name <- fresh
      expr <- normalizeExpr(expr)
      _ <- addStmt(AssignStmt(name, expr, expr.span))
    } yield name
  }

  private def normalizeExpr(expr: Expr): Normalizing[Expr] = expr match {
    case Null(_) | Input(_) | Number(_, _) | Identifier(_, _) =>
      throw new IllegalStateException("calls to normalizeExpr should only come from bind, which must handle these cases")
    case VarRef(id, span) => Monad[Normalizing].pure(VarRef(id, span))
    case Alloc(expr, span) => bind(expr).map(Alloc(_, span))
    case Deref(pointer, span) => bind(pointer).map(Deref(_, span))
    case Record(fields, span) => Traverse[List].traverse(fields) {
      case RecordField(name, expr, span) => bind(expr).map(RecordField(name, _, span))
    }.map(fields => Record(fields, span))
    case FieldAccess(record, field, span) => bind(record).map(FieldAccess(_, field, span))
    case CallFuncExpr(targetFun, args, span) => for {
      tf <- bind(targetFun)
      args <- Traverse[List].traverse(args)(e => bind(e, force = true))
    } yield CallFuncExpr(tf, args, span)
    case BinaryOp(operator, left, right, span) => for {
      l <- bind(left)
      r <- bind(right)
    } yield BinaryOp(operator, l, r, span)
  }
}
