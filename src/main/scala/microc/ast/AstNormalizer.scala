package microc.ast

import cats.data.{State, WriterT}
import cats.syntax.bifunctor._
import cats.{Monad, Traverse}

object AstNormalizer {
  def normalize(program: Program): Program = Program(
    Traverse[List].traverse(program.funs)(normalizeFun).runA(FreshStore(0)).value, program.span
  )

  private case class FreshStore(k: Int)
  private type VariableStore[A] = State[FreshStore, A]
  private type Normalizing[A] = WriterT[VariableStore, List[StmtInNestedBlock], A]

  private def fresh: Normalizing[Identifier] = WriterT.liftF(for {
    store <- State.get
    _ <- State.set(store.copy(k = store.k + 1))
  } yield Identifier(s"_t${store.k}", Span.invalid))

  private def normalizeFun(decl: FunDecl): VariableStore[FunDecl] = decl match {
    case FunDecl(name, params, block, span) =>
      val normalized = Traverse[List].traverse(block.stmts)(normalizeStmt)
      for {
        pair <- normalized.run.map(_.leftMap(_.map {
          case AssignStmt(DirectWrite(Identifier(name, idSpan), _), _, span) =>
            VarStmt(List(IdentifierDecl(name, idSpan)), span)
          case _ => ???
        }))
        (newVars, stmts) = pair
      } yield FunDecl(name, params, FunBlockStmt(block.vars ++ newVars, stmts, block.ret, block.span), span)
  }

  private def normalizeAssignment(stmt: AssignStmt): Normalizing[StmtInNestedBlock] = stmt.left match {
    case DirectWrite(_, _) | DirectFieldWrite(_, _, _) => normalizeExpr(stmt.right).map(AssignStmt(stmt.left, _, stmt.span))
    case IndirectFieldWrite(expr, field, span) => ???
    case IndirectWrite(expr, span) => ???
    case _ => throw new IllegalStateException()
  }

  private def normalizeStmt(stmt: StmtInNestedBlock): Normalizing[StmtInNestedBlock] = stmt match {
    case stmt: AssignStmt => normalizeAssignment(stmt)
    case NestedBlockStmt(body, span) => Traverse[List].traverse(body)(normalizeStmt).map(NestedBlockStmt(_, span))
    case IfStmt(guard, thenBranch, elseBranch, span) => for {
      g <- normalizeExpr(guard)
      thn <- normalizeStmt(thenBranch)
      els <- Traverse[Option].traverse(elseBranch)(normalizeStmt)
    } yield IfStmt(g, thn, els, span)
    case WhileStmt(guard, block, span) => for {
      g <- normalizeExpr(guard)
      b <- normalizeStmt(block)
    } yield WhileStmt(g, b, span)
    case OutputStmt(expr, span) => normalizeExpr(expr).map(OutputStmt(_, span))
  }

  private def addStmt(stmt: StmtInNestedBlock): Normalizing[Unit] = WriterT.tell(List(stmt))

  private def bind(expr: Expr): Normalizing[Expr] = for {
    name <- fresh
    expr <- normalizeExpr(expr)
    _ <- addStmt(AssignStmt(name, expr, expr.span))
  } yield name

  private def normalizeExpr(expr: Expr): Normalizing[Expr] = expr match {
    case Null(_) | Input(_) | VarRef(_, _) | Number(_, _) | Identifier(_, _) => Monad[Normalizing].pure(expr)
//    case Alloc(Identifier(_, _), _) | Deref(Identifier(_, _), _) => expr
    case Alloc(expr, span) => bind(expr).map(Alloc(_, span))
    case Deref(pointer, span) => bind(pointer).map(Deref(_, span))
    case Record(fields, span) => Traverse[List].traverse(fields) {
      case RecordField(name, expr, span) => bind(expr).map(RecordField(name, _, span))
    }.map(fields => Record(fields, span))
    case FieldAccess(record, field, span) => bind(record).map(FieldAccess(_, field, span))
    case CallFuncExpr(targetFun, args, span) => for {
      tf <- bind(targetFun)
      args <- Traverse[List].traverse(args)(bind)
    } yield CallFuncExpr(tf, args, span)
    case BinaryOp(operator, left, right, span) => for {
      l <- bind(left)
      r <- bind(right)
    } yield BinaryOp(operator, l, r, span)
  }
}
