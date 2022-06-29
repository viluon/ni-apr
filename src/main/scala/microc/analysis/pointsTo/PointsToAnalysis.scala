package microc.analysis.pointsTo

import cats.data.Writer
import cats.{Monad, Traverse}
import microc.analysis.Declarations
import microc.ast
import microc.ast.{Alloc, AssignStmt, BinaryOp, CallFuncExpr, Decl, Deref, DirectWrite, Expr, FieldAccess, FunDecl, Identifier, IfStmt, IndirectWrite, Input, NestedBlockStmt, OutputStmt, Span, StmtInNestedBlock, VarRef, WhileStmt}
import microc.solver.Cubic
import microc.solver.Cubic.{CondSubSet, Constraint, In}

object PointsToAnalysis {
  sealed trait Cell {
    def span: Span
  }

  case class VarCell(decl: Decl) extends Cell {
    override def toString: String = decl.toString
    override def span: Span = decl.span
  }

  case class AllocCell(alloc: Alloc) extends Cell {
    override def toString: String = alloc.toString
    override def span: Span = alloc.span
  }

  case class Andersen(program: FunDecl, decls: Declarations) {
    def collectAddressOfCells: Writer[Set[Cell], Boolean] = {
      type M[A] = Writer[Set[Cell], A]

      def pure[A](x: A): M[A] = Monad[M].pure(x)
      def tell(cell: Cell): M[Unit] = Writer.tell(Set(cell))

      def goExpr(expr: Expr): M[Boolean] = expr match {
        case ast.Null(_) => pure(true)
        case ast.Number(_, _) => pure(true)
        case Identifier(_, _) => pure(true)
        case BinaryOp(_, _, _, _) => pure(true)
        case CallFuncExpr(_, _, _) => pure(true)
        case Input(_) => pure(true)
        case a: Alloc => tell(AllocCell(a)).map(_ => true)
        case VarRef(id, _) => tell(VarCell(decls(id))).map(_ => true)
        case Deref(_, _) => pure(true)
        case ast.Record(_, _) => pure(true)
        case FieldAccess(_, _, _) => pure(true)
      }

      // TODO this is general for anything that focuses on expressions
      def go(stmt: StmtInNestedBlock): M[Boolean] = stmt match {
        case AssignStmt(left, right, _) => goExpr(left).flatMap(_ => goExpr(right))
        case NestedBlockStmt(body, _) => Traverse[List].traverse(body)(go).map(_ => true)
        case IfStmt(guard, thenBranch, elseBranch, _) => for {
          _ <- goExpr(guard)
          _ <- go(thenBranch)
          _ <- Traverse[Option].traverse(elseBranch)(go)
        } yield true
        case WhileStmt(guard, block, _) => goExpr(guard).flatMap(_ => go(block))
        case OutputStmt(expr, _) => goExpr(expr)
      }

      Traverse[List].traverse(program.block.stmts)(go).map(_ => true)
    }

    def generateConstraints(addressOfCells: Seq[Cell]): Writer[List[Constraint[Cell, Cell]], Boolean] = {
      type M[A] = Writer[List[Constraint[Cell, Cell]], A]
      def pure[A](x: A): M[A] = Monad[M].pure(x)
      def tell(cell: Constraint[Cell, Cell]): M[Unit] = Writer.tell(List(cell))

      def go(stmt: StmtInNestedBlock): M[Boolean] = stmt match {
        case AssignStmt(DirectWrite(_, _), ast.Null(_), _) => pure(true)
        case AssignStmt(DirectWrite(x, _), a: Alloc, _) => tell(In(AllocCell(a), VarCell(decls(x)))).map(_ => true)
        case AssignStmt(DirectWrite(x, _), VarRef(y, _), _) => tell(In(VarCell(decls(y)), VarCell(decls(x)))).map(_ => true)
        case AssignStmt(DirectWrite(x, _), y: Identifier, _) => // TODO this requires a new unconditional constraint for [[y]] ⊆ [[x]]
          throw new IllegalStateException("direct assignment is not yet implemented")
        case AssignStmt(DirectWrite(x, _), Deref(y: Identifier, _), _) =>
          Writer.tell[List[Constraint[Cell, Cell]]]((
            for (c <- addressOfCells) yield CondSubSet[Cell, Cell](In(c, VarCell(decls(y))), c, VarCell(decls(x)))
          ).toList).map(_ => true)
        case AssignStmt(IndirectWrite(x: Identifier, _), y: Identifier, _) =>
          Writer.tell[List[Constraint[Cell, Cell]]]((
            for (c <- addressOfCells) yield CondSubSet[Cell, Cell](In(c, VarCell(decls(x))), VarCell(decls(y)), c)
          ).toList).map(_ => true)
        case NestedBlockStmt(body, _) => Traverse[List].traverse(body)(go).map(_ => true)
        case IfStmt(_, thenBranch, elseBranch, _) => for {
          _ <- go(thenBranch)
          _ <- Traverse[Option].traverse(elseBranch)(go)
        } yield true
        case WhileStmt(_, block, _) => go(block)
        case _ => throw new IllegalStateException(s"cannot handle $stmt")
      }

      Traverse[List].traverse(program.block.stmts)(go).map(_ => true)
    }

    def analyse(): Map[Decl, Set[Cell]] = {
      val (addressOfCells, _) = collectAddressOfCells.run
      val cells = addressOfCells union program.params.appendedAll(
        program.block.vars/*FIXME: may not catch all of them due to normalization*/.flatMap(_.decls)
      ).map(VarCell).toSet
      val (constraints, _) = generateConstraints(addressOfCells.toSeq).run
      val solver = microc.solver.Cubic(cells, cells, constraints)
      val sln = solver.solve()
      sln.flatMap(p => p._1 match {
        case VarCell(decl) => Map(decl -> (p._2 match {
          case Cubic.ConstrVar(_, sol, _, _) => sol.toSet
        }))
        case AllocCell(_) => Map()
      }).toMap
    }
  }
}
