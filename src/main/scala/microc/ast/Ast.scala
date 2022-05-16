package microc.ast

import microc.util.ReflectionHelpers

/**
  * A source code location of an AST node.
  *
  * @param line The line number.
  * @param col  The column number.
  */
case class Loc(line: Int, col: Int) extends Ordered[Loc] {
  override def toString: String = s"$line:$col"

  override def compare(that: Loc): Int = {
    val d = this.line - that.line
    if (d == 0) this.col - that.col else d
  }
}

object Loc {
  val invalid = Loc(-1, -1)
}

case class Span(from: Loc, to: Loc, highlight: Option[Span] = None) extends Ordered[Span] {
  def containsPos(line: Int, col: Int): Boolean = containsLine(line) &&
    ((if (line == from.line) from.col else 0) to (if (line == to.line) to.col else Int.MaxValue)).contains(col)

  def containsLine(line: Int): Boolean = (from.line to to.line) contains line

  override def toString: String = s"$from-$to"

  override def compare(that: Span): Int = {
    val d = this.from.compare(that.from)
    if (d == 0) this.to.compare(that.to) else d
  }

  def ++(other: Span): Span = other match {
    case Span(_, to, hl) => Span(from, to, highlight.orElse(hl))
  }

  def highlighting(hl: Span): Span = copy(highlight = Some(hl))
}

object Span {
  val invalid: Span = Span(Loc.invalid, Loc.invalid)
}

/** A binary operator */
sealed trait BinaryOperator {
  def eval(l: Int, r: Int): Option[Int] = this match {
    case Plus() => Some(l + r)
    case Minus() => Some(l - r)
    case Times() => Some(l * r)
    case Divide() => if (r == 0) None else Some(l / r)
    case Equal() => Some(if (l == r) 1 else 0)
    case GreaterThan() => Some(if (l > r) 1 else 0)
  }
}

object BinaryOperator {
  lazy val all: Set[BinaryOperator] = reflect.runtime.universe
    .typeOf[BinaryOperator].typeSymbol.asClass.knownDirectSubclasses
    .map(op => new ReflectionHelpers.CaseClassFactory(op.asType.toType))
    .map(_.buildWith(Seq()).asInstanceOf[BinaryOperator])

  def apply(s: String): BinaryOperator = s match {
    case "+"  => Plus()
    case "-"  => Minus()
    case "*"  => Times()
    case "/"  => Divide()
    case "==" => Equal()
    case ">"  => GreaterThan()
  }
}

case class Plus() extends BinaryOperator {
  override def toString: String = "+"
}

case class Minus() extends BinaryOperator {
  override def toString: String = "-"
}

case class Times() extends BinaryOperator {
  override def toString: String = "*"
}

case class Divide() extends BinaryOperator {
  override def toString: String = "/"
}

case class Equal() extends BinaryOperator {
  override def toString: String = "=="
}

case class GreaterThan() extends BinaryOperator {
  override def toString: String = ">"
}

// ----------------------------------------------------------------------------
// BASE NODES
// ----------------------------------------------------------------------------

object AstNode {
  private val Printer = new AstPrinter()
}

/**
  * An AST node
  */
sealed abstract class AstNode {

  /**
    * The source code location of the AST node.
    *
    * @return a source code location.
    */
  var span: Span

  def children: Iterable[AstNode] = Iterable.empty

  def tree: Iterable[AstNode] = Iterable(this) ++ children.flatMap(_.tree)

  override def toString: String = {
    AstNode.Printer.print(this) + s"[$span]"
  }
}

sealed trait Expr extends AstNode

sealed trait Stmt extends AstNode

sealed trait Decl extends AstNode {
  def name: String
}

/**
  * A statement in a nested block.
  *
  * It cannot be a declaration or a return.
  */
sealed trait StmtInNestedBlock extends Stmt

// ----------------------------------------------------------------------------
// EXPRESSIONS
// ----------------------------------------------------------------------------

case class Null(var span: Span) extends Expr

case class Number(value: Int, var span: Span) extends Expr

case class Identifier(name: String, var span: Span) extends Expr

case class BinaryOp(operator: BinaryOperator, left: Expr, right: Expr, var span: Span) extends Expr {
  override def children: Iterable[AstNode] = List(left, right)
}

case class CallFuncExpr(targetFun: Expr, args: List[Expr], var span: Span) extends Expr {
  override def children: Iterable[AstNode] = targetFun :: args
}

/**
  * Read of one integer from standard input.
  *
  * @param loc The source code location.
  */
case class Input(var span: Span) extends Expr

case class Alloc(expr: Expr, var span: Span) extends Expr {
  override def children: Iterable[AstNode] = List(expr)
}

/**
  * Variable reference.
  *
  * It is used to create a pointer to a variable `id` (&id).
  *
  * @param id  The target variable.
  * @param loc The source code location.
  */
case class VarRef(id: Identifier, var span: Span) extends Expr {
  override def children: Iterable[AstNode] = List(id)
}

/**
  * Pointer dereference
  *
  * It is used to dereference a pointer `p` (`*p`).
  *
  * @param pointer The pointer to dereference
  * @param loc The source code location
  */
case class Deref(pointer: Expr, var span: Span) extends Expr {
  override def children: Iterable[AstNode] = List(pointer)
}

case class Record(fields: List[RecordField], var span: Span) extends Expr {
  override def children: Iterable[AstNode] = fields.map(_.expr)
}

case class RecordField(name: String, expr: Expr, var span: Span) extends AstNode {
  override def children: Iterable[AstNode] = List(expr)
}

/**
  * Accessing a field of a record (x.y).
  *
  * @param record The expression that evaluates to a record value.
  * @param field  The name of the field of the record.
  * @param loc    The source code location.
  */
case class FieldAccess(record: Expr, field: String, var span: Span) extends Expr {
  override def children: Iterable[AstNode] = List(record)
}

// ----------------------------------------------------------------------------
// STATEMENTS
// ----------------------------------------------------------------------------

/**
  * An Assignment.
  *
  * @param left  The LValue (where to assign)
  * @param right The RValue (what to assign)
  * @param loc   The source code location
  */
case class AssignStmt(left: Expr, right: Expr, var span: Span) extends StmtInNestedBlock {
  override def children: Iterable[AstNode] = List(left, right)
}

case object DirectWrite {
  def unapply(expr: Expr): Option[(Identifier, Span)] = expr match {
    case x: Identifier => Some(x, x.span)
    case _             => None
  }
}

case object IndirectWrite {
  def unapply(expr: Expr): Option[(Expr, Span)] = expr match {
    case Deref(e, l) => Some(e, l)
    case _           => None
  }
}

case object DirectFieldWrite {
  def unapply(expr: Expr): Option[(Identifier, String, Span)] = expr match {
    case FieldAccess(record: Identifier, field, span) => Some((record, field, span))
    case _                                           => None
  }
}

case object IndirectFieldWrite {
  def unapply(expr: Expr): Option[(Expr, String, Span)] = expr match {
    case FieldAccess(Deref(record, _), field, span) => Some((record, field, span))
    case _                                         => None
  }
}

sealed trait Block extends Stmt {
  def body: List[Stmt]

  override def children: Iterable[AstNode] = body
}

object Block {
  def unapply(that: Block): Option[List[Stmt]] = Some(that.body)
}

/**
  * Represents a block of statements surrounded by curly braces.
  *
  * {{{
  * {
  *   stmt_1
  *   stmt_2
  *   ...
  *   stmt_n
  * }
  * }}}
  *
  * @param body The list of statements in this block
  * @param loc The location
  */
case class NestedBlockStmt(body: List[StmtInNestedBlock], var span: Span) extends Block with StmtInNestedBlock

case class FunBlockStmt(vars: List[VarStmt], stmts: List[StmtInNestedBlock], ret: ReturnStmt, var span: Span) extends Block {
  val body: List[Stmt] = vars ++ (stmts :+ ret)
}

case class ReturnStmt(expr: Expr, var span: Span) extends Stmt {
  override def children: Iterable[AstNode] = List(expr)
}

case class IfStmt(guard: Expr, thenBranch: StmtInNestedBlock, elseBranch: Option[StmtInNestedBlock], var span: Span)
    extends StmtInNestedBlock {
  override def children: Iterable[AstNode] = guard :: thenBranch :: elseBranch.map(x => List(x)).getOrElse(Nil)
}

case class WhileStmt(guard: Expr, block: StmtInNestedBlock, var span: Span) extends StmtInNestedBlock {
  override def children: Iterable[AstNode] = List(guard, block)
}

case class OutputStmt(expr: Expr, var span: Span) extends StmtInNestedBlock {
  override def children: Iterable[AstNode] = List(expr)
}

/**
  * Function local variables declaration.
  *
  * @param decls The list of variable declarations.
  * @param loc   The source code location.
  */
case class VarStmt(decls: List[IdentifierDecl], var span: Span) extends Stmt {
  override def children: Iterable[AstNode] = decls
}

case class IdentifierDecl(name: String, var span: Span) extends Decl

case class FunDecl(name: String, params: List[IdentifierDecl], block: FunBlockStmt, var span: Span) extends Decl {
  override def toString: String = s"$name(${params.mkString(",")}){...}:$span"

  override def children: Iterable[AstNode] = params :+ block
}

/**
  * The complete program.
  *
  * A program is just a list of functions.
  * The `main` function is where the execution begins.
  *
  * @param funs The list of functions.
  * @param loc  The source code location.
  */
case class Program(funs: List[FunDecl], var span: Span) extends AstNode {
  def mainFunOption: Option[FunDecl] =
    funs.find(_.name == "main")

  override def children: Iterable[AstNode] = funs

  def declarations: Iterable[Decl] = tree.collect { case x: Decl => x }
}
