package microc.ast

trait DepthFirstAstVisitor[T] {
  def visit(node: AstNode, arg: T): Unit
  def visitChildren(node: AstNode, arg: T): Unit = node.children.foreach(n => visit(n, arg))
}
