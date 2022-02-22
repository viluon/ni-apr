package microc.ast

trait AstVisitor {
  def visit(node: AstNode): Unit = visitChildren(node)
  def visitChildren(node: AstNode): Unit = node.children.foreach(visit)
}
