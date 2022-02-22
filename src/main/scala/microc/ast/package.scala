package microc

import microc.analysis.Declarations

import scala.collection.mutable

package object ast {

  /**
    * Defines convenient operation on identifiers
    *
    * @param that the target for the operation
    */
  implicit class IdentifierOps(that: Identifier) {

    /**
      * Links an identifier to its declaration.
      *
      * @param declarations a set of declaration to use for the lookup
      * @return identifier's declaration
      */
    def declaration(implicit declarations: Declarations): Decl = declarations(that)
  }

  /**
    * Defines convenient operations on AST nodes
    *
    * @param node the target for the operation
    */
  implicit class AstOps(node: AstNode) {

    /**
      * Returns the set of identifier declarations appearing in the subtree of the node.
      *
      * @param declarations a set of declaration to use for the lookup
      * @return a set of declarations that appear in the subtree starting by this node
      */
    def appearingIds(implicit declarations: Declarations): Set[Decl] = {
      val ids = mutable.Set[Decl]()

      val visitor = new DepthFirstAstVisitor[Unit] {
        override def visit(node: AstNode, arg: Unit): Unit = {
          node match {
            case id: Identifier =>
              id.declaration match {
                case local: IdentifierDecl => ids += local
                case fun: FunDecl          => ids += fun
              }
            case decl: IdentifierDecl => ids += decl
            case fun: FunDecl         => ids += fun
            case _                           =>
          }
          visitChildren(node, ())
        }
      }

      visitor.visit(node, ())
      ids.toSet
    }
  }
}
