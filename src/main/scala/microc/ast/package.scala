package microc

import microc.analysis.Declarations

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
      def visit(node: AstNode): Set[Decl] = {
        val ids: Set[Decl] = node match {
          case x: Identifier => Set(x.declaration)
          case x: Decl => Set(x)
          case _ => Set.empty
        }
        node.children.foldLeft(ids)((xs, x) => xs ++ visit(x))
      }

      visit(node)
    }
  }
}
