package microc

import microc.ast.{Decl, FunDecl, Identifier}

package object analysis {

  type Declarations = Map[Identifier, (Option[FunDecl], Decl)]

}
