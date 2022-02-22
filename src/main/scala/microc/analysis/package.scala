package microc

import microc.ast.{Decl, Identifier}

package object analysis {

  type Declarations = Map[Identifier, Decl]

}
