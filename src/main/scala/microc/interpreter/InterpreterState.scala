package microc.interpreter

import microc.ast.IdentifierDecl

case class InterpreterState(heap: Array[Value], env: Map[IdentifierDecl, Int])
