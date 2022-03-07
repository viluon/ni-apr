package microc.interpreter

import microc.ast.Identifier

case class InterpreterState(heap: Array[Value], env: Map[Identifier, Int])
