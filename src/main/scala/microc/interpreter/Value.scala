package microc.interpreter

import microc.ast.FunDecl

sealed trait Value {
  final def truthy: Boolean = this match {
    case NullVal | IntVal(0) => false
    case _ => true
  }
}

object NullVal extends Value {
  override def toString: String = "NullVal"
}
case class IntVal(n: Int) extends Value
case class AddrVal(addr: Int) extends Value
case class FunAddrVal(decl: FunDecl) extends Value
case class RecordVal() extends Value // TODO