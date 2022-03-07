package microc.interpreter

sealed trait Value {
  final def truthy: Boolean = this match {
    case NullVal | IntVal(0) => false
    case _ => true
  }
}

object NullVal extends Value
case class IntVal(n: Int) extends Value
case class AddrVal(addr: Int) extends Value
case class FunAddrVal(name: String) extends Value
case class RecordVal() extends Value // TODO
