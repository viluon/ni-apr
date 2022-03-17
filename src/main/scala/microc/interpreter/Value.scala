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
// represents an index into a record (for l-values), i.e. a pointer of type String
case class RecordFieldVal(addr: Value, field: String) extends Value
case class RecordVal(fields: Map[String, Value]) extends Value {
  override def toString: String = fields.toList.sortBy(_._1).map(f => s"${f._1}: ${f._2}").mkString("{", ", ", "}")
}
