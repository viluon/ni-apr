package microc.util.json

sealed trait JSON
case class JSONObj(v: List[(JSONStr, JSON)]) extends JSON
case class JSONArr(v: List[JSON]) extends JSON
case class JSONStr(v: String) extends JSON
case class JSONNum(v: Int) extends JSON
