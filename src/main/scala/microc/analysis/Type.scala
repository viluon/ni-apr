package microc.analysis

import scala.util.control.Breaks.{break, breakable}

sealed trait Type
object Type {
  case class Pointer(t: Type) extends Type {
    override def toString: String = s"↑$t"
  }

  case class Function(params: List[Type], ret: Type) extends Type {
    override def toString: String = params.mkString("(", ", ", ") -> ") + ret.toString
  }

  case class Var(n: scala.Int) extends Type {
    private lazy val str = {
      var m = n
      var str = ""
      breakable {
        while (true) {
          str += ('₀' to '₉') (m % 10)
          if (m < 10) break
          m /= 10
        }
      }
      "τ" + str.reverse
    }

    override def toString: String = str
  }

  case object Int extends Type {
    override def toString: String = "int"
  }
}
