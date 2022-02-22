package microc

import scala.collection.Set

package object parser {
  object Keywords {
    val Alloc = "alloc"
    val Input = "input"
    val While = "while"
    val If = "if"
    val Else = "else"
    val Var = "var"
    val Return = "return"
    val Null = "null"
    val Output = "output"

    val All: Set[String] = Set(
      Alloc,
      Input,
      While,
      If,
      Else,
      Var,
      Return,
      Null,
      Output
    )
  }
}
