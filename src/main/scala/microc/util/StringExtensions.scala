package microc.util

object StringExtensions {
  implicit class StringOps(val str: String) extends AnyVal {
    def indented: String = indent(2)
    def indent(n: Int): String = str.linesIterator.map(line => " " * n + line).mkString(System.lineSeparator())
  }
}
