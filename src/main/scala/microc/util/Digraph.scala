package microc.util

case class Digraph(name: String, nodes: Set[String], edges: Set[(String, String, String)]) {
  import StringExtensions.StringOps
  override def toString: String = s"digraph $name {\n" + body.indented + "\n}"
  lazy val body: String =
    nodes.toList.sorted.mkString("\n") + "\n" + edges.toList.sorted.map {
      case (from, to, props) => s"$from -> $to $props"
    }.mkString("\n")
}
