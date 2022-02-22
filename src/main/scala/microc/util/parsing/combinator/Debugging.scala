package microc.util.parsing.combinator

trait Debugging extends Parsers {
  private var indent = 0
  private var parsers = 0
  private var accepts = 0
  private var rejects = 0
  private var depth = 0

  abstract override def Parser[A](f: Input => Result[A]): Parser[A] = {
    var name: () => String = () => ""
    val p = super.Parser { in =>
      println(s"${"  " * indent}Trying «${name()}» at '$in'")
      indent += 1
      depth = math.max(depth, indent)
      val r = f(in)
      if (r.success) accepts += 1 else rejects += 1
      indent -= 1
      println(s"${"  " * indent}«${name()}» --> $r (${r.hashCode()})")
      r
    }
    name = () => s"${p.label} (${p.hashCode()})"
    println(s"Created: ${name()}")
    parsers += 1
    p
  }

  override def parse[A](p: Parser[A], in: Input): Result[A] = {
    val r = super.parse(p, in)
    printStats()
    r
  }

  def printStats(): Unit = {
    println(s"""** PARSING STATS:
               |   - parsers: $parsers
               |   - attempts: ${accepts + rejects}
               |     - accepts: $accepts
               |     - rejects: $rejects
               |   - max depth: $depth
               |""".stripMargin)
  }
}
