package microc.ast

import microc.util.json._

class JSONAstPrinter extends AstVisitor {
  override def visit(node: AstNode): Unit = serialize(node)

  def serialize(v: Any): JSON = v match {
    case xs: Iterable[_] => JSONArr(xs.map(serialize).toList)
    case x: Product      => serializeProduct(x)
    case x               => JSONStr(x.toString)
  }

  protected def serializeProduct(that: Product): JSONObj = {
    val fields = for {
      i <- (0 until that.productArity).toList
      name = that.productElementName(i)
      value = serialize(that.productElement(i))
    } yield JSONStr(name) -> value

    JSONObj(List(JSONStr(that.productPrefix) -> JSONObj(fields)))
  }
}
