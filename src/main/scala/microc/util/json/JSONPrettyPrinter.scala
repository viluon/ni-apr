package microc.util.json

import microc.util.CharacterSets.NL
import microc.util.Collections._
import microc.util.IndentWriter

import java.io.Writer

class JSONPrettyPrinter(indent: Option[Int] = None) {
    private def visitField(field: (JSONStr, JSON), out: IndentWriter): Unit = {
      val (key, value) = field
      visit(key, out)
      out << ": "
      visit(value, out)
    }

    private def sep(out: IndentWriter): Unit = {
      out << ","
      if (indent.isDefined) {
        out << NL
      }
    }

    private def visit(json: JSON, out: IndentWriter): Unit = json match {
      case JSONNum(v) =>
        out << s"\"${v.toString}\""
      case JSONStr(v) =>
        out << s"\"$v\""
      case JSONArr(Nil) =>
        out << "[]"
      case JSONArr(elems) =>
        out << "["
        out indent elems.foreachSep(visit(_, out), sep(out))
        out << "]"
      case JSONObj(Nil) =>
        out << "{}"
      case JSONObj(fields) =>
        out << "{"
        out indent fields.foreachSep(visitField(_, out), sep(out))
        out << "}"
    }

    def print(json: JSON, to: Writer): Unit = {
      visit(json, new IndentWriter(to, indent))
    }
}