package microc.ast

import microc.util.CharacterSets.NL
import microc.util.Collections._
import microc.util.IndentWriter

import java.io.StringWriter

class AstPrinter(indent: Int = 2) {

  def print(node: AstNode): String = {
    val sb: StringWriter = new StringWriter()
    val out: IndentWriter = new IndentWriter(sb, Some(indent))
    visit(node, out)
    sb.toString
  }

  protected def visit(node: AstNode, out: IndentWriter): Unit = {
    def comma(): Unit = out << ","

    node match {
      case CallFuncExpr(targetFun, args, _) =>
        visit(targetFun, out)
        out << "("
        args.foreachSep(visit(_, out), comma())
        out << ")"

      case Identifier(name, _) =>
        out.append(name)

      case BinaryOp(operator, left, right, _) =>
        out << "("
        visit(left, out)
        out << " " << operator.toString << " "
        visit(right, out)
        out << ")"

      case Deref(expr, _) =>
        out << "(*"
        visit(expr, out)
        out << ")"

      case Number(value, _) =>
        out << value.toString

      case Input(_) =>
        out << "input"

      case Alloc(expr, _) =>
        out << "alloc "
        visit(expr, out)

      case VarRef(id, _) =>
        out << "&"
        visit(id, out)

      case RecordField(name, expr, _) =>
        out << name << ":"
        visit(expr, out)

      case Record(fields, _) =>
        out << "{"
        fields.foreachSep(visit(_, out), comma())
        out << "}"

      case FieldAccess(record, field, _) =>
        visit(record, out)
        out << "." << field

      case Null(_) =>
        out << "null"

      case IdentifierDecl(name, _) =>
        out << name

      case FunDecl(name, params, block, _) =>
        out << name
        out << "("
        params.foreachSep(visit(_, out), comma())
        out << ") "
        visit(block, out)

      case AssignStmt(left, right, _) =>
        visit(left, out)
        out << " = "
        visit(right, out)
        out << ";"

      case IfStmt(guard, thenBranch, elseBranch, _) =>
        out << "if ("
        visit(guard, out)
        out << ") "
        visit(thenBranch, out)
        elseBranch.foreach { branch =>
          out << " else "
          visit(branch, out)
        }

      case OutputStmt(expr, _) =>
        out << "output "
        visit(expr, out)
        out << ";"

      case WhileStmt(guard, block, _) =>
        out << "while ("
        visit(guard, out)
        out << ") "
        visit(block, out)

      case x: Block if x.body.isEmpty =>
        out << "{}"

      case x: Block =>
        out << "{"
        out indent x.body.foreachSep(visit(_, out), out << NL)
        out << "}"

      case ReturnStmt(expr, _) =>
        out << "return "
        visit(expr, out)
        out << ";"

      case VarStmt(decls, _) =>
        out << "var "
        decls.foreachSep(visit(_, out), comma())
        out << ";"

      case Program(funs, _) =>
        funs.foreachSep(visit(_, out), out << NL << NL)
    }
  }
}
