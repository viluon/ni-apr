package microc.ast

import microc.util.CharacterSets.NL
import microc.util.Collections._
import microc.util.IndentWriter

import java.io.StringWriter

class AstPrinter(indent: Int = 2) extends AstVisitor {

  protected val sb: StringWriter = new StringWriter()
  protected val out: IndentWriter = new IndentWriter(sb, Some(indent))

  protected def comma(): Unit = out << ","

  override def visit(node: AstNode): Unit = node match {
    case CallFuncExpr(targetFun, args, _) =>
      visit(targetFun)
      out << "("
      args.foreachSep(visit, comma())
      out << ")"

    case Identifier(name, _) =>
      out.append(name)

    case BinaryOp(operator, left, right, _) =>
      out << "("
      visit(left)
      out << " " << operator.toString << " "
      visit(right)
      out << ")"

    case Deref(expr, _) =>
      out << "(*"
      visit(expr)
      out << ")"

    case Number(value, _) =>
      out << value.toString

    case Input(_) =>
      out << "input"

    case Alloc(expr, _) =>
      out << "alloc "
      visit(expr)

    case VarRef(id, _) =>
      out << "&"
      visit(id)

    case RecordField(name, expr, _) =>
      out << name << ":"
      visit(expr)

    case Record(fields, _) =>
      out << "{"
      fields.foreachSep(visit, comma())
      out << "}"

    case FieldAccess(record, field, _) =>
      visit(record)
      out << "." << field

    case Null(_) =>
      out << "null"

    case IdentifierDecl(name, _) =>
      out << name

    case FunDecl(name, params, block, _) =>
      out << name
      out << "("
      params.foreachSep(visit, comma())
      out << ") "
      visit(block)

    case AssignStmt(left, right, _) =>
      visit(left)
      out << " = "
      visit(right)
      out << ";"

    case IfStmt(guard, thenBranch, elseBranch, _) =>
      out << "if ("
      visit(guard)
      out << ") "
      visit(thenBranch)
      elseBranch.foreach { branch =>
        out << " else "
        visit(branch)
      }

    case OutputStmt(expr, _) =>
      out << "output "
      visit(expr)
      out << ";"

    case WhileStmt(guard, block, _) =>
      out << "while ("
      visit(guard)
      out << ") "
      visit(block)

    case x: Block if x.body.isEmpty =>
      out << "{}"

    case x: Block =>
      out << "{"
      out indent x.body.foreachSep(visit, out << NL)
      out << "}"

    case ReturnStmt(expr, _) =>
      out << "return "
      visit(expr)
      out << ";"

    case VarStmt(decls, _) =>
      out << "var "
      decls.foreachSep(visit, comma())
      out << ";"

    case Program(funs, _) =>
      funs.foreachSep(visit, out << NL << NL)
  }

  def result: String = sb.toString
}
