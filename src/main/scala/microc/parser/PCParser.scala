package microc.parser

import microc.ast
import microc.ast.Loc
import microc.util.parsing.combinator.{CharInput, StringParsers}

import scala.util.matching.Regex

/**
  * A parser combinator implementation of the microC grammar using a PEG-style parser.
  *
  * Each of the public lazy val corresponds to one of the grammar non-terminal.
  *
  * The [[microc.parser.Parser]] can be mixed in directly as the parser is stateless.
  */
class PCParser extends StringParsers with microc.parser.Parser {

  // ----------------------------------------------------------------------------
  // PRIMITIVES
  // ----------------------------------------------------------------------------

  lazy val Number: Parser[ast.Number] =
    C ~ """-?\d+""".r ^^ { case loc ~ num => ast.Number(num.toInt, loc) } label "Number"

  lazy val Id: Parser[String] =
    ("""[_a-zA-Z]\w*""".r)
      .withMessage("expected identifier, got '" + handleWhiteSpace(_).charAt(0) + "'")
      .filterWithMessage(!Keywords.All.contains(_), "expected identifier, got keyword '" + _ + "'")
      .label("Id")

  // ----------------------------------------------------------------------------
  // EXPRESSIONS
  // ----------------------------------------------------------------------------

  lazy val Identifier: Parser[ast.Identifier] =
    C ~ Id ^^ { case loc ~ name => ast.Identifier(name, loc) } label "Identifier"

  lazy val Input: Parser[ast.Input] =
    C <~ Keywords.Input ^^ ast.Input label "Input"

  lazy val Alloc: Parser[ast.Alloc] =
    (C <~ Keywords.Alloc) ~ Expr ^^ { case loc ~ expr => ast.Alloc(expr, loc) } label "Alloc"

  lazy val Null: Parser[ast.Null] =
    C <~ Keywords.Null ^^ (loc => ast.Null(loc)) label "Null"

  lazy val Args: Parser[List[ast.Expr]] = repsep(Expr, ",") label "Args"

  lazy val Call: Parser[ast.Expr => ast.CallFuncExpr] =
    (C <~ "(") ~ (Args <~ ")") ^^ {
      case loc ~ args =>
        expr =>
          ast.CallFuncExpr(expr, args, loc)
    } label "Call"

  lazy val FieldAccess: Parser[ast.Expr => ast.FieldAccess] =
    (C <~ ".") ~ Id ^^ {
      case loc ~ field =>
        expr =>
          ast.FieldAccess(expr, field, loc)
    } label "FieldAccess"

  lazy val Deref: Parser[ast.Deref] =
    (C <~ "*") ~ UnaryExpr ^^ { case loc ~ expr => ast.Deref(expr, loc) }

  lazy val Ref: Parser[ast.VarRef] =
    (C <~ "&") ~ Identifier ^^ { case loc ~ expr => ast.VarRef(expr, loc) }

  lazy val PrimaryExpr: Parser[ast.Expr] = Number | Identifier | Parens | Record label "PrimaryExpr"

  lazy val PostfixExpr: Parser[ast.Expr] = PrimaryExpr ~ (FieldAccess | Call).* ^^ {
    case first ~ Nil      => first
    case first ~ builders => builders.foldLeft(first)((acc, x) => x(acc))
  }

  lazy val UnaryExpr: Parser[ast.Expr] = Deref | Ref | Input | Alloc | Null | PostfixExpr

  lazy val MultiplicativeExpr: Parser[ast.Expr] = chainl1(UnaryExpr, binaryOp(ast.Times) | binaryOp(ast.Divide))

  lazy val AdditiveExpr: Parser[ast.Expr] = chainl1(MultiplicativeExpr, binaryOp(ast.Plus) | binaryOp(ast.Minus))

  lazy val RelationalExpr: Parser[ast.Expr] = chainl1(AdditiveExpr, binaryOp(ast.GreatThan))

  lazy val EqualityExpr: Parser[ast.Expr] = chainl1(RelationalExpr, binaryOp(ast.Equal)) label "EqualityExpr"

  lazy val Expr: Parser[ast.Expr] = EqualityExpr withMessage (_ => "expected expression") label "Expr"

  lazy val Parens: Parser[ast.Expr] = "(" ~> Expr <~ ")" label "Parens"

  // ----------------------------------------------------------------------------
  // EXPRESSIONS - RECORD DECLARATIONS
  // ----------------------------------------------------------------------------

  lazy val RecordField: Parser[ast.RecordField] =
    C ~ (Id <~ ":") ~ Expr ^^ { case loc ~ id ~ expr => ast.RecordField(id, expr, loc) }

  lazy val Record: Parser[ast.Record] =
    (C <~ "{") ~ repsep(RecordField, ",") <~ "}" ^^ { case loc ~ fields => ast.Record(fields, loc) }

  // ----------------------------------------------------------------------------
  // STATEMENTS
  // ----------------------------------------------------------------------------

  lazy val OutputStmt: Parser[ast.OutputStmt] =
    (C <~ Keywords.Output) ~ Expr <~ ";" ^^ { case loc ~ expr => ast.OutputStmt(expr, loc) }

  lazy val AssignmentStmt: Parser[ast.AssignStmt] =
    Expr ~ (C <~ "=") ~ Expr <~ ";" ^^ {
      case left ~ loc ~ right => ast.AssignStmt(left, right, loc)
    }

  lazy val BlockStmt: Parser[ast.NestedBlockStmt] =
    (C <~ "{") ~ Stmts <~ "}" ^^ { case loc ~ body => ast.NestedBlockStmt(body, loc) }

  lazy val WhileStmt: Parser[ast.WhileStmt] =
    ((C <~ Keywords.While) <~ "(") ~ Expr ~ (")" ~> Stmt) ^^ {
      case loc ~ guard ~ body => ast.WhileStmt(guard, body, loc)
    }

  lazy val IfStmt: Parser[ast.IfStmt] =
    ((C <~ Keywords.If) <~ "(") ~ Expr ~ (")" ~> Stmt) ~ (Keywords.Else ~> Stmt).? ^^ {
      case loc ~ guard ~ thenBranch ~ elseBranch => ast.IfStmt(guard, thenBranch, elseBranch, loc)
    }

  lazy val VarStmt: Parser[ast.VarStmt] =
    (C <~ Keywords.Var) ~ rep1sep(IdentifierDecl, ",") <~ ";" ^^ {
      case loc ~ vars => ast.VarStmt(vars, loc)
    }

  lazy val VarStmts: Parser[List[ast.VarStmt]] = VarStmt.*

  lazy val Stmt: Parser[ast.StmtInNestedBlock] = OutputStmt | WhileStmt | IfStmt | attempt(BlockStmt) | AssignmentStmt

  lazy val Stmts: Parser[List[ast.StmtInNestedBlock]] = Stmt.*

  lazy val ReturnStmt: Parser[ast.ReturnStmt] =
    (C <~ Keywords.Return) ~ Expr <~ ";" ^^ { case loc ~ expr => ast.ReturnStmt(expr, loc) }

  lazy val FunBlockStmt: Parser[ast.FunBlockStmt] =
    (C <~ "{") ~ VarStmts ~ Stmts ~ ReturnStmt <~ "}" ^^ {
      case loc ~ vars ~ body ~ ret => ast.FunBlockStmt(vars, body, ret, loc)
    }

  // ----------------------------------------------------------------------------
  // DECLARATIONS
  // ----------------------------------------------------------------------------

  lazy val IdentifierDecl: Parser[ast.IdentifierDecl] =
    C ~ Id ^^ { case loc ~ name => ast.IdentifierDecl(name, loc) }

  lazy val FunDecl: Parser[ast.FunDecl] =
    C ~ Id ~ ("(" ~> repsep(IdentifierDecl, ",") <~ ")") ~ FunBlockStmt ^^ {
      case loc ~ id ~ params ~ body => ast.FunDecl(id, params, body, loc)
    }

  lazy val Program: Parser[ast.Program] =
    FunDecl.* ^^ { funs =>
      ast.Program(funs, Loc(1, 1))
    }

  // ----------------------------------------------------------------------------
  // PARSER HELPERS
  // ----------------------------------------------------------------------------

  // ignores whitespace, single line comment (// ...) and multiline comments (/* ... */)
  override protected val whiteSpaceRegex: Option[Regex] = Some("""\s*(?>/(/[^\r\n]*|(?s:\*((?!\*/).)*\*/)))\s*|\s+""".r)

  // converts current parser position into the ast.Loc class
  private lazy val C: Parser[ast.Loc] = cursor ^^ (x => Loc(x.line, x.col))

  private def binaryOp(op: ast.BinaryOperator): Parser[(ast.Expr, ast.Expr) => ast.BinaryOp] =
    C <~ op.toString ^^ { loc => (left, right) =>
      ast.BinaryOp(op, left, right, loc)
    }

  private def doParse[T](source: String, parser: Parser[T]): T =
    parseAll(parser, new CharInput(source)) match {
      case Accept(p, _)        => p
      case Reject(msg, rem, _) =>
        // handleWhiteSpace is only called before a parser,
        // not to move the cursor so we can know when to backtrack
        val rem2 = handleWhiteSpace(rem)
        throw ParseException(msg, Loc(rem2.pos.line, rem2.pos.col))
    }

  override def parseProgram(source: String): ast.Program = doParse(source, Program)

  override def parseExpr(source: String): ast.Expr = doParse(source, Expr)

  override def parseStmt(source: String): ast.Stmt = doParse(source, Stmt)
}
