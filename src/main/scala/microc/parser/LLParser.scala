package microc.parser

import microc.ast

import scala.annotation.tailrec

/**
  * Tokens recognized by the microC grammar
  */
sealed trait TokenType
object TokenType {

  // delimiters
  case object LPAREN extends TokenType
  case object RPAREN extends TokenType
  case object LBRACE extends TokenType
  case object RBRACE extends TokenType
  case object COMMA extends TokenType
  case object DOT extends TokenType
  case object MINUS extends TokenType
  case object PLUS extends TokenType
  case object COLON extends TokenType
  case object SEMICOLON extends TokenType
  case object SLASH extends TokenType
  case object STAR extends TokenType
  case object EQUAL extends TokenType
  case object EQUAL_EQUAL extends TokenType
  case object GREATER extends TokenType
  case object AND extends TokenType
  case object ERROR extends TokenType
  case object EOI extends TokenType

  // literals
  case object IDENTIFIER extends TokenType
  case object NUMBER extends TokenType

  // keywords
  case object IF extends TokenType
  case object ELSE extends TokenType
  case object NULL extends TokenType
  case object VAR extends TokenType
  case object RETURN extends TokenType
  case object WHILE extends TokenType
  case object ALLOC extends TokenType
  case object INPUT extends TokenType
  case object OUTPUT extends TokenType
}

/**
  * Represents a parsed token.
  *
  * @param typ the type of the token
  * @param value the actual string value
  * @param offset the offset from the beginning of the parse input
  * @param loc the token location
  */
case class Token(typ: TokenType, value: String, loc: ast.Loc)

object Lexer {
  final val EOICh = '\u001a'
}

/**
  * A dead-simple imperative-style stateful lexer.
  *
  * @param input the source code to lex
  */
class Lexer(input: String) {
  import Lexer._
  import TokenType._

  // start of the token
  protected var start = 0
  // offset from the beginning of the input
  protected var offset = 0
  // current line
  protected var line = 1
  // current column
  protected var col = 1
  // result
  protected var tokens: List[Token] = Nil

  /**
    * Lex the source code and return the parsed tokens
    *
    * @return the parsed tokens
    */
  def process(): List[Token] = {
    while (!atTheEnd) {
      start = offset
      scan()
    }
    tokens.reverse
  }

  protected def scan(): Unit = {
    advance() match {
      case ' ' | '\t' | '\r' | '\n'  =>
      case '('                       => addToken(LPAREN)
      case ')'                       => addToken(RPAREN)
      case '{'                       => addToken(LBRACE)
      case '}'                       => addToken(RBRACE)
      case ','                       => addToken(COMMA)
      case '.'                       => addToken(DOT)
      case '-'                       => addToken(MINUS)
      case '+'                       => addToken(PLUS)
      case ':'                       => addToken(COLON)
      case ';'                       => addToken(SEMICOLON)
      case '*'                       => addToken(STAR)
      case '>'                       => addToken(GREATER)
      case '&'                       => addToken(AND)
      case '=' if next('=')          => addToken(EQUAL_EQUAL)
      case '='                       => addToken(EQUAL)
      case '/' if next('*')          => multiLineComment()
      case '/' if next('/')          => singleComment()
      case '/'                       => addToken(SLASH)
      case x if x.isDigit            => number()
      case x if isIdentifierStart(x) => identifier()
      case x                         => addToken(ERROR, x.toString)
    }
  }

  protected def isIdentifierStart(ch: Char): Boolean = ch == '_' || ch.isLetter

  protected def isIdentifierPart(ch: Char): Boolean = ch == '_' || ch.isLetterOrDigit

  protected def identifier(): Unit = {
    while (isIdentifierPart(peek)) advance()

    input.substring(start, offset) match {
      case Keywords.If     => addToken(IF)
      case Keywords.Else   => addToken(ELSE)
      case Keywords.Null   => addToken(NULL)
      case Keywords.Var    => addToken(VAR)
      case Keywords.Return => addToken(RETURN)
      case Keywords.While  => addToken(WHILE)
      case Keywords.Alloc  => addToken(ALLOC)
      case Keywords.Input  => addToken(INPUT)
      case Keywords.Output => addToken(OUTPUT)
      case _               => addToken(IDENTIFIER)
    }
  }

  protected def number(): Unit = {
    while (peek.isDigit) advance()
    addToken(NUMBER)
  }

  protected def multiLineComment(): Unit = {
    while (!atTheEnd && !(next('*') && next('/'))) advance()
  }

  protected def singleComment(): Unit = {
    while (!atTheEnd && !next('\n')) advance()
  }

  // ----------------------------------------------------------------------------
  // LEXER HELPERS
  // ----------------------------------------------------------------------------

  protected def addToken(typ: TokenType, value: String = input.substring(start, offset)): Unit = {
    // the location is always the start of the token
    val loc = ast.Loc(line, col - (offset - start))
    val token = Token(typ, value, loc)
    tokens = token :: tokens
  }

  protected def peek: Char = if (atTheEnd) EOICh else input.charAt(offset)

  protected def next(ch: Char): Boolean = {
    if (atTheEnd) {
      false
    } else if (peek == ch) {
      advance()
      true
    } else {
      false
    }
  }

  protected def advance(): Char = {
    col += 1
    val x = input.charAt(offset)
    if (x == '\n') {
      line += 1
      col = 1
    }
    offset += 1
    x
  }

  protected def atTheEnd: Boolean = offset >= input.length
}

/**
  * A dead-simple LL(k) imperative-style recursive-descent parser for the MicroC grammar.
  *
  * Each public function corresponds to one of the grammar non-terminal.
  *
  * @param source the source code
  * @param tokens the source code tokens
  */
class InternalLLParser(source: String, tokens: List[Token]) {
  import TokenType._

  // ----------------------------------------------------------------------------
  // PARSER STATE
  // ----------------------------------------------------------------------------

  // just seen token
  private var _prev: Token = _
  // the unseen token stream. The `_next.head` is our lookahead
  private var _next: List[Token] = tokens

  // ----------------------------------------------------------------------------
  // EXPRESSIONS
  // ----------------------------------------------------------------------------

  // Identifier = IDENTIFIER
  def Identifier(): ast.Identifier = advance() match {
    case Token(IDENTIFIER, v, l) => ast.Identifier(v, l)
    case x                       => error(s"expected identifier, got '${x.value}'", x)
  }

  // Number = [ '-' ] NUMBER
  def Number(): ast.Number = advance() match {
    case Token(NUMBER, v, l) =>
      ast.Number(v.toInt, l)
    case x if x.typ == MINUS =>
      // we do not have unary minus
      val num = consume(NUMBER, "expected a number")
      ast.Number(-num.value.toInt, x.loc)
    case x =>
      error(s"expected number, got '${x.value}'", x)
  }

  // Field = Id ':' Expr
  def Field(): ast.RecordField = {
    val id = Identifier()
    consume(COLON, "expected ':' after field name")
    val expr = Expr()
    ast.RecordField(id.name, expr, id.loc)
  }

  // Paren = '(' Expr ')'
  def Paren(): ast.Expr = {
    consume(LPAREN, "expected '('")
    val expr = Expr()
    consume(RPAREN, "expected ')' after expression")
    expr
  }

  // Record = '{' [ Field { ',' Field } ] '}'
  def Record(): ast.Record = {
    val op = consume(LBRACE, "Expected '{' for record definition")
    val fields = repsep(Field(), COMMA, RBRACE)
    consume(RBRACE, "Expected '}' after record definition")
    ast.Record(fields, op.loc)
  }

  // PrimaryExpr = Number | Identifier | Record | Paren
  def PrimaryExpr(): ast.Expr = peekType match {
    case MINUS | NUMBER => Number()
    case IDENTIFIER     => Identifier()
    case LPAREN         => Paren()
    case LBRACE         => Record()
    case _ =>
      val x = advance()
      error(s"expected expression, got '${x.value}'", x)
  }

  // PostfixExpr = PrimaryExpr { FieldAccess | Call }
  // FieldAccess = '.' Identifier
  //        Call = '(' [ Expr { ',' Expr } ] ')'
  def PostfixExpr(): ast.Expr = {
    @tailrec def loop(expr: ast.Expr): ast.Expr = peekType match {
      case DOT =>
        val op = advance()
        val id = Identifier()
        loop(ast.FieldAccess(expr, id.name, op.loc))
      case LPAREN =>
        val op = advance()
        val args = repsep(Expr(), COMMA, RPAREN)
        consume(RPAREN, "Expected ')' after call")
        loop(ast.CallFuncExpr(expr, args, op.loc))
      case _ =>
        expr
    }

    loop(PrimaryExpr())
  }

  // UnaryExpr = Deref | Ref | Input | Alloc | Null
  //     Deref = '*' UnaryExpr
  //       Ref = '&' Identifier
  //     Input = 'input'
  //     Alloc = 'alloc' Expr
  //      Null = 'null'
  def UnaryExpr(): ast.Expr = peekType match {
    case STAR =>
      val t = advance()
      ast.Deref(UnaryExpr(), t.loc)
    case AND =>
      val t = advance()
      ast.VarRef(Identifier(), t.loc)
    case INPUT =>
      val t = advance()
      ast.Input(t.loc)
    case ALLOC =>
      val t = advance()
      ast.Alloc(Expr(), t.loc)
    case NULL =>
      val t = advance()
      ast.Null(t.loc)
    case _ => PostfixExpr()
  }

  // MultiplicativeExpr = UnaryExpr { ('*' | '/') UnaryExpr }
  def MultiplicativeExpr(): ast.Expr = binaryOp(UnaryExpr, STAR, SLASH)

  // AdditiveExpr = MultiplicativeExpr { ('+' | '-') MultiplicativeExpr }
  def AdditiveExpr(): ast.Expr = binaryOp(MultiplicativeExpr, PLUS, MINUS)

  // RelationalExpr = AdditiveExpr { '>' AdditiveExpr }
  def RelationalExpr(): ast.Expr = binaryOp(AdditiveExpr, GREATER)

  // RelationalExpr = RelationalExpr { '==' RelationalExpr }
  def EqualityExpr(): ast.Expr = binaryOp(RelationalExpr, EQUAL_EQUAL)

  // Expr = EqualityExpr
  def Expr(): ast.Expr = EqualityExpr()

  // AssignmentStmt = Expr '=' Expr ';'
  def AssignmentStmt(): ast.AssignStmt = {
    val left = Expr()
    val op = consume(EQUAL, "expected '='")
    val right = Expr()
    consume(SEMICOLON, "expected ';' after assignment")
    ast.AssignStmt(left, right, op.loc)
  }

  // OutputStmt = 'output' Expr ';'
  def OutputStmt(): ast.OutputStmt = {
    val op = consume(OUTPUT, s"expected '${Keywords.Output}'")
    val expr = Expr()
    consume(SEMICOLON, "expected ';' after output statement")
    ast.OutputStmt(expr, op.loc)
  }

  // WhileStmt = 'while' '(' Expr ')' Stmt
  def WhileStmt(): ast.WhileStmt = {
    val op = consume(WHILE, s"expected '${Keywords.While}'")
    consume(LPAREN, "expected '(' after while statement")
    val guard = Expr()
    consume(RPAREN, "expected ')' after expression")
    val body = Stmt()
    ast.WhileStmt(guard, body, op.loc)
  }

  // IfStmt = 'if' '(' Expr ')' Stmt [ 'else' Stmt ]
  def IfStmt(): ast.IfStmt = {
    val op = consume(IF, s"expected '${Keywords.If}'")
    consume(LPAREN, "expected '(' after if statement")
    val guard = Expr()
    consume(RPAREN, "expected ')' after expression")
    val thenBranch = Stmt()
    val elseBranch = if (peekType == ELSE) {
      advance()
      Some(Stmt())
    } else {
      None
    }

    ast.IfStmt(guard, thenBranch, elseBranch, op.loc)
  }

  // Stmt = OutputStmt | WhileStmt | IfStmt | '{' { Stmt } '}' | AssignmentStmt
  def Stmt(): ast.StmtInNestedBlock = peekType match {
    case OUTPUT => OutputStmt()
    case WHILE  => WhileStmt()
    case IF     => IfStmt()
    case LBRACE =>
      // it could be a block or an assignment, cf. `{x:1}.x = 1`
      // which is btw incorrect (assignment to rvalue)
      // but that's job of the semantic analysis
      peekType(3) match {
        case COLON =>
          AssignmentStmt()
        case _ =>
          val lbrace = advance()
          val stmts = rep(Stmt(), peekType == RBRACE)
          consume(RBRACE, "expected '}' to close the block")
          ast.NestedBlockStmt(stmts, lbrace.loc)
      }
    case _ => AssignmentStmt()
  }

  // VarStmt = 'var' IdentifierDecl { ',' IdentifierDecl } ';'
  def VarStmt(): ast.VarStmt = {
    val op = consume(VAR, s"expected '${Keywords.Var}'")
    val vars = repsep(IdentifierDecl(), COMMA, SEMICOLON)
    if (vars.isEmpty) error("expected variable declarations", op)
    consume(SEMICOLON, "expected ';' after variable declarations")
    ast.VarStmt(vars, op.loc)
  }

  // ReturnStmt = 'return' Expr ';'
  def ReturnStmt(): ast.ReturnStmt = {
    val op = consume(RETURN, s"expected '${Keywords.Return}'")
    val expr = Expr()
    consume(SEMICOLON, "expected ';' after function return")
    ast.ReturnStmt(expr, op.loc)
  }

  // FunBlockStmt = '{' { VarStmt } { Stmt } ReturnStmt '}'
  def FunBlockStmt(): ast.FunBlockStmt = {
    val lbrace = consume(LBRACE, "expected '{' after function header")
    val vars = rep(VarStmt(), peekType != VAR)
    val body = rep(Stmt(), peekType == RETURN)
    val ret = ReturnStmt()
    consume(RBRACE, "expected '}' after function definition")
    ast.FunBlockStmt(vars, body, ret, lbrace.loc)
  }

  // ----------------------------------------------------------------------------
  // DECLARATIONS
  // ----------------------------------------------------------------------------

  // IdentifierDecl = Identifier
  def IdentifierDecl(): ast.IdentifierDecl = {
    val id = Identifier()
    ast.IdentifierDecl(id.name, id.loc)
  }

  // FunDecl = Identifier '(' [ IdentifierDecl { ',' IdentifierDecl } ] ')' FunBlockStmt
  def FunDecl(): ast.FunDecl = {
    val id = Identifier()
    consume(LPAREN, "expected '(' after function name")
    val params = repsep(IdentifierDecl(), COMMA, RPAREN)
    consume(RPAREN, "expected ')' after function list")
    val body = FunBlockStmt()
    ast.FunDecl(id.name, params, body, id.loc)
  }

  // Program = { FunDecl }
  def Program(): ast.Program = {
    ast.Program(rep(FunDecl(), atTheEnd), ast.Loc(1, 1))
  }

  // ----------------------------------------------------------------------------
  // PARSER HELPERS
  // ----------------------------------------------------------------------------

  protected def binaryOp(operand: () => ast.Expr, operators: TokenType*): ast.Expr = {
    @tailrec def loop(left: ast.Expr): ast.Expr = peekType match {
      case x if operators.contains(x) =>
        val op = advance()
        val right = operand()
        val expr = ast.BinaryOp(ast.BinaryOperator(op.value), left, right, op.loc)
        loop(expr)
      case _ => left
    }

    loop(operand())
  }

  protected def rep[T](elem: => T, until: => Boolean): List[T] = {
    @tailrec def loop(elems: List[T]): List[T] = if (!until) loop(elem :: elems) else elems.reverse

    loop(Nil)
  }

  protected def repsep[T](elem: => T, sep: TokenType, until: TokenType): List[T] = {
    @tailrec def loop(elems: List[T]): List[T] = peekType match {
      case `sep` =>
        advance()
        loop(elem :: elems)
      case _ => elems.reverse
    }

    if (peekType != until) loop(elem :: Nil) else Nil
  }

  protected def advance(): Token = _next match {
    case Nil => _prev
    case x :: xs =>
      _prev = x
      _next = xs
      x
  }

  protected def peekType: TokenType = peek.typ

  protected def peekType(k: Int): TokenType =
    _next.drop(k - 1).headOption.map(_.typ).getOrElse(EOI)

  protected def peek: Token = {
    if (atTheEnd) _prev
    else _next.head
  }

  protected def consume(value: TokenType, msg: String): Token = peekType match {
    case `value` => advance()
    case _       => error(msg, peek)
  }

  protected def atTheEnd: Boolean = _next.isEmpty

  protected def error(msg: String, token: Token): Nothing = {
    throw ParseException(msg, token.loc)
  }
}

class LLParser extends microc.parser.Parser {

  private def doParse[T](source: String, p: InternalLLParser => T): T = {
    val lexer = new Lexer(source)
    val parser = new InternalLLParser(source, lexer.process())
    p(parser)
  }

  override def parseProgram(source: String): ast.Program = doParse(source, _.Program())

  override def parseExpr(source: String): ast.Expr = doParse(source, _.Expr())

  override def parseStmt(source: String): ast.Stmt = doParse(source, _.Stmt())
}
