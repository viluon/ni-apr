package microc.parser

import microc.ast._
import microc.cli.Reporter
import munit.{FunSuite, Location}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

abstract class AbstractParserTest extends FunSuite {

  def parser: Parser

  check[Expr]("1 + 2 - 3 + 4 * 5", "(((1 + 2) - 3) + (4 * 5))[1:1-1:17]")

  check[Expr]("42// 1 + 1", "42[1:1-1:2]")

  check[Expr]("-42", "-42[1:1-1:3]")

  check[Expr]("""|/*
       | comment
       | */ 42
       |""".stripMargin, "42[3:5-3:6]")

  check[Expr]("1 + 2 // abc", "(1 + 2)[1:1-1:5]")

  check[Expr]("1 + /* 2 */ 3 // abc", "(1 + 3)[1:1-1:13]")

  check[Stmt]("*f.a.g = 1;", "(*f.a.g) = 1;[1:1-1:11]")

  check[Expr]("*f.a.g", "(*f.a.g)[1:1-1:6]")

  check[Expr]("(*f).a.g", "(*f).a.g[1:1-1:8]")

  check[Expr]("2 * (3 + 4) > 1 == 0") {
    case Success(v) =>
      assertEquals(v.toString, "(((2 * (3 + 4)) > 1) == 0)[1:1-1:20]")
      assertEquals(v.span.highlight, Some(Span(Loc(1, 17), Loc(1, 18))))
  }

  check[Expr]("{x: 1, y: x}.z", "{x:1,y:x}.z[1:1-1:14]")

  check[Expr]("f(x)", "f(x)[1:1-1:4]")

  check[Expr]("{x: f}.x(y)", "{x:f}.x(y)[1:1-1:11]")

  check[Expr]("alloc null", "alloc null[1:1-1:10]")

  check[Expr]("alloc {x: 1}", "alloc {x:1}[1:1-1:12]")

  check[Expr]("alloc 1+2*2", "alloc (1 + (2 * 2))[1:1-1:11]")

  check[Expr]("(*alloc {x: 1}).x", "(*alloc {x:1}).x[1:1-1:17]")

  check[Stmt]("{ x:1 }.x = 2;", "{x:1}.x = 2;[1:1-1:14]")

  check[Stmt]("output { x:1 }.x;", "output {x:1}.x;[1:1-1:17]")

  check[Stmt]("{ { x:1 }.x = 1; }", "{\n  {x:1}.x = 1;\n}[1:1-1:18]")

  check[Stmt]("while (x>0) {}", "while ((x > 0)) {}[1:1-1:14]")

  check[Stmt]("while (x>0) while (y) output x+y;", "while ((x > 0)) while (y) output (x + y);[1:1-1:33]")

  check[Stmt]("if (x) output 1; else output 2;", "if (x) output 1; else output 2;[1:1-1:31]")

  check[Stmt]("if (a) if (b) c = 1; else d = 2; else e = 3;") {
    case Success(v) =>
      assertEquals(
        v,
        IfStmt(
          Identifier("a", Span(Loc(1, 5), Loc(1, 5))),
          IfStmt(
            Identifier("b", Span(Loc(1, 12), Loc(1, 12))),
            AssignStmt(Identifier("c", Span(Loc(1, 15), Loc(1, 15))), Number(1, Span(Loc(1, 19), Loc(1, 19))), Span(Loc(1, 15), Loc(1, 20))),
            Some(
              AssignStmt(Identifier("d", Span(Loc(1, 27), Loc(1, 27))), Number(2, Span(Loc(1, 31), Loc(1, 31))), Span(Loc(1, 27), Loc(1, 32))),
            ),
            Span(Loc(1, 8), Loc(1, 32))
          ),
          Some(
            AssignStmt(Identifier("e", Span(Loc(1, 39), Loc(1, 39))), Number(3, Span(Loc(1, 43), Loc(1, 43))), Span(Loc(1, 39), Loc(1, 44)))
          ),
          Span(Loc(1, 1), Loc(1, 44))
        )
      )
  }

  check[Stmt]("{{}}") {
    case Success(v) =>
      assertEquals(
        v,
        NestedBlockStmt(List(NestedBlockStmt(Nil, Span(Loc(1, 2), Loc(1, 3)))), Span(Loc(1, 1), Loc(1, 4)))
      )
  }

  check[Stmt]("{{{}{}}}") {
    case Success(v) =>
      assertEquals(
        v,
        NestedBlockStmt(
          List(
            NestedBlockStmt(
              List(
                NestedBlockStmt(Nil, Span(Loc(1, 3), Loc(1, 4))),
                NestedBlockStmt(Nil, Span(Loc(1, 5), Loc(1, 6)))
              ),
              Span(Loc(1, 2), Loc(1, 7))
            )
          ),
          Span(Loc(1, 1), Loc(1, 8))
        )
      )
  }

  check[Stmt]("{{x = 1;}{y = 2;}}", "{\n  {\n    x = 1;\n  }\n  {\n    y = 2;\n  }\n}[1:1-1:18]")

  check[Program](
    """|f() {
       |  return 1;
       |}
       |""".stripMargin,
    "f() {\n  return 1;\n}[1:1-3:1]"
  )

  check[Program](
    """|f() {
       |  var x;
       |  return 1;
       |}
       |""".stripMargin,
    "f() {\n  var x;\n  return 1;\n}[1:1-4:1]"
  )

  check[Program](
    """|f() {
       |  var x, y;
       |  var z;
       |  return 1;
       |}
       |""".stripMargin,
    "f() {\n  var x,y;\n  var z;\n  return 1;\n}[1:1-5:1]"
  )

  check[Program](
    """|f() {
       |  var x;
       |  {}
       |  return 1;
       |}
       |""".stripMargin,
    "f() {\n  var x;\n  {}\n  return 1;\n}[1:1-5:1]"
  )

  test("associativity and position of arithmetic operations") {
    val actual = parser.parseExpr("1 + 2 * 3 * 4 + 5")
    val expected =
      BinaryOp(
        Plus,
        BinaryOp(
          Plus,
          Number(1, Span(Loc(1, 1), Loc(1, 1))),
          BinaryOp(
            Times,
            BinaryOp(
              Times,
              Number(2, Span(Loc(1, 5), Loc(1, 5))),
              Number(3, Span(Loc(1, 9), Loc(1, 9))),
              Span(Loc(1, 5), Loc(1, 9), Some(Span(Loc(1, 7), Loc(1, 7))))
            ),
            Number(4, Span(Loc(1, 13), Loc(1, 13))),
            Span(Loc(1, 5), Loc(1, 13), Some(Span(Loc(1, 11), Loc(1, 11))))
          ),
          Span(Loc(1, 1), Loc(1, 13), Some(Span(Loc(1, 3), Loc(1, 3))))
        ),
        Number(5, Span(Loc(1, 17), Loc(1, 17))),
        Span(Loc(1, 1), Loc(1, 17), Some(Span(Loc(1, 15), Loc(1, 15))))
      )

    assertEquals(actual, expected)
  }

  // ----------------------------------------------------------------------------
  // HELPERS
  // ----------------------------------------------------------------------------

  protected def check[T <: AstNode](
      code: String
  )(matcher: PartialFunction[Try[T], Unit])(implicit loc: Location, tag: ClassTag[T]): Unit = {
    test(s"parse: $code") {
      val f =
        if (classOf[Expr].isAssignableFrom(tag.runtimeClass)) parser.parseExpr _
        else if (classOf[Stmt].isAssignableFrom(tag.runtimeClass)) parser.parseStmt _
        else if (classOf[Program].isAssignableFrom(tag.runtimeClass)) parser.parseProgram _
        else throw new IllegalArgumentException("Only Expr, Stmt or Program are possible")

      matcher(Try(f(code).asInstanceOf[T]))
    }
  }

  protected def check[T <: AstNode](code: String, expected: String)(implicit loc: Location, tag: ClassTag[T]): Unit = {
    check[T](code) {
      case Success(value) => assertEquals(value.toString, expected)
      case Failure(e: ParseException) =>
        fail(s"Failed to check '${e.format(new Reporter(code))}'")
    }
  }

  protected def checkFail[T <: AstNode](code: String, expected: ParseException)(implicit loc: Location,
                                                                                tag: ClassTag[T]): Unit = {
    check[T](code) {
      case Success(_) => fail(s"Code '$code' should not parse")
      case Failure(e: ParseException) => assertEquals(e, expected)
    }
  }
}
