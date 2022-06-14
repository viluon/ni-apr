package microc.analysis

import microc.Parsing
import microc.ast._
import munit.FunSuite

class SemanticAnalysisTest extends FunSuite with Parsing {
  test("Basic test") {
    val Right(ds) = analyze(
      """
        | main() {
        |   var x;
        |   x = 42;
        |   return x;
        | }
        |""".stripMargin
    )

    assertEquals(ds.size, 2)
    assertEquals(ds(Identifier("x", Span(Loc(4, 4), Loc(4, 4))))._2, IdentifierDecl("x", Span(Loc(3, 8), Loc(3, 8))))
    assertEquals(ds(Identifier("x", Span(Loc(5, 11), Loc(5, 11))))._2, IdentifierDecl("x", Span(Loc(3, 8), Loc(3, 8))))
  }

  test("Identifier redefinition (function, function)") {
    assertEquals(analyze(
      """
        | f() {
        |   return 1;
        | }
        |
        | f() {
        |   return 2;
        | }
        |""".stripMargin),
      Left(List(SemanticError("redeclaration of f, previously declared at 2:2-4:2", Span(Loc(6, 2), Loc(8, 2)))))
    )
  }

  test("Identifier redefinition (function, variable)") {
    assertEquals(
      analyze(
        """
          | f() {
          |   return 1;
          | }
          |
          | g() {
          |   var f;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("redeclaration of f, previously declared at 2:2-4:2", Span(Loc(7, 8), Loc(7, 8)))))
    )
  }

  test("Identifier redefinition (variable, variable)") {
    assertEquals(
      analyze(
        """
          | f() {
          |   var x,y,x;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("redeclaration of x, previously declared at 3:8-3:8", Span(Loc(3, 12), Loc(3, 12)))))
    )
  }

  test("Identifier redefinition (param, variable)") {
    assertEquals(
      analyze(
        """
          | f(x) {
          |   var y,x;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("redeclaration of x, previously declared at 2:4-2:4", Span(Loc(3, 10), Loc(3, 10)))))
    )
  }

  test("Uninitialized variable") {
    assertEquals(
      analyze(
        """
          | f() {
          |   var x;
          |   y = x;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("undefined reference to y", Span(Loc(4, 4), Loc(4, 4)))))
    )
  }

  test("Duplicate field") {
    assertEquals(
      analyze(
        """
          | f() {
          |   var x;
          |   x = {a: 1, b: 2, a: 1};
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("field a already exists at 4:9-4:12", Span(Loc(4, 21), Loc(4, 24)))))
    )
  }

  test("Assign to a function") {
    assertEquals(
      analyze(
        """
          | f() {
          |   var x;
          |   f = x;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("cannot assign to a function", Span(Loc(4, 4), Loc(4, 9), Some(Span(Loc(4, 4), Loc(4, 4)))))))
    )
  }

  test("Assign to rvalue (int)") {
    assertEquals(
      analyze(
        """
          | f() {
          |   3 = 3;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError("cannot assign to rvalue 3[3:4-3:4]", Span(Loc(3, 4), Loc(3, 9), Some(Span(Loc(3, 4), Loc(3, 4)))))))
    )
  }

  test("Assign to rvalue (record)") {
    assertEquals(
      analyze(
        """
          | f() {
          |   {x:1}.x = 3;
          |   return 1;
          | }
          |""".stripMargin),
      Left(List(SemanticError(
        "cannot assign to rvalue {x:1}.x[3:4-3:10]",
        Span(Loc(3, 4), Loc(3, 15), Some(Span(Loc(3, 4), Loc(3, 10), Some(Span(Loc(3, 9), Loc(3, 10))))))
      )))
    )
  }

  // ------------------------------------------------------------------
  // HELPERS
  // ------------------------------------------------------------------

  def analyze(code: String): Either[List[SemanticError], Declarations] = try {
    val analysis = new SemanticAnalysis()
    val (declarations, _) = analysis.analyze(parseUnsafe(code))
    Right(declarations)
  } catch {
    case e: SemanticException => Left(e.errors)
  }
}
