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
    assertEquals(ds(Identifier("x", Loc(4, 4))), IdentifierDecl("x", Loc(3, 8)))
    assertEquals(ds(Identifier("x", Loc(5, 11))), IdentifierDecl("x", Loc(3, 8)))
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
      Left(List(SemanticError("identifier 'f' already declared (2:2)", Loc(6, 2))))
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
      Left(List(SemanticError("identifier 'f' already declared (2:2)", Loc(7, 8))))
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
      Left(List(SemanticError("identifier 'x' already declared (3:8)", Loc(3, 12))))
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
      Left(List(SemanticError("identifier 'x' already declared (2:4)", Loc(3, 10))))
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
      Left(List(SemanticError("identifier 'y' not declared", Loc(4, 4))))
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
      Left(List(SemanticError("duplicate field name 'a'", Loc(4, 21))))
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
      Left(List(SemanticError("cannot assign to a function", Loc(4, 6))))
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
      Left(List(SemanticError("cannot assign into rvalue 3[3:4]", Loc(3, 6))))
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
      Left(List(SemanticError("cannot assign into rvalue {x:1}.x[3:9]", Loc(3, 12))))
    )
  }

  // ------------------------------------------------------------------
  // HELPERS
  // ------------------------------------------------------------------

  def analyze(code: String): Either[List[SemanticError], Declarations] = try {
    val analysis = new SemanticAnalysis()
    val declarations = analysis.analyze(parseUnsafe(code))
    Right(declarations)
  } catch {
    case e: SemanticException => Left(e.errors)
  }
}
