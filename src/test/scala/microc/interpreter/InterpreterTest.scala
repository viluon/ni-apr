package microc.interpreter

import microc.analysis.SemanticException
import microc.ast.{Loc, Span}
import microc.cli.Reporter
import microc.util.CharacterSets.NL
import microc.{Examples, Parsing}
import munit.FunSuite

import java.io.{StringReader, StringWriter}
import scala.annotation.tailrec

class InterpreterTest extends FunSuite with Parsing with Examples {
  test("basic") {
    assertEquals(
      run("main() { return 1; }"),
      Right(Result(1))
    )
  }

  test("basic expression") {
    assertEquals(
      run("main() { return 1+1; }"),
      Right(Result(2))
    )
  }

  test("return variable") {
    assertEquals(
      run("main() { var x; x = 42; return x; }"),
      Right(Result(42))
    )
  }

  test("return variable expression") {
    assertEquals(
      run("main() { var x, y; x = 43; y = 1; return x-y; }"),
      Right(Result(42))
    )
  }

  test("number comparison") {
    assertEquals(
      run("main() { var x, y; x = 1; y = 1; return x == y; }"),
      Right(Result(1))
    )
  }

  test("pointer comparison") {
    assertEquals(
      run("main() { var x; x = null; return x == null; }"),
      Right(Result(1))
    )
  }

  test("pointer comparison 2") {
    assertEquals(
      run("main() { var x; x = alloc null; return *x == null; }"),
      Right(Result(1))
    )
  }

  test("pointer comparison 3") {
    assertEquals(
      run("main() { var x, y; x = alloc null; y = x; return x == y; }"),
      Right(Result(1))
    )
  }

  test("nested block") {
    assertEquals(
      run(
        """
          | main() {
          |   var x;
          |   x = 1;
          |   {
          |     x = x + 41;
          |   }
          |   return x;
          | }
          |""".stripMargin),
      Right(Result(42))
    )
  }

  test("basic pointer") {
    assertEquals(
      run(
        """
          | main() {
          |   var x, y;
          |   x = 43;
          |   y = &x;
          |   *y = *y - 1;
          |   return x;
          | }
          |""".stripMargin),
      Right(Result(42))
    )
  }

  test("double pointer") {
    assertEquals(
      run(
        """
          | main() {
          |   var x, y, z;
          |   x = 43;
          |   y = &x;
          |   z = &y;
          |   **z = *y - 1;
          |   return x;
          | }
          |""".stripMargin),
      Right(Result(42))
    )
  }

  test("function calls") {
    assertEquals(
      run(
        """
          | g(x) {
          |   return x + 1;
          | }
          |
          | f(x, y) {
          |   return g(x+y);
          | }
          |
          | main() {
          |   return f(21, 20);
          | }
          |""".stripMargin),
      Right(Result(42))
    )
  }

  test("basic records") {
    assertEquals(
      run(
        """
          | main() {
          |   var r;
          |   r = { x: 1, y: 41};
          |   return r.x + r.y;
          | }
          |""".stripMargin),
      Right(Result(42))
    )
  }

  test("nested records") {
    assertEquals(
      run(
        """
          | main() {
          |   var r, s;
          |   r = { a: 1 };
          |   s = { a: &r, b: 2 };
          |   (*(s.a)).a = 40 + s.b;
          |   return r.a;
          | }
          |""".stripMargin),
      Right(Result(42))
    )
  }

  test("missing fields") {
    assertEquals(
      run(
        """
          | main() {
          |   var r;
          |   r = { a: 1 };
          |   r.b = 1;
          |   return r.b;
          | }
          |""".stripMargin),
      Left(ExecutionException("cannot assign to field b of record r[5:4-5:4]", Span(Loc(5, 4), Loc(5, 6), Some(Span(Loc(5, 5), Loc(5, 6))))))
    )
  }

  test("no nested records") {
    assertEquals(
      run(
        """
          | main() {
          |   var r;
          |   r = { a: 1, b: {x: 1} };
          |   return 0;
          | }
          |""".stripMargin
      )
      , Left(ExecutionException("nested records are not supported, use pointers", Span(Loc(4, 16), Loc(4, 24), Some(Span(Loc(4, 19), Loc(4, 24))))))
    )
  }

  test("assign to records") {
    assertEquals(
      run(
        """
          | main() {
          |   var r,s;
          |   r = { a: null, b: null };
          |   s = { c: 1 };
          |   r.a = &s;
          |   r.b = s;
          |   s.c = 2;
          |   return (*(r.a)).c + r.b.c;
          | }
          |""".stripMargin),
      Right(Result(3))
    )
  }

  test("records with functions") {
    assertEquals(
      run(
        """
          | inc(s) {
          |   (*s).c = (*s).c + 1;
          |   return *s;
          | }
          |
          | new() {
          |   return { c: 0, inc: inc };
          | }
          |
          | main() {
          |   var c, p;
          |   c = new();
          |   p = &c;
          |   return ((((c.inc)(p)).inc)(p)).c;
          | }
          |""".stripMargin),
      Right(Result(2))
    )
  }

  test("I/O") {
    assertEquals(
      run(
        """
          | main() {
          |   var x, y;
          |   x = input;
          |   y = input;
          |   output x + y;
          |   return 0;
          | }
          |""".stripMargin,
        input = "1" + NL + "2" + NL
      ),
      Right(Result(0, "3"))
    )
  }

  test("unnecessary complicated function") {
    assertEquals(
      run(
        """
          | foo(p,x) {
          |   var f,q;
          |   if (*p == 0) {
          |     f = 1;
          |   } else {
          |     q = alloc 0;
          |     *q = (*p)-1;
          |     f = (*p)*(x(q,x));
          |   }
          |   return f;
          | }
          |
          | main() {
          |   var n;
          |   n = input;
          |   return foo(&n, foo);
          |  }
          |""".stripMargin,
        "5" + NL
      ),
      Right(Result(120))
    )
  }

  test("while") {
    assertEquals(
      run(
        """
          | fac(n) {
          |   var x;
          |   x = 1;
          |   while (n > 0) {
          |     x = x * n;
          |     n = n - 1;
          |   }
          |
          |   return x;
          | }
          |
          | main() {
          |   return fac(5);
          | }
          |""".stripMargin),
      Right(Result(120))
    )
  }

  test("alloc") {
    assertEquals(
      run(
        """
          | main() {
          |   var x,y,z;
          |   x = alloc 1;
          |   y = &x;
          |   z = &y;
          |   ***z = **y + *x;
          |   return *x;
          | }
          |""".stripMargin
      ),
      Right(Result(2))
    )
  }

  test("main with params") {
    assertEquals(
      run(
        """
          | main(x,y) {
          |   return x + y;
          | }
          |""".stripMargin,
        mainArgs = List(1, 41)
      ),
      Right(Result(42))
    )
  }

  test("factorial") {
    assertEquals(run(ExampleRecursiveFactorial), Right(Result(120)))
  }

  test("map") {
    assertEquals(run(ExampleMapInc), Right(Result(0, (1 to 42).mkString("\n"))))
  }

  // ------------------------------------------------------------------
  // HELPERS
  // ------------------------------------------------------------------

  case class Result(ret: Int, stdout: String = "", stdin: String = "")

  def run(code: String, input: String = "", mainArgs: List[Int] = Nil): Either[ExecutionException, Result] = {
    val stdout = new StringWriter()
    val stdin = new StringReader(input)
    val program = parseUnsafe(code)
    val reporter = new Reporter(code)
    val interpreter = try {
      Interpreter(program, stdin, stdout)
    } catch {
      case e: SemanticException =>
        println(e.format(reporter))
        throw e
    }

    @tailrec def readStdin(acc: StringBuilder = new StringBuilder): String = {
      val c = stdin.read()
      if (c != -1) readStdin(acc.append(c))
      else acc.toString()
    }

    try {
      val status = interpreter.run(mainArgs)
      val unconsumedStdin = readStdin()
      Right(Result(status, stdout.toString.trim, unconsumedStdin))
    } catch {
      case e: ExecutionException =>
        println(e.format(reporter))
        Left(e)
    }
  }
}
