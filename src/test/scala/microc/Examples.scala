package microc

trait Examples {
  val ExampleMapInc: String = """
     | not_end(l) {
     |   var x;
     |   if (l == null) { 
     |     x=0; 
     |   } else { 
     |     x=1; 
     |   }
     |   return x;
     | }
     | 
     | map(l, f) {
     |   var xs,g,h;
     |   xs=l;
     |   
     |   while (not_end(xs)) {
     |     h=(*xs).h;
     |     h=f(h);
     |     (*xs).h=h;
     |     output (*xs).h;
     |     xs=(*xs).t;
     |   }
     |   
     |   return null;
     | }
     |
     | foo(x) {
     |   return x+1;
     | }
     |
     | main() {
     |   var h,t,n,z;
     |   t = null;
     |   n = 42;
     |   
     |   while (n>0) {
     |     n = n-1;
     |     h = alloc {h: n, t: t};
     |     t = h;
     |   }
     |   
     |   z = map(h,foo);
     |   
     |   return 0;
     | }
     |""".stripMargin

  val ExampleRecursiveFactorial: String = """
      | fac(n) {
      |   var x;
      |   if (n == 0) {
      |     x = 1;
      |   } else {
      |     x = n * fac(n - 1);
      |   }
      |   return x;
      | }
      |
      | main() {
      |   return fac(5);
      | }
      |""".stripMargin

  val ExampleTypeCheckingValid: String = """
      | f() {
      |  var x, y, z;
      |  x = input;
      |  y = alloc x;
      |  *y = x;
      |  z = *y;
      |  return z;
      | }
      |""".stripMargin

  val ExampleTypeCheckingRecords: String = """
      | main() {
      |  var a,b,c,d;
      |
      |  a = {x: 1};
      |  b = {x: &a};
      |  c = {x: main};
      |  d = {x: 1, y: &b};
      |
      |  return 0;
      | }
      |""".stripMargin

  val ExampleTypeCheckingComplex: String = """
      | list_append(list, x) {
      |  var node;
      |  node = alloc {v: x, prev: list, next: null};
      |  if (list == null) {
      |  } else {
      |      (*list).next = node;
      |  }
      |
      |   return node;
      | }
      |
      | main(n) {
      |   var list;
      |   list = null;
      |   list = list_append(list, 1);
      |   return 0;
      | }
      |""".stripMargin
}

object Examples extends Examples
