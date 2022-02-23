loop(a,b,n) {
  var r;

  if (n == 0) {
    r = a;
  } else {
    if (n > 0) {
      if (n == 1) {
        r = b;
      } else {
        r = loop(b, a + b, n - 1);
      }
    } else {
      r = -1;
    }
  }

  return r;
}

fib(n) {
    var f, r;

    f = loop(0, 1, n);
    if (f == -1) {
        r = {r: 1, f: f};
    } else {
        r = {r: 0, f: f};
    }

    return r;
}

main() {
    var n, f, r;
    n = input;
    f = fib(n);
    if (f.r == 0) {
      output f.f;
    }
    return f.r;
}