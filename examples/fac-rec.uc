fac(n) {
    var f;

    if (n == 0) {
      f = 1;
    } else {
      f = n * fac(n - 1);
    }

    return f;
}

main() {
    var n;

    n = input;
    output fac(n);

    return 0;
}