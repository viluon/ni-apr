fib(n) {
    var r;
    r = n;
    if (n > 1) {
        r = fib(n - 2) + fib(n - 1);
    }
    return r;
}

main() {
    var n, r;
    n = input;
    if (n > -1) {
        output fib(n);
        r = 0;
    } else {
        r = 1;
    }
    return r;
}