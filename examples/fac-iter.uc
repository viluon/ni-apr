fac(n){
    var f;

    f = 1;
    while (n>0) {
        f = f * n;
        n = n - 1;
    }

    return f;
}

main() {
    var n;

    n = input;
    output fac(n);

    return 0;
}