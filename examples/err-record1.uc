main() {
    var n, k;

    k = {a: 1, b: 2};
    n = alloc {c: k, d: 4}; // error: nested records are not allowed

    return 0;
}