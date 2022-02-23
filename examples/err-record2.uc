main() {
    var n, k;

    k = {a: 1, b: 2};
    n = alloc {c: &k, d: 4};
    output (*(*n).c).c; // error: missing field

    return 0;
}