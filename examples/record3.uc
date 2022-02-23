main() {
    var n, k, r1;

    k = {f: 4};
    n = alloc {p: 4, q: 5};
    *n = {p: 44, q: &k};
    r1 = (*(*n).q).f;
    output r1; // outputs 4

    return 0;
}