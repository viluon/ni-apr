main() {
    var n, r1;

    n = alloc {p: 4, q: 2};
    *n = {p:5, q: 6};
    r1 = (*n).p;
    output r1; // outputs 5

    return 0;
}