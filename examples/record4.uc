main() {
    var n, k, r1;
    k = {a: 1, b: 2};
    n = {c: &k, d: 4};
    r1 = (*(n.c).a);
    output r1;// output 1
    return r1;
}