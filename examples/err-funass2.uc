main() {
    var f;

    f = &main; // error: taking address of a function
    *f = 5;

    return 0;
}