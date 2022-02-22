main() {
    var funalias;
    // error should be here
    funalias = &main;
    *funalias = 5;
    return 0;
}