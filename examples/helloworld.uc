main() {
    var c0,c1,c2,c3,c4,c5,c6;

    c0 = 8; c2 = 0; c3 = 0; c4 = 0; c5 = 0; c6 = 0;

    while(c0) {
        c1 = 4;
        while(c1) {
            c2 = c2 + 2;
            c3 = c3 + 3;
            c4 = c4 + 3;
            c5 = c5 + 1;
            c1 = c1 - 1;
        }
        c2 = c2 + 1;
        c3 = c3 + 1;
        c4 = c4 - 1;
        c6 = c6 + 1;
        c0 = c0 - 1;
    }

    output c2;
    c3 = c3 - 3;
    output c3;
    c3 = c3 + 7;
    output c3;
    output c3;
    c3 = c3 + 3;
    output c3;
    output c5;
    c4 = c4 - 1;
    output c4;
    output c3;
    c3 = c3 + 3;
    output c3;
    c3 = c3 - 6;
    output c3;
    c3 = c3 - 8;
    output c3;
    output c5 + 1;
    output c6 + 2;

    return 0;
}