f() {
    return 17;
}

g() {
    return 17;
}

main(){
    var n;

    if (f > g) { // error: function comparison
        n = 10;
    }

    return 0;
}