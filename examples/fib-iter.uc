fib(n){
    var f1,f2,i;
    var temp;

    f1 = 0;
    f2 = 1;
    
    i = n;
    while( i>0 ){
        temp = f2;
        f2 = f1 + f2;
        f1 = temp;
        i = i - 1;
    }

    return f1;
}

main() {
    var n;

    n = input;
    output fib(n);

    return 0;
}
