fib(n){
    var f1,f2,i;
    var temp;
    f1=1;
    f2=1;
    
    i=n;
    while( i>1 ){
        temp = f1+f2;
        f1=f2;
        f2=temp;
        i=i-1;
    }
    return f2;
}

main(){
    var n;
    n=input;
    output fib(n);
    return 0;
}