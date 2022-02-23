f(){
    return 5;
}

g(){
    return 5;
}

main(){
    f = g; // error: assignment to a function

    return 0;
}