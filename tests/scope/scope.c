// mini-c program to test if scoping correctly works

int global;

int scope() {   
    int n;

    global = 1000;
    n = 1000;

    {
        int n;
        int global;
        global = 1;
        n = 2;
    }

    return global + n;
}
