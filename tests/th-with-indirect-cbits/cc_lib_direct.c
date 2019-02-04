int indirect(int);

int direct(int n) {
    return indirect(n) + 1;
}
