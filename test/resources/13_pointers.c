/* Tests: pointers (&, *) */
int main() {
    int x = 42;
    int* p = &x;

    /* dereference */
    if (*p != 42) return 1;

    /* assign through pointer */
    *p = 100;
    if (x != 100) return 1;

    x = 42;
    return *p;
}
