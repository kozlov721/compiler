/* Tests: function declarations and definitions (up to 6 args) */

int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

int add5(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
}

int main() {
    int x = add(20, 22);       /* 42 */
    if (x != 42) return 1;

    int y = multiply(6, 7);    /* 42 */
    if (y != 42) return 1;

    int z = add5(1, 2, 3, 16, 20); /* 42 */
    if (z != 42) return 1;

    return 42;
}
