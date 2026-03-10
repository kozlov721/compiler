/* Tests: +, -, *, /, % */
int main() {
    int a = 10 + 4;   /* 14 */
    int b = a * 3;    /* 42 */
    int c = b / 1;    /* 42 */
    int d = c - 0;    /* 42 */
    int e = d % 100;  /* 42 */
    return e;
}
