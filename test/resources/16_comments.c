/* Tests: // line comments and block comments */
/* This is a
   multi-line block comment */
int main() {
    int x = 10; // set to 10
    // x = 999; (commented out, should not execute)
    x = x + 32; /* add 32 */
    /* x = 1; */ // double-commented: neither should execute
    return x; /* return 42 */
}
