/* Tests: string literals (stored in .data section) */
int puts(char* s);

int main() {
    char* msg = "hello";
    /* just verify string literal assignment compiles and runs */
    return 42;
}
