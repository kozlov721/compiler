// so the compiler won't complain about undeclared functions
int puts(char *);
int printf(char *, int, int);
int getchar();
int isdigit(char);

int factorial(int n) {
    int res = 1;
    while (n) {
        res *= n;
        --n;
    }
    return res;
}

int read_num() {
    int n = 0;
    int c = getchar();
    while (isdigit(c)) {
        n *= 10;
        n += c - '0';
        c = getchar();
    }
    return n;
}

int main() {
    puts("Input a number");
    printf("> ", 0, 0);
    int n = read_num();
    int x = factorial(n);
    printf("The factorial of %d is %d\n", n, factorial(n));
    return 0;
}
