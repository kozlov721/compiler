int puts(char *str);
int printf(char *str, int a, int b);
int getchar();
int isdigit(char c);

int factorial(int n) {
    int res = 1;
    while (n) {
        res = res * n;
        n = n - 1;
    }
    return res;
}

int read_num() {
    int n = 0;
    int c = getchar();
    while (isdigit(c)) {
        n = n * 10;
        n = n + c - '0';
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
