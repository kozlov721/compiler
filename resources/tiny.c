int factorial(int n) {
    int res = 1;
    while (n) {
        res = res * n;
        n = n - 1;
    }
    return res;
}

int main() {
    return factorial(5);
}
