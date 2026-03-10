/* Tests: for loop */
int main() {
    int sum = 0;
    int i = 0;

    /* sum 1..9 = 45; subtract 3 → 42 */
    for (i = 1; i <= 9; ++i) {
        sum = sum + i;
    }
    sum = sum - 3;
    if (sum != 42) return 1;

    return 42;
}
