/* Tests: while loop */
int main() {
    int i = 0;
    int sum = 0;

    /* sum 1..9 = 45; then subtract 3 → 42 */
    while (i < 9) {
        ++i;
        sum = sum + i;
    }
    sum = sum - 3;
    if (sum != 42) return 1;

    return 42;
}
