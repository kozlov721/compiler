/* Tests: break and continue */
int main() {
    /* break test: sum 0..9 = 45 */
    int i = 0;
    int sum = 0;
    while (1) {
        if (i >= 10) break;
        sum = sum + i;
        ++i;
    }
    if (sum != 45) return 1;

    /* continue test: skip odd values, sum even numbers 0..10 = 30 */
    i = 0;
    sum = 0;
    while (i <= 10) {
        if (i % 2 != 0) {
            ++i;
            continue;
        }
        sum = sum + i;
        ++i;
    }
    if (sum != 30) return 1;

    return 42;
}
