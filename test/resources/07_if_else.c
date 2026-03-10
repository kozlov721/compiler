/* Tests: if / else */
int main() {
    int x = 0;

    if (1) {
        x = 42;
    } else {
        x = 1;
    }
    if (x != 42) return 1;

    if (0) {
        x = 1;
    } else {
        x = 42;
    }
    if (x != 42) return 1;

    /* nested if/else */
    int a = 10;
    int b = 20;
    if (a > b) {
        x = 1;
    } else if (a == b) {
        x = 1;
    } else {
        x = 42;
    }
    if (x != 42) return 1;

    return 42;
}
