/* Tests: prefix ++ and -- */
int main() {
    int x = 40;
    ++x;       /* 41 */
    ++x;       /* 42 */
    if (x != 42) return 1;

    int y = 44;
    --y;       /* 43 */
    --y;       /* 42 */
    if (y != 42) return 1;

    return 42;
}
