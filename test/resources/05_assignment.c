/* Tests: +=, -=, *=, /=, %=, &=, |=, ^=, >>=, <<= */
int main() {
    int x = 5;
    x += 3;   /* 8  */
    x *= 6;   /* 48 */
    x -= 6;   /* 42 */
    if (x != 42) return 1;

    x = 90;
    x /= 2;   /* 45 */
    x %= 43;  /* 2  */
    if (x != 2) return 1;

    x = 0xFF;
    x &= 0x2A; /* 42 */
    if (x != 42) return 1;

    x = 32;
    x |= 10;   /* 42 */
    if (x != 42) return 1;

    x = 31;
    x ^= 53;   /* 42 */
    if (x != 42) return 1;

    x = 336;
    x >>= 3;   /* 42 */
    if (x != 42) return 1;

    x = 21;
    x <<= 1;   /* 42 */
    if (x != 42) return 1;

    return 42;
}
