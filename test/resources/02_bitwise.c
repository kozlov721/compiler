/* Tests: &, |, ^, ~, >>, << */
int main() {
    int a = 0xFF & 0x2A;  /* 42: AND */
    if (a != 42) return 1;

    int b = 0x20 | 0x0A;  /* 42: OR */
    if (b != 42) return 1;

    int c = 0x1F ^ 0x35;  /* 42: XOR */
    if (c != 42) return 1;

    /* NOT: ~~x == x for any x */
    int d = 42;
    if (~~d != 42) return 1;
    /* NOT changes bits: x & ~x == 0 */
    if ((d & ~d) != 0) return 1;

    int e = 168 >> 2;     /* 42: right shift (168/4) */
    if (e != 42) return 1;

    int f = 21 << 1;      /* 42: left shift (21*2) */
    if (f != 42) return 1;

    return 42;
}
