int main() {
    int x;
    x = 0;
    int i;
    for (i = 0; i < 5; ++i) {
        ++x;
    }
    int n;
    n = 1;
    while (x) {
        n *= x;
        --x;
    }
    if (x != 120) {
        return 1;
    } else {
        return 0;
    }
}
