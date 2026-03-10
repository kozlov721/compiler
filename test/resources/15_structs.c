/* Tests: struct declarations */
struct Point {
    int x;
    int y;
};

int main() {
    /* just verify struct declaration and variable allocation work */
    struct Point p;
    int x = 42;
    return x;
}
