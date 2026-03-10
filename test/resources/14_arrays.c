/* Tests: array declarations and initialization */
int main() {
    /* declare and initialize an array */
    int arr[] = {10, 20, 12};

    /* each int is 4 bytes; ++p advances by 1 byte, so 4 increments = next int */
    int* p = &arr;
    int a = *p;
    ++p; ++p; ++p; ++p;   /* advance 4 bytes to arr[1] */
    int b = *p;
    ++p; ++p; ++p; ++p;   /* advance 4 bytes to arr[2] */
    int c = *p;

    /* 10 + 20 + 12 = 42 */
    if (a != 10) return 1;
    if (b != 20) return 1;
    if (c != 12) return 1;

    return a + b + c;
}
