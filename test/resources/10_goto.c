/* Tests: goto and labels */
int main() {
    int x = 0;

    goto set;
    x = 1;      /* should be skipped */
set:
    x = 42;
    goto end;
    x = 1;      /* should be skipped */
end:
    return x;
}
