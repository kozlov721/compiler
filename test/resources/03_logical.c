/* Tests: &&, ||, ! */
int main() {
    int t = 1;
    int f = 0;

    /* && returns 1 when both are true */
    if (!(t && t)) return 1;
    /* && returns 0 when one is false */
    if (t && f)    return 1;
    /* || returns 1 when at least one is true */
    if (!(t || f)) return 1;
    /* || returns 0 when both are false */
    if (f || f)    return 1;
    /* ! negates */
    if (!t)        return 1;
    if (!(!f))     return 1;

    return 42;
}
