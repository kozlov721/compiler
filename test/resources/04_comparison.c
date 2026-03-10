/* Tests: ==, !=, <, >, <=, >= */
int main() {
    if (!(1 == 1))  return 1;
    if (1 == 2)     return 1;
    if (!(1 != 2))  return 1;
    if (1 != 1)     return 1;
    if (!(1 < 2))   return 1;
    if (2 < 1)      return 1;
    if (!(2 > 1))   return 1;
    if (1 > 2)      return 1;
    if (!(1 <= 1))  return 1;
    if (!(1 <= 2))  return 1;
    if (2 <= 1)     return 1;
    if (!(2 >= 2))  return 1;
    if (!(2 >= 1))  return 1;
    if (1 >= 2)     return 1;
    return 42;
}
