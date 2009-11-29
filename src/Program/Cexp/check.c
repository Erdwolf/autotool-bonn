#include <stdio.h>

int f (int x) {
  return (++x)+(++x);
}

int main () {
  int x = 3;
  int y = 4;
  (x++ , y) = 5;
  printf ("%d %d\n", x, y);
}
