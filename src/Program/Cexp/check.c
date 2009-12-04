#include <iostream>

using namespace std;

int f (int x) {
  return (++x)+(++x);
}

int main () {
  cout << f(3) << endl;
  int x = 3;
  int y = 4;
  (x++ , y) = 5;
  cout << x << y << endl;
}
