#include <iostream>

using namespace std;

int main() {
  char c;
  while(cin.get(c)) {
    if (c!=13)
      cout<<c;
  }

  return 1;
}
