#include <iostream>

using namespace std;

int main() {
  while(cin) {
    char c;
    cin.get(c);
    if (c==32)
      cout<<"\n";
    else
      cout<<c;
  }
}
