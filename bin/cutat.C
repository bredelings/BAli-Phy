#include <cassert>

#include <string>
#include <iostream>

using std::string;

int main(int argc,char* argv[]) {
  if (argc != 2) {
    std::cerr<<"Usage: "<<argv[0]<<" <cut pattern>\n";
    exit(1);
  }

  string pattern(argv[1]);

  string line;
  while(getline(std::cin,line)) {
    int where = line.find(pattern);
    if (where != -1)
      break;
  }

  while(getline(std::cin,line))
    std::cout<<line;
}
