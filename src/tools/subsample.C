#include <iostream>
#include <string>
#include "util.H"

using namespace std;

int main(int argc,char* argv[]) { 
  if (argc != 2) {
    std::cerr<<"Usage: "<<argv[0]<<" <factor>\n";
    exit(1);
  }

  int subsample = convertTo<int>(argv[1]);

  string line;

  // print header
  getline(cin,line);
  cout<<line<<endl;

  // print selected lines
  int lines=0;
  while(getline(cin,line)) {
    if (lines%subsample == 0)
      cout<<line<<endl;
    lines++;
  }
  return 0;
}
