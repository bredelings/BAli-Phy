#include <cassert>

#include <string>
#include <sstream>
#include <iostream>

using std::string;

double convertToDouble(const string& s) {
   std::istringstream i(s);
   double d;
   i>>d;
   return d;
}

int main(int argc,char* argv[]) {
  if (argc != 3) {
    std::cerr<<"Usage: "<<argv[0]<<" <key> <cut value>\n";
    exit(1);
  }

  string pattern(argv[1]);
  pattern += " = ";

  string cutoff_string(argv[2]);
  double cutoff = convertToDouble(cutoff_string);

  string line;
  while(getline(std::cin,line)) {
    int where = line.find(pattern);
    if (where == -1) continue;
    double value = convertToDouble(line.substr(where+pattern.size()));

    //    std::cerr<<line<<std::endl;
    //    std::cerr<<"where = "<<where<<std::endl;
    //    std::cerr<<value<<std::endl;
    if (value >= cutoff)
      break;
  }

  std::cout<<line<<std::endl;
  while(getline(std::cin,line))
    std::cout<<line<<std::endl;
}
