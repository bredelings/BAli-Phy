#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "statistics.H"

using std::vector;
using std::valarray;

int main(int argc,char* argv[]) { 
  assert(argc == 1);
  std::string label = argv[1];

  std::cout.precision(3);

  // Read in the values
  vector<double> values;
  while(std::cin) {
    double d;
    std::cin >> d;
    values.push_back(d);
  }
  std::cerr<<"Read in "<<values.size()<<" values\n";

  // Translate vector to valarray...
  valarray<double> values2(values.size());
  for(int i=0;i<values2.size();i++)
    values2[i] = values[i];

  // Print out reports on the statistics
  std::cout<<" E "<<label<<" = "<<statistics::average(values2);
  std::cout<<" [+- "<<sqrt(statistics::Var(values2))<<"]";
}
