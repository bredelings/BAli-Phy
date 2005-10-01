#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "statistics.H"

using namespace std;

int main(int argc,char* argv[]) { 
  assert(argc == 2);
  string label = argv[1];

  cout.precision(3);

  // Read in the values
  vector<double> values;
  while(cin) {
    double d;
    cin >> d;
    values.push_back(d);
  }
  cerr<<"Read in "<<values.size()<<" values\n";

  // Translate vector to valarray...
  valarray<double> values2(values.size());
  for(int i=0;i<values2.size();i++)
    values2[i] = values[i];

  // Print out reports on the statistics
  cout<<" E "<<label<<" = "<<statistics::average(values2);
  cout<<" [+- "<<sqrt(statistics::Var(values2))<<"]";


  double P = 0.95;

  std::pair<double,double> interval = statistics::confidence_interval(values2,P);

  cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
}
