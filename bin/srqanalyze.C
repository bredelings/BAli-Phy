#include <iostream>
#include <vector>
#include <valarray>
#include <cassert>
#include <cmath>

#include "util.H"
#include "arguments.H"
#include "bootstrap.H"

double Pr_statistic(const valarray<bool>& v) {
  int sum=0;
  for(int i=0;i<v.size();i++)
    if (v[i]) sum++;
  return double(sum)/v.size();
}

double moment(const valarray<double>& v,int m) {
  double total = 0;
  for(int i=0;i<v.size();i++)
    total += pow(v[i],m);
  return total/v.size();
}

double Var_statistic(const valarray<double>& v) {
  double m1 = moment(v,1);
  double m2 = moment(v,2);
  return m2 - m1*m1;
}

using namespace std;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  cerr.precision(10);
  cout.precision(10);

  // initialize random number generator
  unsigned long seed = 0;
  if (args.set("seed")) {
    seed = convertTo<unsigned long>(args["seed"]);
    myrand_init(seed);
  }
  else
    seed = myrand_init();
  std::cout<<"random seed = "<<seed<<endl<<endl;

  // Read in data
  vector<bool> data;
  int i;
  while(cin>>i) {
    assert(i==0 or i==1);
    data.push_back(i);
  } 
  valarray<bool> sample(data.size());
  for(int i=0;i<sample.size();i++)
    sample[i] = data[i];

  double mean = Pr_statistic(sample);
  cout<<"mean = "<<mean<<endl;
  std::valarray<double> values(sample.size());
  for(int i=1;i<=500;i++) {
    values = bootstrap_apply<bool,double>(sample,Pr_statistic,10000,i);
    double var = Var_statistic(values);
    cout<<"blocksize = "<<i<<"   sigma = "<<sqrt(var)<<endl;
  }
}
