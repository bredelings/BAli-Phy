#include <iostream>
#include <vector>
#include <valarray>
#include <cassert>
#include <cmath>

#include "util.H"
#include "arguments.H"
#include "bootstrap.H"

FIXME - needs to include statistics.H, and change Pr_statistic ->statistics::Pr

vector<int> confidence_interval(const valarray<double>& sample,double P,int n=10000,int blocksize=100) {
  valarray<double> values = bootstrap_apply<bool,double>(sample,Pr_statistic,n,i);

  vector<double> values2(values.size());
  for(int i=0;i<values.size();i++)
    values2[i] = values[i];
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

  int nreplicates =1000;
  if (args.set("replicates")) {
    nreplicates = convertTo<int>(args["replicates"]);
  }

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
  std::valarray<double> values(nreplicates);
  for(int i=1;i<=500;i++) {
    values = bootstrap_apply<bool,double>(sample,Pr_statistic,nreplicates,i);
    cout<<"------------------\n";
    double var = Var_statistic(values);
    cout<<"blocksize = "<<i<<"   sigma = "<<sqrt(var)<<endl;
  }
}
