#include <iostream>
#include <vector>
#include <valarray>
#include <cassert>
#include <cmath>

#include "util.H"
#include "arguments.H"
#include "bootstrap.H"
#include "statistics.H"

using namespace std;

vector<double> confidence_interval(const valarray<bool>& sample,
				   double (*statistic)(const valarray<bool>&),
				   double P,
				   int n=10000,
				   int blocksize=100) 
{
  valarray<double> values = bootstrap_apply<bool,double>(sample,statistic,n,blocksize);

  return statistics::confidence_interval(values,P);
}

double lower_confidence_bound(const valarray<bool>& sample,
			      double (*statistic)(const valarray<bool>&),
			      double P,
			      int n=10000,
			      int blocksize=100) 
{

  valarray<double> values = bootstrap_apply<bool,double>(sample,statistic,n,blocksize);

  return statistics::lower_confidence_bound(values,P);
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  cerr.precision(10);
  cout.precision(10);

  // initialize random number generator
  unsigned long seed = 0;
  if (args.set("seed")) {
    seed = args.loadvalue<unsigned long>("seed");
    myrand_init(seed);
  }
  else
    seed = myrand_init();
  std::cout<<"random seed = "<<seed<<endl<<endl;

  int nreplicates = args.loadvalue("replicates",10000);
  std::cout<<"nreplicates = "<<nreplicates<<endl<<endl;

  int blocksize = args.loadvalue("blocksize",100);
  std::cout<<"blocksize = "<<blocksize<<endl<<endl;

  int pseudocount = args.loadvalue("pseudocount",0);
  std::cout<<"pseudocount = "<<pseudocount<<endl<<endl;
  
  /*----------------------- Read in data ---------------------------*/
  vector<bool> data;
  int i;
  while(cin>>i) {
    assert(i==0 or i==1);
    data.push_back(i);
  } 
  for(int i=0;i<pseudocount;i++) {
    data.push_back(true);
    data.push_back(false);
  }

  valarray<bool> sample(data.size());
  for(int i=0;i<sample.size();i++)
    sample[i] = data[i];
    

  /*----------------------- Basic Statistic ---------------------------*/
  cout<<"Read "<<sample.size()<<" values.    Y = "<<statistics::count(sample)<<"       N = "<<sample.size() - statistics::count(sample)<<endl;

  double mean = statistics::Pr(sample);
  cout<<"mean = "<<mean<<endl;
  cout<<"var  = "<<mean*(1.0-mean)/sample.size()<<" (assumes independant samples && mean is right)"<<endl;
  cout<<"log odds = "<<log10(mean/(1.0-mean))<<" = log("<<mean/(1.0-mean)<<")"<<endl;
  cout<<endl;
  cout<<endl;

  /*------------------------- Really analyze the distribution ------------------------*/

  double lowerb = lower_confidence_bound(sample,statistics::Pr,0.95,nreplicates,blocksize);
  cout<<"95% confidence P > "<<lowerb<<endl;
  cout<<"95% confidence log odds > "<<log10(lowerb/(1.0-lowerb))<<" = log("<<lowerb/(1.0-lowerb)<<")"<<endl;
  cout<<endl;
  cout<<endl;

  double lowerb2 = lower_confidence_bound(sample,statistics::Pr,0.99,nreplicates,blocksize);
  cout<<"99% confidence P > "<<lowerb2<<endl;
  cout<<"99% confidence log odds > "<<log10(lowerb2/(1.0-lowerb2))<<" = log("<<lowerb2/(1.0-lowerb2)<<")"<<endl;
  cout<<endl;
  cout<<endl;
  
  vector<double> interval = confidence_interval(sample,statistics::Pr,0.99,nreplicates,blocksize);
  cout<<"99% confidence interval = ("<<interval[0]<<","<<interval[1]<<")\n";
  cout<<"                  width = "<<interval[1] - interval[0]<<" = 2.0*"<<(interval[1] - interval[0])/2.0<<endl;
  cout<<endl;
  cout<<endl;
  
  /*
  std::valarray<double> values(nreplicates);
  for(int i=1;i<=500;i++) {
    values = bootstrap_apply<bool,double>(sample,statistics::Pr,nreplicates,i);
    cout<<"------------------\n";
    double var = statistics::Var(values);
    cout<<"blocksize = "<<i<<"   sigma = "<<sqrt(var)<<endl;
  }
  */
}
