#include <iostream>
#include <vector>
#include <valarray>
#include <algorithm>

#include "statistics.H"
#include "bootstrap.H"
#include "logsum.H"
#include "arguments.H"
using namespace std;

double Pr_harmonic(const valarray<double>& v) {
  double sum = 0;

  for(int i=0;i<v.size();i++)
    sum += v[i];

  double denominator=log_0;

  for(int i=0;i<v.size();i++)
    denominator = logsum(denominator,sum-v[i]);

  return sum - denominator + log(double(v.size()));
}

double Pr_smoothed(const valarray<double>& v,double delta,double Pdata) {

  double log_delta = log(delta);
  double log_inv_delta = log(1-delta);

  double n = v.size();
  double top    = log(n) + log_delta - log_inv_delta + Pdata;
  double bottom = log(n) + log_delta - log_inv_delta;

  for(int i=0;i<v.size();i++) {
    double weight = -logsum(log_delta, log_inv_delta + v[i]-Pdata);
    top = logsum(top,weight + v[i]);

    bottom = logsum(bottom,weight);
  }

  return top - bottom;
}

double Pr_smoothed(const valarray<double>& v) {
  // Sample from the prior w/ probability delta...
  double delta=0.01;

  // Use the harmonic estimator as the starting guess
  double Pdata = Pr_harmonic(v);

  // initialize this to a value which allows it to enter the loop
  double deltaP = 1.0;

  int iterations = 0;
  double dx = 10.0;

  while (std::abs(deltaP) > 1.0e-3) {

    double g1 = Pr_smoothed(v,delta,Pdata) - Pdata;
    std::cerr<<"iterations = "<<iterations<<"  Pdata = "<<Pdata<<"   g = "<<g1<<endl;

    double Pdata2 = Pdata + g1;

    dx = g1 * 10;
    double g2 = Pr_smoothed(v,delta,Pdata+dx) - (Pdata+dx);

    double dgdx = (g2-g1)/dx;

    double Pdata3 = Pdata - g1/dgdx;
    if (Pdata3 < 2.0*Pdata or Pdata3 > 0 or Pdata3 > 0.5*Pdata)
      Pdata3 = Pdata + 10*g1;

    double g3 = Pr_smoothed(v,delta,Pdata3) - Pdata3;

    // Try to do Newton's method
    if (std::abs(g3) <= std::abs(g2) and ((g3 > 0) or (std::abs(dgdx)>0.01))) {
      deltaP = Pdata3-Pdata;
      Pdata = Pdata3;
    }
    // Otherwise see if we can go 10 times as far as one step
    else if (std::abs(g2) <= std::abs(g1)) {
      Pdata2 += g2;
      deltaP = Pdata2-Pdata;
      Pdata = Pdata2;
    }
    // Otherwise go one step
    else {
      deltaP = g1;
      Pdata += g1;
    }

    std::cerr<<" Pdata2 = "<<Pdata2<<"   g = "<<g2<<endl;
    std::cerr<<" Pdata3 = "<<Pdata3<<"   g = "<<g3<<"        dg/dx = "<<dgdx<<endl;

    iterations++;

    // if we aren't converging, warn and don't give an answer
    if (iterations >400) {
      std::cerr<<"Probabilities not converging!!!";
      return log_0;
      //      exit(1);
    }
    
  }
  return Pdata;
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  std::cout.precision(3);
  std::cout.setf(ios::fixed);

  /*---------- Initialize random seed -----------*/
  unsigned long seed = 0;
  if (args.set("seed")) {
    seed = convertTo<unsigned long>(args["seed"]);
    myrand_init(seed);
  }
  else
    seed = myrand_init();
  std::cerr<<"random seed = "<<seed<<endl<<endl;

  vector<double> data;

  double d;
  while(cin>>d) {
    data.push_back(d);
  }

  cerr<<"OK: read "<<data.size()<< " values."<<endl;
 
  // Translate vector to valarray...
  valarray<double> values(data.size());
  for(int i=0;i<values.size();i++)
    values[i] = data[i];

  double PM = Pr_smoothed(values);

  cout<<"P(M|data) = "<<PM<<"  ";
  cout.flush();

  //---------- Get bootstrap sample --------------/

  int blocksize = data.size()/100+2;
  if (blocksize > values.size())
    blocksize = values.size();

  valarray<double> values2 = bootstrap_apply<double,double>(values,Pr_smoothed,100,blocksize);

  double stddev = sqrt( statistics::Var(values2) );

  std::cout<<"  +- "<<stddev<<endl;
}
