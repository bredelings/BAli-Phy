#include <iostream>
#include <vector>
#include <algorithm>

#include "logsum.H"
using namespace std;

double Pr_harmonic(const vector<double>& v) {
  double sum = 0;

  for(int i=0;i<v.size();i++)
    sum += v[i];

  double denominator=log_0;

  for(int i=0;i<v.size();i++)
    denominator = logsum(denominator,sum-v[i]);

  return sum - denominator + log(double(v.size()));
}


double Pr_smoothed(const vector<double>& v,double delta,double Pdata) {
  double sum = 0;
  for(int i=0;i<v.size();i++)
    sum += v[i];

  double log_delta = log(delta);
  double log_inv_delta = log(1-delta);

  double n = v.size();
  double top    = log(n) + log_delta - log_inv_delta;
  double bottom = log(n) + log_delta - log_inv_delta - Pdata;

  for(int i=0;i<v.size();i++) {
    double weight = -logsum(log_delta+Pdata,log_inv_delta+v[i]);
    top = logsum(top,v[i] + weight);
    bottom = logsum(bottom,weight);
  }

  return top - bottom;
}


double Pr(const vector<double>& v,int b,int e) {
  if (b >= v.size())
    return log_0;
  if (e >= v.size())
    e = v.size()-1;

  vector<double> v1(e-b+1);
  copy(v.begin()+b,v.begin()+e,v1.begin());
  return Pr_harmonic(v1);
}

double Pr_smoothed(const vector<double>& v,int b,int e,double delta) {
  if (b >= v.size())
    return log_0;
  if (e >= v.size())
    e = v.size()-1;

  /*----------------- Set up the sub-vector -----------------*/
  vector<double> v1(e-b+1);
  copy(v.begin()+b,v.begin()+e,v1.begin());

  /*------------------- start the iteration -----------------*/
  double Pdata = Pr_harmonic(v1);

  // initialize this to a value which allows it to enter the loop
  double deltaP = 1.0;
  int iterations = 0;
  while (std::abs(deltaP) > 1.0e-8) {
    deltaP = Pr_smoothed(v1,delta,Pdata) - Pdata;
    Pdata += deltaP;

    iterations++;
    std::cerr<<"iterations = "<<iterations<<"  Pdata = "<<Pdata<<endl;
    // if we aren't converging, warn and don't give an answer
    if (iterations >200) {
      std::cerr<<"Probabilities not converging!!!";
      return log_0;
      //      exit(1);
    }
    
  }
  return Pr_smoothed(v1,delta,Pdata);
}


int main() {

  const double delta = 0.01;

  vector<double> data;

  double d;
  while(cin>>d) {
    data.push_back(d);
  }

  cout<<"OK: read "<<data.size()<< " values."<<endl;
 
  int width = data.size()/3+2;

  cout<<"E1 = "<<Pr(data,0,width)<<"    ";
  cout<<"E2 = "<<Pr(data,width,width*2)<<"    ";
  cout<<"E3 = "<<Pr(data,width*2,width*3)<<endl;

  cout<<"S1 = "<<Pr_smoothed(data,0,width,delta)<<"    ";
  cout<<"S2 = "<<Pr_smoothed(data,width,width*2,delta)<<"    ";
  cout<<"S3 = "<<Pr_smoothed(data,width*2,width*3,delta)<<endl;
}
