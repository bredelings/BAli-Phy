#include <iostream>
#include <vector>
#include <algorithm>

#include "logsum.H"
using namespace std;

double Pr(const vector<double>& v) {
  double sum = 0;

  for(int i=0;i<v.size();i++)
    sum += v[i];

  double denominator=log_0;

  for(int i=0;i<v.size();i++)
    denominator = logsum(denominator,sum-v[i]);

  return sum-denominator;
}


double Pr(const vector<double>& v,int b,int e) {
  if (b >= v.size())
    return log_0;
  if (e > v.size())
    e = v.size();

  vector<double> v1(e-b+1);
  copy(v.begin()+b,v.begin()+e,v1.begin());
  return Pr(v1);
}


int main() {

  vector<double> data;

  double d;
  while(cin>>d) {
    data.push_back(d);
  }

  cerr<<"OK: read "<<data.size()<< " values."<<endl;
 
  int width = data.size()/3+2;

  cout<<"E1 = "<<Pr(data,0,width)<<"    ";
  cout<<"E2 = "<<Pr(data,width,width*2)<<"    ";
  cout<<"E3 = "<<Pr(data,width*2,width*3)<<endl;

  
}
