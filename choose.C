#include <cassert>
#include "choose.H"
#include "rng.H"
#include "logsum.H"

using std::vector;

int choose(double x, double y) {
  double sum = logsum(x,y);
  double r = log_unif();
  if (sum + r < x)
    return 0;
  else
    return 1;
  assert(0);
}

int choose(double x, double y, double z) {
  int choice=2;
  double sum = logsum(z,logsum(x,y));
  //  double x_ = exp(x-sum), y_ = exp(y-sum),z_ = exp(z-sum);
  //  std::cerr<<x_<<"  "<<y_<<"  "<<z_;
  double r = log_unif();
  if (sum + r < x)
    choice=0;
  else if (sum + r < logsum(x,y))
    choice=1;

  //  std::cerr<<" "<<choice<<endl;
  return choice;
}


int choose(vector<double>::const_iterator here,int size) {
  vector<double> sum(size);

  sum[0] = *here;here++;
  for(int i=1;i<sum.size();i++) {
    sum[i] = logsum(*here,sum[i-1]);
    here++;  
  }

  double r = log_unif() + sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;

  std::abort();
}

int choose(const vector<double>& P) {
  return choose(P.begin(),P.size());
}


double choose_P(int c,double x, double y, double z) {
  double sum = logsum(z,logsum(x,y));
  if (c==0)
    return x-sum;
  else if (c==1)
    return y-sum;
  else if (c==2)
    return z-sum;
  assert(0);
}

double choose_P(int s, const std::vector<double>& P) {
  double sum = log_0;
  for(int i=0;i<P.size();i++)
    sum = logsum(sum,P[i]);

  return P[s] - sum;
}

