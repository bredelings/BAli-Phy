#include <cassert>
#include "choose.H"
#include "rng.H"
#include "logsum.H"
#include "myexception.H"

using std::vector;

int choose2(double x, double y,double T) {
  x /= T;
  y /= T;

  double sum = logsum(x,y);
  double r = log_unif();
  if (sum + r < x/T)
    return 0;
  else
    return 1;
}

int choose3(double x, double y, double z, double T) {
  x /= T;
  y /= T;
  z /= T;

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


int choose(vector<double>::const_iterator here,int size,double T) {
  vector<double> sum(size);

  sum[0] = *here/T;here++;
  for(int i=1;i<sum.size();i++) {
    sum[i] = logsum(*here/T,sum[i-1]);
    here++;  
  }

  double r = log_unif() + sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen";
}

int choose(const vector<double>& P,double T) {
  return choose(P.begin(),P.size(),T);
}


double choose3_P(int c,double x, double y, double z, double T) {
  x /= T;
  y /= T;
  z /= T;

  double sum = logsum(z,logsum(x,y));
  if (c==0)
    return x-sum;
  else if (c==1)
    return y-sum;
  else if (c==2)
    return z-sum;
  throw myexception()<<__PRETTY_FUNCTION__<<": incorrect option specified...";
}

double choose_P(int s, const std::vector<double>& P, double T) {
  assert(s >= 0 and s < P.size());

  double sum = log_0;
  for(int i=0;i<P.size();i++)
    sum = logsum(sum,P[i]/T);

  return P[s]/T - sum;
}

int choose_nonlog(const vector<double>& P) {

  double sum = 0;
  for(int i=0;i<P.size();i++)
    sum += P[i];
  double R = myrandomf()*sum;
  
  sum=0;
  for(int i=0; i<P.size();i++) {
    sum += P[i];
    if (R <= sum)
      return i;
  }

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen";}


int choose(const std::vector<log_double_t>& P,double T) {
  vector<log_double_t> sum(P.size());

  sum[0] = pow(P[0],1.0/T);
  for(int i=1;i<sum.size();i++)
    sum[i] = sum[i-1] + pow(P[i],1.0/T);

  log_double_t r = log_double_t(myrandomf()) * sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen";
}

log_double_t choose_P(int s,const std::vector<log_double_t>& P,double T) {
  assert(s >= 0 and s < P.size());

  log_double_t sum = 0.0;
  for(int i=0;i<P.size();i++)
    sum  += pow(P[i],1.0/T);

  return pow(P[s],1.0/T)/sum;
}
