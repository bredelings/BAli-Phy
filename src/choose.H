#ifndef CHOOSE_H
#define CHOOSE_H

#include <vector>
#include "rng.H"
#include "mytypes.H"
#include "myexception.H"

int choose2(efloat_t, efloat_t);

template <typename F>
int choose(const std::vector<F>& P) {
  std::vector<F> sum(P.size());

  sum[0] = P[0];
  for(int i=1;i<sum.size();i++)
    sum[i] = sum[i-1] + P[i];

  F r = F(myrandomf()) * sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen\n"<<show_stack_trace();
}

template <typename F>
int choose(const std::vector<F>& P,double T) {
  std::vector<F> sum(P.size());

  sum[0] = pow(P[0],1.0/T);
  for(int i=1;i<sum.size();i++)
    sum[i] = sum[i-1] + pow(P[i],1.0/T);

  F r = F(myrandomf()) * sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen\n"<<show_stack_trace();
}

template <typename F>
F choose_P(int s,const std::vector<F>& P) {
  assert(s >= 0 and s < P.size());

  F sum = 0.0;
  for(int i=0;i<P.size();i++)
    sum  += P[i];

  return P[s]/sum;
}

template <typename F>
F choose_P(int s,const std::vector<F>& P,double T) {
  assert(s >= 0 and s < P.size());

  F sum = 0.0;
  for(int i=0;i<P.size();i++)
    sum  += pow(P[i],1.0/T);

  return pow(P[s],1.0/T)/sum;
}

#endif

