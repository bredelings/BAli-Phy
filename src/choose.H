#ifndef CHOOSE_H
#define CHOOSE_H

#include <vector>
#include "rng.H"
#include "mytypes.H"
#include "myexception.H"

int choose2(efloat_t, efloat_t);

template <typename F>
int choose_scratch(const std::vector<F>& P, std::vector<F>& sum) 
{
  assert(P.size() == sum.size());

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
inline int choose_scratch(std::vector<F>& P) 
{
  return choose_scratch(P,P);
}

template <typename F>
inline int choose(const std::vector<F>& P) 
{
  std::vector<F> sum(P.size());

  return choose_scratch(P,sum);
}

template <typename F>
F choose_P(int s,const std::vector<F>& P) {
  assert(s >= 0 and s < P.size());

  F sum = 0.0;
  for(int i=0;i<P.size();i++)
    sum  += P[i];

  return P[s]/sum;
}

#endif

