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

  F r = F(uniform()) * sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen\n"<<show_stack_trace();
}

template <typename F>
int choose_MH_scratch(int i,const std::vector<F>& P, std::vector<F>& sum) 
{
  assert(P.size() == sum.size());
  assert(i < P.size());

  if (P.size() == 1)
    return 0;

  // compute min of all elements
  // compute sum of all elements EXCEPT P[i]
  F total = 0;
  F min = P[0];
  for(int k=0;k<sum.size();k++) 
  {
    if (k != i) total += P[k];
    sum[k] = total;

    if (P[k] < min) min = P[k];
  }
  total += P[i];

  // choose the element 
  F r = F(uniform()) * (total - min);

  for(int k=0;k<sum.size();k++) 
    if (r < sum[k])
      return k;

  if (r < total - min)
    return i;

  throw myexception()<<__PRETTY_FUNCTION__<<": no option chosen\n"<<show_stack_trace();
}

template <typename F>
inline int choose_scratch(std::vector<F>& P) 
{
  return choose_scratch(P,P);
}

template <typename F>
inline int choose_MH_scratch(int i,std::vector<F>& P) 
{
  return choose_MH_scratch(i,P,P);
}

template <typename F>
inline int choose(const std::vector<F>& P) 
{
  std::vector<F> sum(P.size());

  return choose_scratch(P,sum);
}

template <typename F>
inline int choose_MH(int i,const std::vector<F>& P) 
{
  std::vector<F> sum(P.size());

  return choose_MH_scratch(i,P,sum);
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
F choose_MH_P(int i,int j,const std::vector<F>& P) {
  assert(i >= 0 and i < P.size());
  assert(j >= 0 and j < P.size());

  if (P.size() == 1) {
    if (j==0) 
      return 1.0;
    else
      return 0.0;
  }

  F min = P[0];
  F sum = P[0];
  for(int k=1;k<P.size();k++) {
    sum  += P[k];

    if (P[k] < min) min = P[k];
  }

  if (i==j) {
    if (P[j] > min)
      return (P[j] - min)/(sum - min);
    else
      return 0.0;
  }
  else
    return P[j]/(sum - min);
}

#endif

