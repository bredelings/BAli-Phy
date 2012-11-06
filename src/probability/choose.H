/*
   Copyright (C) 2004-2006 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef CHOOSE_H
#define CHOOSE_H

#include <vector>
#include "rng.H"
#include "mytypes.H"
#include "myexception.H"
#include "util.H"

template <typename F>
class choose_exception: public myexception {
public:
  /// What is the current choice?
  int i;

  /// What are the probabilities of each of the options?
  std::vector<F> Pr;

  choose_exception(int i, const std::vector<F>& V)
    :Pr(V)
  {
    (*this)<<"No option chosen! (current = "<<i<<")\n";
    for(int j=0;j<Pr.size();j++) {
      if (i == j) (*this)<<"*";
      (*this)<<"Pr["<<j<<"] = "<<Pr[j]<<"\n";
    }
    (*this)<<show_stack_trace();
    //    std::abort();
  }
  ~choose_exception() throw() {}
};

template <> choose_exception<efloat_t>::choose_exception(int i, const std::vector<efloat_t>& V);

/// Select from choice 0 or 1 in proportion to the probabilities given.
int choose2(efloat_t, efloat_t);

/// Select from choices [0,P.size-1] in proportion to the probabilities given, using pre-allocated scratch space.
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

  choose_exception<F> c(0, P);
  c.prepend(":\n");
  c.prepend(__PRETTY_FUNCTION__);
  throw c;
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

template <typename F>
inline int choose_MH(int i, const std::vector<F>& P) 
{
  const int N = P.size();

  // Get the order of the probabilities
  std::vector<int> o = iota<int>(N);
  sequence_order<F> A(P);
  std::sort(o.begin(), o.end(), A);

  // Compute sum of elements larger than the k-th smallest
  std::vector<F> total_greater_than(N);
  total_greater_than[N-1] = 0;
  for(int k = N-2; k >= 0; k--)
    total_greater_than[k] = total_greater_than[k+1] + P[o[k+1]];

  F U = uniform();

  F prod = 1;
  F sum = 0;
  int I = -1;
  for(int k=0; k<N; k++)
  {
    if (o[k] == i) I=k;

    F accept_k = 0;

    if (I == -1) { // k<I
      accept_k = P[o[k]] / total_greater_than[k];   // MH
    }
    else if (k == I)
    {
      accept_k = (I==N-1)?1:0;                      // MH
    }
    else if (k > I)
    {
      accept_k = P[o[k]] / total_greater_than[I];   // Gibbs
    }
    else
      std::abort();

    F Pr_k = prod * accept_k;

    sum += Pr_k;
    if (U <= sum) return o[k];

    if (I == -1)
      prod *= (1.0 - accept_k);
  }

  choose_exception<F> c(i, P);
  c.prepend(":\n");
  c.prepend(__PRETTY_FUNCTION__);
  throw c;
}

template <typename F>
inline F choose_MH_P(int i, int j,const std::vector<F>& P) 
{
  const int N = P.size();

  // Get the order of the probabilities
  std::vector<int> o = iota<int>(N);
  sequence_order<F> A(P);
  std::sort(o.begin(), o.end(), A);

  //  if (i==j and o[N-1] != i) return 0;

  // Compute sum of elements larger than the k-th smallest
  std::vector<F> total_greater_than(N);
  total_greater_than[N-1] = 0;
  for(int k = N-2; k >= 0; k--)
    total_greater_than[k] = total_greater_than[k+1] + P[o[k+1]];

  F prod = 1;
  int I = -1;
  for(int k=0; k<N; k++)
  {
    if (o[k] == i) I=k;

    F accept_k = 0;

    if (I == -1) { // k<I
      accept_k = P[o[k]] / total_greater_than[k];   // MH
    }
    else if (k == I)
    {
      accept_k = (I==N-1)?1:0;                      // MH
    }
    else if (k > I)
    {
      accept_k = P[o[k]] / total_greater_than[I];   // Gibbs
    }
    else
      std::abort();

    F Pr_k = prod * accept_k;

    if (o[k] == j) return Pr_k;

    if (I == -1)
      prod *= (1.0 - accept_k);
  }

  choose_exception<F> c(i, P);
  c.prepend(": \n");
  c.prepend(__PRETTY_FUNCTION__);
  throw c;
}
#endif

