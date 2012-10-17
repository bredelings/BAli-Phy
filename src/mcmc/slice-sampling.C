/*
   Copyright (C) 2008 Benjamin Redelings

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

///
/// \file slice-sampling.C
///
/// \brief This file implements classes and functions for uniform slice sampling.
///

#include "slice-sampling.H"
#include "rng.H"
#include "choose.H"

using std::vector;

namespace slice_sampling {
  double identity(double x) {return x;}
}

double slice_function::current_value() const
{
  std::abort();
}

double parameter_slice_function::operator()(double x)
{
  count++;
  P.set_parameter_value(n,inverse(x));
  return log(P.heated_probability());
}

double parameter_slice_function::operator()()
{
  count++;
  return log(P.heated_probability());
}

double parameter_slice_function::current_value() const
{
  return P.get_parameter_value_as<Double>(n);
}

parameter_slice_function::parameter_slice_function(Probability_Model& P_,int n_)
  :slice_function(P_.get_bounds(n_)),
   count(0),P(P_),n(n_),
   transform(slice_sampling::identity),
   inverse(slice_sampling::identity)
{ }

parameter_slice_function::parameter_slice_function(Probability_Model& P_,int n_,
						   double(*f1)(double),
						   double(*f2)(double))
  :slice_function(P_.get_bounds(n_)),
   count(0),P(P_),n(n_),transform(f1),inverse(f2)
{
  if (has_lower_bound)
    lower_bound = transform(lower_bound);
  if (has_upper_bound)
    upper_bound = transform(upper_bound);
}


double branch_length_slice_function::operator()(double l)
{
  count++;
  P.setlength(b,l);
  return log(P.heated_probability());
}

double branch_length_slice_function::operator()()
{
  count++;
  return log(P.heated_probability());
}

double branch_length_slice_function::current_value() const
{
  return P.T->directed_branch(b).length();
}

branch_length_slice_function::branch_length_slice_function(Parameters& P_,int b_)
  :count(0),P(P_),b(b_)
{ 
  set_lower_bound(0);
}

double slide_node_slice_function::operator()() {
  count++;
  return log(P.heated_probability());
}

double slide_node_slice_function::operator()(double x) 
{
  /* Problem: If y << x, then we may not be able to recover y = (x+y)-x.
     This occurs, for example, when x = 1 and y = 1.0e-9.

     Solution: Store x0 and y0 separately, and compute y = y0 + (x0-x).
  */
  assert(0 <= x and x <= (x0+y0));
  P.setlength(b1,x);
  P.setlength(b2,y0+(x0-x));
  return operator()();
}

double slide_node_slice_function::current_value() const
{
  return P.T->branch(b1).length();
}

slide_node_slice_function::slide_node_slice_function(Parameters& P_,int b0)
  :count(0),P(P_)
{
  vector<const_branchview> b;
  b.push_back( P.T->directed_branch(b0) );

  // choose branches to alter
  append(b[0].branches_after(),b);

  if (not b.size() == 3)
    throw myexception()<<"pointing to leaf node!";

  b1 = b[1].undirected_name();
  b2 = b[2].undirected_name();

  x0 = P.T->branch(b1).length();
  y0 = P.T->branch(b2).length();

  set_lower_bound(0);
  set_upper_bound(x0+y0);
}

slide_node_slice_function::slide_node_slice_function(Parameters& P_,int i1,int i2)
  :count(0),b1(i1),b2(i2),P(P_)
{
  x0 = P.T->branch(b1).length();
  y0 = P.T->branch(b2).length();

  set_lower_bound(0);
  set_upper_bound(x0+y0);
}

/// \brief Compute the sum of the branch mean parameters for \a P
double sum_of_means(const Parameters& P)
{
  double sum = 0;
  for(int i=0;i<P.n_branch_means();i++) 
    sum += P.get_parameter_value_as<Double>(P.branch_mean_index(i));
  return sum;
}

/// \brief Scale the branch means of \a P so that they sum to \a t, but do not invalidate cached values.
///
/// \param P The model state to modify.
/// \param t The sum of the branch means after modification.
///
/// Neither recalc_smodel() nor recalc_imodel() is called on the data_partitions.
///
double set_sum_of_means_tricky(Parameters& P, double t)
{
  double sum = sum_of_means(P);
  double scale = t/sum;
  for(int i=0;i<P.n_branch_means();i++) 
    P.branch_mean_tricky(i,P.get_parameter_value_as<Double>(P.branch_mean_index(i))*scale);

  return scale;
}

double scale_means_only_slice_function::operator()(double t)
{
  // Set the values of \mu[i]
  double scale = set_sum_of_means_tricky(P, initial_sum_of_means * exp(t));

  // Scale the tree in the opposite direction
  SequenceTree& T = *P.T.modify();

  for(int b=0;b<T.n_branches();b++) 
  {
    const double L = T.branch(b).length();
    T.branch(b).set_length(L/scale);
  }

  return operator()();
}

double scale_means_only_slice_function::operator()()
{
  count++;

  SequenceTree& T = *P.T.modify();
  const int B = T.n_branches();
  const int n = P.n_branch_means();

  // return pi * (\sum_i \mu_i)^(n-B)
  return log(P.heated_probability()) + log(sum_of_means(P))*(n-B);
}

double scale_means_only_slice_function::current_value() const
{
  double t = log(sum_of_means(P)/initial_sum_of_means);
  return t;
}

scale_means_only_slice_function::scale_means_only_slice_function(Parameters& P_)
  :count(0),
   initial_sum_of_means(sum_of_means(P_)),
   P(P_)
{ 
  Bounds<double>& b = *this;

#ifndef NDEBUG
  std::clog<<"Bounds on t are "<<b<<std::endl;
#endif

  for(int i=0; i<P.n_branch_means(); i++)
  {
    Bounds<double> b2 = P.get_bounds(P.branch_mean_index(i));
    if (P.is_fixed(P.branch_mean_index(i)))
      throw myexception()<<"One of the means is fixed";

    if (b2.has_lower_bound and b2.lower_bound > 0)
    {
      b2.has_lower_bound = true;
      b2.lower_bound = log(b2.lower_bound) - log(P.get_parameter_value_as<Double>(P.branch_mean_index(i)));
    }
    else
      b2.has_lower_bound = false;

    if (b2.has_upper_bound)
      b2.upper_bound = log(b2.upper_bound) - log(P.get_parameter_value_as<Double>(P.branch_mean_index(i)));

    b = b and b2;
  }

#ifndef NDEBUG
  std::clog<<"Bounds on t are now "<<b<<std::endl<<std::endl;
#endif
}

double constant_sum_slice_function::operator()(double t)
{
  vector<Double> x = P.get_parameter_values_as<Double>(indices);

  double total = sum(x);

  double factor = (total - t)/(total - x[n]);

  for(int i=0;i<indices.size();i++)
    if (i == n)
      x[i] = t;
    else
      x[i] *= factor;

  assert(std::abs(::sum(x) - total) < 1.0e-9);

  P.set_parameter_values(indices,x);
  return operator()();
}


double constant_sum_slice_function::operator()()
{
  count++;

  vector<Double> x = P.get_parameter_values_as<Double>(indices);

  double total = sum(x);

  double t = current_value();

  const int N = indices.size();

  // return pi * (1-x)^(N-1)
  return log(P.heated_probability()) + (N-1)*log(total-t);
}

double constant_sum_slice_function::current_value() const
{
  return P.get_parameter_value_as<Double>(indices[n]);
}


constant_sum_slice_function::constant_sum_slice_function(Probability_Model& P_, const vector<int>& indices_,int n_)
  :count(0),
   indices(indices_),
   n(n_),
   P(P_)
{ 
  vector<Double> x = P.get_parameter_values_as<Double>(indices);
  double total = sum(x);

  set_lower_bound(0);
  set_upper_bound(total);
}

std::pair<double,double> 
find_slice_boundaries_stepping_out(double x0,slice_function& g,double logy, double w,int m)
{
  assert(g.in_range(x0));

  double u = uniform()*w;
  double L = x0 - u;
  double R = x0 + (w-u);

  // Expand the interval until its ends are outside the slice, or until
  // the limit on steps is reached.

  if (m>1) {
    int J = floor(uniform()*m);
    int K = (m-1)-J;

    while (J>0 and (not g.below_lower_bound(L)) and g(L)>logy) {
      L -= w;
      J--;
    }

    while (K>0 and (not g.above_upper_bound(R)) and g(R)>logy) {
      R += w;
      K--;
    }
  }
  else {
    while ((not g.below_lower_bound(L)) and g(L)>logy)
      L -= w;

    while ((not g.above_upper_bound(R)) and g(R)>logy)
      R += w;
  }

  // Shrink interval to lower and upper bounds.

  if (g.below_lower_bound(L)) L = g.lower_bound;
  if (g.above_upper_bound(R)) R = g.upper_bound;

  assert(L < R);

  return std::pair<double,double>(L,R);
}

// Does this x0 really need to be the original point?
// I think it just serves to let you know which way the interval gets shrunk...

double search_interval(double x0,double& L, double& R, slice_function& g,double logy)
{
  //  assert(g(x0) > g(L) and g(x0) > g(R));
  assert(g(x0) >= logy);
  assert(L < R);
  assert(L <= x0 and x0 <= R);

  double L0 = L, R0 = R;

  // std::cerr<<"**    L0 = "<<L0<<"   x0 = "<<x0<<"   R0 = "<<R0<<std::endl;
  for(int i=0;i<200;i++)
  {
    double x1 = L + uniform()*(R-L);
    double gx1 = g(x1);
    //   std::cerr<<"    L  = "<<L <<"   x = "<<g.current_value()<<"   R  = "<<R<<std::endl;
    //   std::cerr<<"    logy  = "<<logy<<"\n"; //  logy_x0 = "<<logy_x0<<" logy_current = "<<g()<<std::endl;

    if (gx1 >= logy) return x1;

    if (x1 > x0) 
      R = x1;
    else
      L = x1;
  }
  std::cerr<<"Warning!  Is size of the interval really ZERO?"<<std::endl;
  double logy_x0 = g(x0);  
  std::cerr<<"    L0 = "<<L0<<"   x0 = "<<x0<<"   R0 = "<<R0<<std::endl;
  std::cerr<<"    L  = "<<L <<"   x = "<<g.current_value()<<"   R  = "<<R<<std::endl;
  std::cerr<<"    logy  = "<<logy<<"  logy_x0 = "<<logy_x0<<"  logy_current = "<<g()<<std::endl;

  std::abort();

  return x0;
}

double slice_sample(double x0, slice_function& g,double w, int m)
{
  assert(g.in_range(x0));

  double gx0 = g();
  volatile double diff = gx0 - g(x0);
  assert(std::abs(diff) < 1.0e-9);

  // Determine the slice level, in log terms.

  double logy = gx0 - exponential(1);

  // Find the initial interval to sample from.

  std::pair<double,double> interval = find_slice_boundaries_stepping_out(x0,g,logy,w,m);
  double L = interval.first;
  double R = interval.second;

  // Sample from the interval, shrinking it on each rejection

  return search_interval(x0,L,R,g,logy);
}

double slice_sample(slice_function& g, double w, int m)
{
  double x0 = g.current_value();
  return slice_sample(x0,g,w,m);
}

inline static void dec(double& x, double w, const slice_function& g, bool& hit_lower_bound)
{
  x -= w;
  if (g.below_lower_bound(x)) {
    x = g.lower_bound;
    hit_lower_bound=true;
  }
}

inline static void inc(double& x, double w, const slice_function& g, bool& hit_upper_bound)
{
  x += w;
  if (g.above_upper_bound(x)) {
    x = g.upper_bound;
    hit_upper_bound = true;
  }
}


// Assumes uni-modal
std::pair<double,double> find_slice_boundaries_search(double& x0,slice_function& g,double logy,
						      double w,int /*m*/)
{
  // the initial point should be within the bounds, if the exist
  assert(not g.below_lower_bound(x0));
  assert(not g.above_upper_bound(x0));

  bool hit_lower_bound=false;
  bool hit_upper_bound=false;

  double gx0 = g(x0);

  double L = x0;
  double R = L;
  inc(R,w,g,hit_upper_bound);
  
  //  if (gx > logy) return find_slice_boundaries_stepping_out(x,g,logy,w,m,lower_bound,lower,upper_bound,upper);

  int state = -1;

  while(1)
  {
    double gL = g(L);
    double gR = g(R);

    if (gx0 < logy and gL > gx0) {
      x0 = L;
      gx0 = gL;
    }

    if (gx0 < logy and gR > gx0) {
      x0 = R;
      gx0 = gR;
    }

    // below bound, and slopes upwards to the right
    if (gR < logy and gL < gR) 
    {
      if (state == 2)
	break;
      else if (state == 1) {
	inc(R,w,g,hit_upper_bound);
	break;
      }

      state = 0;
      hit_lower_bound=false;
      L = R;
      inc(R,w,g,hit_upper_bound);
    }
    // below bound, and slopes upwards to the left
    else if (gL < logy and gR < gL) 
    {
      if (state == 2)
	break;
      if (state == 1)
      {
	dec(L,w,g,hit_lower_bound);
	break;
      }

      state = 1;
      hit_upper_bound=false;
      R = L;
      dec(L,w,g,hit_lower_bound);
    }
    else {
      state = 2;
      bool moved = false;
      if (gL >= logy and not hit_lower_bound) {
	moved = true;
	dec(L,w,g,hit_lower_bound);
      }

      if (gR >= logy and not hit_upper_bound) {
	moved = true;
	inc(R,w,g,hit_upper_bound);
      }

      if (not moved) break;
    }
  }

  return std::pair<double,double>(L,R);
}


double sample_from_interval(const std::pair<double,double>& interval)
{
  return interval.first + uniform()*(interval.second - interval.first);
}

std::pair<int,double> search_multi_intervals(vector<double>& X0,
					     vector< std::pair<double,double> >& intervals,
					     vector< slice_function* >& g,
					     double logy)
{
  const int N = X0.size();
  assert(intervals.size() == N);
  assert(g.size() == N);

  assert((*g[0])(X0[0]) >= logy);

  while(1) 
  {
    // calculate slice sizes
    vector<double> slice_sizes(N);
    for(int i=0;i<N;i++)
      slice_sizes[i] = intervals[i].second - intervals[i].first;

    // choose an alternative to try to sample a slice from
    int C = choose(slice_sizes);

    double& L = intervals[C].first;
    double& R = intervals[C].second;
    double& x0 = X0[C];

    // std::cerr<<" C = "<<C<<std::endl;
    assert(L <= x0 and x0 <= R);
    
    double x1 = sample_from_interval(intervals[C]);
    
    double gx1 = (*g[C])(x1);
    
    if (gx1 >= logy) return std::pair<int,double>(C,x1);
    
    double gx0 = (*g[C])(X0[C]);
    
    //      std::cerr<<"L2 = "<<L2<<"   x0_2 = "<<x0_2<<"   R2 = "<<R2<<"     x1 = "<<x1<<"\n";
    
    if (x1 > x0) {
      if (gx1 > gx0) {
	L = x0;
	x0 = x1;
      }
      else
	R = x1;
    }
    else {
      if (gx1 > gx0) {
	R = x0;
	x0 = x1;
      }
      else
	L = x1;
    }
    assert(intervals[C].first <= X0[C] and X0[C] < intervals[C].second);
  }
}
					   
std::pair<int,double> slice_sample_multi(vector<double>& X0, vector<slice_function*>& g, double w, int m)
{
  int N = g.size();

  double g1x0 = (*g[0])();

  assert(std::abs(g1x0 - (*g[0])(X0[0])) < 1.0e-9);

  // Determine the slice level, in log terms.

  double logy = g1x0 - exponential(1);

  // Find the initial interval to sample from - in G[0]

  vector<std::pair<double,double> > intervals(N);

  intervals[0] = find_slice_boundaries_stepping_out(X0[0],*g[0],logy,w,m);

  for(int i=1;i<N;i++)
    intervals[i] = find_slice_boundaries_search(X0[i],*g[i],logy,w,m);

  // Sample from the intervals, shrinking them on each rejection
  return search_multi_intervals(X0,intervals,g,logy);
}

std::pair<int,double> slice_sample_multi(double x0, vector<slice_function*>& g, double w, int m)
{
  int N = g.size();

  vector<double> X0(N,x0);

  return slice_sample_multi(X0,g,w,m);
}

std::pair<int,double> slice_sample_multi(vector<slice_function*>& g, double w, int m)
{
  double x0 = g[0]->current_value();
  return slice_sample_multi(x0,g,w,m);
}

double transform_epsilon(double lambda_E)
{
  assert(lambda_E < 0);
  double E_length = lambda_E - logdiff(0,lambda_E);

  return E_length;
}

double inverse_epsilon(double E_length)
{
  double lambda_E = E_length - logsum(0,E_length);

  return lambda_E;
}
