#include "slice-sampling.H"
#include "rng.H"
#include "choose.H"

namespace slice_sampling {
  double identity(double x) {return x;}
}



double parameter_slice_function::operator()(double x)
{
  count++;
  P.parameter(n,inverse(x));
  return log(P.probability());
}

double parameter_slice_function::operator()()
{
  count++;
  return log(P.probability());
}

parameter_slice_function::parameter_slice_function(Parameters& P_,int n_)
  :count(0),P(P_),n(n_),
   transform(slice_sampling::identity),
   inverse(slice_sampling::identity)
{ }

parameter_slice_function::parameter_slice_function(Parameters& P_,int n_,
						   double(*f1)(double),
						   double(*f2)(double))
  :count(0),P(P_),n(n_),transform(f1),inverse(f2)
{ }

double branch_length_slice_function::operator()(double l)
{
  count++;
  P.setlength(b,l);
  return log(P.probability());
}

double branch_length_slice_function::operator()()
{
  count++;
  return log(P.probability());
}

branch_length_slice_function::branch_length_slice_function(Parameters& P_,int b_)
  :count(0),P(P_),b(b_)
{ }


std::pair<double,double> 
find_slice_boundaries_stepping_out(double x0,slice_function& g,double logy,
				   double w,int m,
				   bool lower_bound,double lower,
				   bool upper_bound,double upper)
{
  double u = uniform()*w;
  double L = x0 - u;
  double R = x0 + (w-u);

  // Expand the interval until its ends are outside the slice, or until
  // the limit on steps is reached.

  if (m>1) {
    int J = floor(uniform()*m);
    int K = (m-1)-J;

    while (J>0 and ((not lower_bound) or L>lower) and g(L)>logy) {
      L -= w;
      J--;
    }

    while (K>0 and ((not upper_bound) or R<upper) and g(R)>logy) {
      R += w;
      K--;
    }
  }
  else {
    while (((not lower_bound) or L>lower) and g(L)>logy)
      L -= w;

    while (((not upper_bound) or R<upper) and g(R)>logy)
      R += w;
  }

  // Shrink interval to lower and upper bounds.

  if (lower_bound and L<lower) L = lower;
  if (upper_bound and R>upper) R = upper;

  return std::pair<double,double>(L,R);
}

// Does this x0 really need to be the original point?
// I think it just serves to let you know which way the interval gets shrunk...

double search_interval(double x0,double& L, double& R, slice_function& g,double logy)
{
  assert(g(x0) > g(L) and g(x0) > g(R));
  assert(g(x0) > logy);

  while(1)
  {
    double x1 = L + uniform()*(R-L);

    double gx1 = g(x1);

    if (gx1 >= logy) return x1;

    if (x1 > x0) 
      R = x1;
    else
      L = x1;
  }
}

double slice_sample(double x0, slice_function& g,
		    double w, int m, 
		    bool lower_bound,
		    double lower, 
		    bool upper_bound,
		    double upper)
{
  double gx0 = g();

  assert(std::abs(gx0 - g(x0)) < 1.0e-9);

  // Determine the slice level, in log terms.

  double logy = gx0 - exponential(1);

  // Find the initial interval to sample from.

  std::pair<double,double> interval = find_slice_boundaries_stepping_out(x0,g,logy,w,m,lower_bound,lower,upper_bound,upper);
  double L = interval.first;
  double R = interval.second;

  // Sample from the interval, shrinking it on each rejection

  return search_interval(x0,L,R,g,logy);
}

inline static void dec(double& x,double w, bool lb, double lower,bool& hit_lower_bound)
{
  x -= w;
  if (lb and (x<lower)) {
    x = lower;
    hit_lower_bound=true;
  }
}

inline static void inc(double& x,double w, bool ub, double upper,bool& hit_upper_bound)
{
  x += w;
  if (ub and (x<upper)) 
  {
    x = upper;
    hit_upper_bound = true;
  }
}


// Assumes uni-modal
std::pair<double,double> find_slice_boundaries_search(double& x0,slice_function& g,double logy,
						      double w,int m,
						      bool lower_bound,double lower,
						      bool upper_bound,double upper)
{
  // the initial point should be within the bounds, if the exist
  assert(not lower_bound or (x0 > lower));
  assert(not upper_bound or (x0 < upper));

  bool hit_lower_bound=false;
  bool hit_upper_bound=false;

  double gx0 = g(x0);

  double L = x0;
  double R = L;
  inc(R,w,upper_bound,upper,hit_upper_bound);
  
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
	inc(R,w,upper_bound,upper,hit_upper_bound);
	break;
      }

      state = 0;
      hit_lower_bound=false;
      L = R;
      inc(R,w,upper_bound,upper,hit_upper_bound);
    }
    // below bound, and slopes upwards to the left
    else if (gL < logy and gR < gL) 
    {
      if (state == 2)
	break;
      if (state == 1)
      {
	dec(L,w,lower_bound,lower,hit_lower_bound);
	break;
      }

      state = 1;
      hit_upper_bound=false;
      R = L;
      dec(L,w,lower_bound,lower,hit_lower_bound);
    }
    else {
      state = 2;
      bool moved = false;
      if (gL >= logy and not hit_lower_bound) {
	moved = true;
	dec(L,w,lower_bound,lower,hit_lower_bound);
      }

      if (gR >= logy and not hit_upper_bound) {
	moved = true;
	inc(R,w,upper_bound,upper,hit_upper_bound);
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
					   

std::pair<int,double> slice_sample_multi(double x0, vector<slice_function*>& g,
					 double w, int m, 
					 bool lower_bound,
					 double lower, 
					 bool upper_bound,
					 double upper)
{
  int N = g.size();

  double g1x0 = (*g[0])();

  assert(std::abs(g1x0 - (*g[0])(x0)) < 1.0e-9);

  // Determine the slice level, in log terms.

  double logy = g1x0 - exponential(1);

  // Find the initial interval to sample from - in G[0]

  vector<double> X0(N,x0);

  vector<std::pair<double,double> > intervals(N);

  intervals[0] = find_slice_boundaries_stepping_out(X0[0],*g[0],logy,w,m,lower_bound,lower,upper_bound,upper);

  for(int i=1;i<N;i++)
    intervals[i] = find_slice_boundaries_search(X0[i],*g[i],logy,w,m,lower_bound,lower,upper_bound,upper);

  // Sample from the intervals, shrinking them on each rejection
  return search_multi_intervals(X0,intervals,g,logy);
}

double transform_epsilon(double lambda_E)
{
  double E_length = lambda_E - logdiff(0,lambda_E);

  return E_length;
}

double inverse_epsilon(double E_length)
{
  double lambda_E = E_length - logsum(0,E_length);

  return lambda_E;
}
