#include "slice-sampling.H"
#include "rng.H"

double parameter_slice_function::operator()(double x)
{
  count++;
  P.parameter(n,x);
  return log(P.probability());
}

parameter_slice_function::parameter_slice_function(Parameters& P_,int n_)
  :count(0),P(P_),n(n_)
{ }

double branch_length_slice_function::operator()(double l)
{
  count++;
  P.setlength(b,l);
  return log(P.probability());
}

branch_length_slice_function::branch_length_slice_function(Parameters& P_,int b_)
  :count(0),P(P_),b(b_)
{ }


double slice_sample(double x0, slice_function& g,
		    double w, int m, 
		    bool lower_bound,
		    double lower, 
		    bool upper_bound,
		    double upper)
{
  double gx0 = g(x0);

  // Determine the slice level, in log terms.

  double logy = gx0 - exponential(1);

  // Find the initial interval to sample from.

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

  // Shrink interval to lower and upper bounds.

  if (lower_bound and L<lower) L = lower;
  if (upper_bound and R>upper) R = upper;

  // Sample from the interval, shrinking it on each rejection

  double x1;

  while(1)
  {
    x1 = L + uniform()*(R-L);

    double gx1 = g(x1);

    if (gx1 >= logy) break;

    if (x1 > x0) 
      R = x1;
    else
      L = x1;
  }

  return x1;
}
