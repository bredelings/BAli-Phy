#ifndef SLICE_SAMPLING_H
#define SLICE_SAMPLING_H

#include "parameters.H"

namespace slice_sampling {
  double identity(double x);
}

struct slice_function {
  virtual double operator()(double)=0;
};


double slice_sample(double x0, slice_function& g,
		    double w, int m, 
		    bool lower_bound,
		    double lower, 
		    bool upper_bound,
		    double upper);

struct parameter_slice_function:public slice_function
{
  int count;

  Parameters& P;

  int n;

  double operator()(double);

  // function to go from the stored value to the value on which the prior is.
  double (*transform)(double);
  // function to go from the value on which the prior is to the stored value.
  double (*inverse)(double);

  parameter_slice_function(Parameters&,int);
  parameter_slice_function(Parameters&,int,double(*)(double),double(*)(double));
};


struct branch_length_slice_function:public slice_function
{
  int count;

  Parameters& P;

  int b;

  double operator()(double);

  branch_length_slice_function(Parameters&,int);
};

double transform_epsilon(double);
double inverse_epsilon(double);

#endif
