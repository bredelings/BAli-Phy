#ifndef SLICE_SAMPLING_H
#define SLICE_SAMPLING_H

#include "parameters.H"

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

  parameter_slice_function(Parameters&,int);
};

struct branch_length_slice_function:public slice_function
{
  int count;

  Parameters& P;

  int b;

  double operator()(double);

  branch_length_slice_function(Parameters&,int);
};

#endif
