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

#ifndef SLICE_SAMPLING_H
#define SLICE_SAMPLING_H

#include "parameters.H"

namespace slice_sampling {
  double identity(double x);
}

struct slice_function 
{
  virtual double operator()(double)=0;

  virtual double operator()()=0;

  virtual double current_value() const;

  bool has_lower_bound;
  double lower_bound;

  bool has_upper_bound;
  double upper_bound;

  bool below_lower_bound(double x) const { return (has_lower_bound and x<lower_bound); }
  bool above_upper_bound(double x) const { return (has_upper_bound and x>upper_bound); }
  bool in_range(double x) const  { return (not below_lower_bound(x) and not above_upper_bound(x));}

  void set_lower_bound(double);
  void set_upper_bound(double);

  slice_function();
};


double slice_sample(double x0, slice_function& g, double w, int m);

double slice_sample(slice_function& g, double w, int m);

std::pair<int,double> slice_sample_multi(double x0, std::vector<slice_function*>& g, double w, int m);

std::pair<int,double> slice_sample_multi(std::vector<double>& X0, std::vector<slice_function*>& g, double w, int m);

std::pair<int,double> slice_sample_multi(std::vector<slice_function*>& g, double w, int m);

struct parameter_slice_function:public slice_function
{
  int count;

  Parameters& P;

  int n;

  double operator()(double);

  double operator()();

  double current_value() const;

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

  double operator()();

  double current_value() const;

  branch_length_slice_function(Parameters&,int);
};

struct slide_node_slice_function: public slice_function {
  int count;
  int b1;
  int b2;
  double total;

  Parameters& P;

  double operator()(double);
  double operator()();
  double current_value() const;
  slide_node_slice_function(Parameters&,int);
  slide_node_slice_function(Parameters&,int,int);
};

double transform_epsilon(double);
double inverse_epsilon(double);

#endif
