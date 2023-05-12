/*
  Copyright (C) 2008,2010 Benjamin Redelings

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
/// \file slice-sampling.H
///
/// \brief This file defines classes and functions for uniform slice sampling.
///

#ifndef SLICE_SAMPLING_H
#define SLICE_SAMPLING_H

#include <utility>                 // for pair
#include <vector>                  // for vector
#include "computation/context.H"   // for context
#include "util/bounds.H"           // for bounds
#include "util/math/log-double.H"  // for log_double_t

class Parameters;

class variables_changed_exception: public myexception
{
    using myexception::myexception;
};

/// A function interface object for use in slice sampling
struct slice_function: public bounds<double>
{
    /// Compute the value of the function evaluated at x 
    virtual double operator()(double x)=0;

    /// Compute the value of the function evaluated at the current value
    virtual double operator()()=0;

    /// Return the current value of x
    virtual double current_value() const;

    virtual void reset() {std::abort();};

    slice_function() {}
    slice_function(const bounds<double>& b):bounds<double>(b) {}
    virtual ~slice_function() {}
};

double slice_sample(double x0, slice_function& g, double w, int m);

double slice_sample(slice_function& g, double w, int m);

std::pair<int,double> slice_sample_multi(double x0, std::vector<slice_function*>& g, double w, int m);

std::pair<int,double> slice_sample_multi(std::vector<double>& X0, std::vector<slice_function*>& g, double w, int m);

std::pair<int,double> slice_sample_multi(std::vector<slice_function*>& g, double w, int m);

class context_slice_function: public slice_function
{
protected:
    log_double_t current_fn_value;

public:

    int count = 0;

    // The original starting point
    context C0;

    // The proposed point
    context_ref& C;

    double operator()(double);

    virtual double operator()();

    virtual void set_value(double x) = 0;

    virtual double current_value() const = 0;

    void reset();

    context_slice_function(context_ref&);

    context_slice_function(context_ref&, const bounds<double>&);
};

/// A slice_function for slice-sampling a single parameter
struct random_variable_slice_function: public context_slice_function
{
    /// The random_variable index.
    int r_mod;

    void set_value(double x);

    double current_value() const;

    random_variable_slice_function(context_ref&, const bounds<double>&, int);
};


/// A slice_function for slice-sampling a single parameter
struct integer_random_variable_slice_function:public context_slice_function
{
    /// The random_variable index.
    int r_mod;

    void set_value(double x);

    double current_value() const;

    integer_random_variable_slice_function(context_ref&, const bounds<int>&, int);
};


/// A slice_function for slice-sampling a single branch length
struct branch_length_slice_function: public context_slice_function
{
    int b;

    void set_value(double);

    double current_value() const;

    branch_length_slice_function(Parameters&,int);
};

/// A slice_function for slice-sampling a single branch length
struct alignment_branch_length_slice_function: public context_slice_function
{
    log_double_t alignment_sum_ratio_0;
    int b;

    double operator()() {return context_slice_function::operator()();};
    double operator()(double x);

    void set_value(double);

    double current_value() const;

    alignment_branch_length_slice_function(Parameters&,int);
};

/// A slice_function for slice-sampling two adjacent branch lengths which preserving their sum
struct slide_node_slice_function: public context_slice_function
{
    int b1;
    int b2;
    double x0;
    double y0;

    void set_value(double);
    double current_value() const;

    slide_node_slice_function(Parameters&,int);
    slide_node_slice_function(Parameters&,int,int);
};

/// \brief A slice_function for changing only the branch length mean
///
/// This function is parameterized in terms of t, where
///   mu'[i] = mu[i] * exp(t)     for i=1..n branch means.
///   T'[i]  = T[i]  * exp(-t)    for i=1..B tranch lengths.
///
/// The slices are levels of \f$Pr(x)\times(\sum_i \mu_i)^{n-B}\f$ where
///   n = the number of means,
///   B = the number of branch lengths.
///
struct scale_means_only_slice_function: public context_slice_function
{
    /// The sum of the branch mean parameters for the initial point.
    double log_current_factor=0;

    double initial_average_scale;
    std::vector<double> initial_scales;
    std::vector<double> initial_branch_lengths;

    double log_average_scale() const;

    double operator()(double x) {return context_slice_function::operator()(x);}

    // We need to override the probability_ratio to add a jacobian.
    double operator()();

    void set_value(double);

    double current_value() const;

    scale_means_only_slice_function(Parameters&);
};

/// \brief A slice_function for changing only the branch length mean
///
/// This function is parameterized in terms of t, where
///   mu'[i] = mu[i] * exp(t)     for i=1..n branch means.
///   T'[i]  = T[i]  * exp(-t)    for i=1..B tranch lengths.
///
/// The slices are levels of \f$Pr(x)\times(\sum_i \mu_i)^{n-B}\f$ where
///   n = the number of means,
///   B = the number of branch lengths.
///
struct scale_means_only_slice_function2: public context_slice_function
{
    std::vector<int> r_scales;
    std::vector<int> r_branch_lengths;

    /// The sum of the branch mean parameters for the initial point.
    double log_current_factor=0;

    double initial_average_scale;
    std::vector<double> initial_scales;
    std::vector<double> initial_branch_lengths;

    int n_scales() const {return r_scales.size();}
    int n_branch_lengths() const {return r_branch_lengths.size();}

    double log_average_scale() const;

    double operator()(double x) {return context_slice_function::operator()(x);}

    // We need to override the probability_ratio to add a jacobian.
    double operator()();

    void set_value(double);

    double current_value() const;

    scale_means_only_slice_function2(context_ref&, const std::vector<int>&, const std::vector<int>&);
};

/// \brief A slice_function for resampling \f$x_n\f$ when \f$\sum_i x_i=C\f$
///
/// This function is parameterized in terms of t, where
///  x'[i] = x[i] * (C-t)/(C-x[n])   if (i!=n)
///  x'[n] = t
///
/// The slices are levels of \f$Pr(x)\times(C-x[n])^{N-1}$ where
///  N = the dimension of x.
///
struct constant_sum_modifiable_slice_function: public context_slice_function
{
    /// Which indices form the dirichlet parameter that we are resampling?
    std::vector<int> indices;

    /// Which of the above indices is the one that we are resampling?
    int n;

    double operator()(double x) {return context_slice_function::operator()(x);}

    /// return the probability * mu^(B-1)
    double operator()();

    void set_value(double);

    double current_value() const;

    constant_sum_modifiable_slice_function(context_ref&, const std::vector<int>&, int);
};

#endif
