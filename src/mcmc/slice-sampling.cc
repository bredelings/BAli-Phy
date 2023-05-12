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

#include <tuple>
#include "util/assert.hh"
#include "slice-sampling.H"
#include "util/rng.H"
#include "probability/choose.H"
#include "mcmc/sample-alignment.H"

extern int log_verbose;

using std::vector;

double slice_function::current_value() const
{
    std::abort();
}

// ********************************** modifiable slice function ****************************************** //
context_slice_function::context_slice_function(context_ref& c)
    :context_slice_function(c, {})
{}

context_slice_function::context_slice_function(context_ref& c, const bounds<double>& b)
    :slice_function(b), C0(c), C(c)
{
    current_fn_value.log() = 0;
}

double context_slice_function::operator()(double x)
{
    count++;

    // We are intentionally only calling context::operator==( ) here.
    // Maybe we should actually call merely context::operator==( ) though?
    if (count == 1) C0.evaluate_program();
    C = C0;
    set_value(x);

    // Here is where we return 0 if the number of variables changes.
    // How can we automate this so that it is called only once?
    auto ratio = C.heated_probability_ratios(C0);
    if (ratio.variables_changed)
        throw variables_changed_exception("Variable changed during slice sampling!");

    current_fn_value = ratio.total_ratio();

    return operator()();
}

double context_slice_function::operator()()
{
    return log(current_fn_value);
}

void context_slice_function::reset()
{
    C = C0;
}

void random_variable_slice_function::set_value(double x)
{
    C.set_modifiable_value(r_mod, x);
}

double random_variable_slice_function::current_value() const
{
    return C.get_modifiable_value(r_mod).as_double();
}

random_variable_slice_function::random_variable_slice_function(context_ref& c, const bounds<double>& bounds, int rv)
    :context_slice_function(c, bounds)
{
    if (auto m = C.find_modifiable_reg(rv))
	r_mod = *m;
    else
	throw myexception()<<"No modifiable reg for slice function!";
}

// ******************************* integer random_variable slice function *************************************** //
bounds<double> convert_bounds(const bounds<int>& int_bounds)
{
    bounds<double> double_bounds = int_bounds;
    if (double_bounds.upper_bound)
        double_bounds.upper_bound = *double_bounds.upper_bound + 1;

    return double_bounds;
}

void integer_random_variable_slice_function::set_value(double x)
{
    int x_integer = (int)floor(x);
    C.set_modifiable_value(r_mod, x_integer);
}

double integer_random_variable_slice_function::current_value() const
{
    return C.get_modifiable_value(r_mod).as_int();
}

integer_random_variable_slice_function::integer_random_variable_slice_function(context_ref& c, const bounds<int>& bounds, int rv)
    :context_slice_function(c, convert_bounds(bounds))
{
    if (auto m = C.find_modifiable_reg(rv))
	r_mod = *m;
    else
	throw myexception()<<"No modifiable reg for slice function!";
}

// ******************************* branch length slice function *************************************** //

void branch_length_slice_function::set_value(double l)
{
    static_cast<Parameters&>(C).setlength(b,l);
}

double branch_length_slice_function::current_value() const
{
    return static_cast<Parameters&>(C).t().branch_length(b);
}

branch_length_slice_function::branch_length_slice_function(Parameters& P,int b_)
    :context_slice_function(P),b(b_)
{ 
    set_lower_bound(0);
}

double alignment_branch_length_slice_function::operator()(double x)
{
    count++;

    // We are intentionally only calling context::operator==( ) here.
    // Maybe we should actually call merely context::operator==( ) though?
    if (count == 1) C0.evaluate_program();
    C = C0;
    set_value(x);
    auto alignment_sum_ratio_1 = sample_alignment(static_cast<Parameters&>(C),b);

    // Here is where we return 0 if the number of variables changes.
    // How can we automate this so that it is called only once?
    current_fn_value = C.heated_probability_ratio(C0)*alignment_sum_ratio_1/alignment_sum_ratio_0;
    return operator()();
}

void alignment_branch_length_slice_function::set_value(double l)
{
    static_cast<Parameters&>(C).setlength(b,l);
}

double alignment_branch_length_slice_function::current_value() const
{
    return static_cast<Parameters&>(C).t().branch_length(b);
}

alignment_branch_length_slice_function::alignment_branch_length_slice_function(Parameters& P,int b_)
    :context_slice_function(P),b(b_)
{ 
    set_lower_bound(0);
    alignment_sum_ratio_0 = sample_alignment(P, b);
    C0 = P;
}

void slide_node_slice_function::set_value(double x) 
{
    /* Problem: If y << x, then we may not be able to recover y = (x+y)-x.
       This occurs, for example, when x = 1 and y = 1.0e-9.

       Solution: Store x0 and y0 separately, and compute y = y0 + (x0-x).
    */
    assert(0 <= x and x <= (x0+y0));
    static_cast<Parameters&>(C).setlength(b1,x);
    static_cast<Parameters&>(C).setlength(b2,y0+(x0-x));
}

double slide_node_slice_function::current_value() const
{
    return static_cast<Parameters&>(C).t().branch_length(b1);
}

slide_node_slice_function::slide_node_slice_function(Parameters& P,int b0)
    :context_slice_function(P)
{
    vector<int> b = P.t().branches_after(b0);

    if (b.size() != 2)
	throw myexception()<<"pointing to leaf node!";

    b1 = b[0];
    b2 = b[1];

    x0 = P.t().branch_length(b[0]);
    y0 = P.t().branch_length(b[1]);

    set_lower_bound(0);
    set_upper_bound(x0+y0);
}

slide_node_slice_function::slide_node_slice_function(Parameters& P,int i1,int i2)
    :context_slice_function(P), b1(i1), b2(i2)
{
    x0 = P.t().branch_length(b1);
    y0 = P.t().branch_length(b2);

    set_lower_bound(0);
    set_upper_bound(x0+y0);
}


/*
 * Joint scaling of branch lengths and scales so that the T*R=D remains constant.
 */

void scale_means_only_slice_function::set_value(double t)
{
    log_current_factor = t;
    double scale = exp(log_current_factor);

    auto& P = static_cast<Parameters&>(C);

    // Scale the branch-scaling factor for each partition.
    for(int i=0; i<initial_scales.size(); i++)
	P.set_branch_scale(i, initial_scales[i]*scale);

    // Scale the branch lengths in the opposite direction
    for(int b=0; b<initial_branch_lengths.size(); b++)
	P.setlength_unsafe(b, initial_branch_lengths[b]/scale);
}

double scale_means_only_slice_function::log_average_scale() const
{
    return log(initial_average_scale) + log_current_factor;
}

double scale_means_only_slice_function::operator()()
{
    // return pi * (\sum_i \mu_i)^(n-B)
    return context_slice_function::operator()() + log_average_scale()*((int)initial_scales.size() - (int)initial_branch_lengths.size());
}

double scale_means_only_slice_function::current_value() const
{
    return log_current_factor;
}

scale_means_only_slice_function::scale_means_only_slice_function(Parameters& P)
    :context_slice_function(P)
{ 
    // 1. Set up initial scales
    if (P.n_branch_scales() == 0)
        throw myexception()<<"Can't do scale_means_only_slice function if there are no scales!";

    initial_average_scale = 0;
    initial_scales.resize(P.n_branch_scales());
    for(int i=0;i<initial_scales.size();i++)
    {
        initial_scales[i] = P.get_branch_scale(i);
        initial_average_scale += initial_scales[i];
    }
    initial_average_scale /= initial_scales.size();

    // FIXME: We should be able to assert that all of the scales are modifiable.

    // 3. Set up initial branch_lengths
    initial_branch_lengths.resize(P.t().n_branches());
    for(int b=0; b<initial_branch_lengths.size(); b++)
        initial_branch_lengths[b] = P.t().branch_length(b);

    // FIXME: We should be able to assert that all of the branch lengths are modifiable.

    // 4. Set bounds on the scale factor to avoid numeric overflow/underflow.
    bounds<double>& b = *this;

    // We want to require that
    //     log(average_scale)                              \in [-40,40]
    //     log(initial_average_scale) + log_current_factor \in [-40,40]
    //                                  log_current_factor \in [-40 - log(initial_average_scale), 40 - log(initial_average_scale)]
    double shift = -log(initial_average_scale);

    b = between<double>(-40+shift,40+shift);

#ifndef NDEBUG
    std::clog<<"bounds on t are "<<b<<std::endl;
#endif
}

/*
 * Joint scaling of branch lengths and scales so that the T*R=D remains constant.
 */

void scale_means_only_slice_function2::set_value(double t)
{
    log_current_factor = t;
    double scale = exp(log_current_factor);

    // Scale the branch-scaling factor for each partition.
    for(int i=0; i<initial_scales.size(); i++)
	C.set_modifiable_value(r_scales[i], initial_scales[i]*scale);

    // Scale the branch lengths in the opposite direction
    for(int b=0; b<initial_branch_lengths.size(); b++)
	C.set_modifiable_value(r_branch_lengths[b], initial_branch_lengths[b]/scale);
}

double scale_means_only_slice_function2::log_average_scale() const
{
    return log(initial_average_scale) + log_current_factor;
}

double scale_means_only_slice_function2::operator()()
{
    // return pi * (\sum_i \mu_i)^(n-B)
    return context_slice_function::operator()() + log_average_scale()*((int)initial_scales.size() - (int)initial_branch_lengths.size());
}

double scale_means_only_slice_function2::current_value() const
{
    return log_current_factor;
}

scale_means_only_slice_function2::scale_means_only_slice_function2(context_ref& C, const std::vector<int>& ss, const std::vector<int>& ls)
    :context_slice_function(C),
     r_scales(ss),
     r_branch_lengths(ls)
{ 
    // 1. Set up initial scales
    if (n_scales() == 0)
        throw myexception()<<"Can't do scale_means_only_slice function if there are no scales!";

    initial_average_scale = 0;
    initial_scales.resize(n_scales());
    for(int i=0;i<initial_scales.size();i++)
    {
        initial_scales[i] = C.get_modifiable_value(r_scales[i]).as_double();
        initial_average_scale += initial_scales[i];
    }
    initial_average_scale /= initial_scales.size();

    // FIXME: We should be able to assert that all of the scales are modifiable.

    // 3. Set up initial branch_lengths
    initial_branch_lengths.resize(n_branch_lengths());
    for(int b=0; b<initial_branch_lengths.size(); b++)
        initial_branch_lengths[b] = C.get_modifiable_value(r_branch_lengths[b]).as_double();

    // FIXME: We should be able to assert that all of the branch lengths are modifiable.

    // 4. Set bounds on the scale factor to avoid numeric overflow/underflow.
    bounds<double>& b = *this;

    // We want to require that
    //     log(average_scale)                              \in [-40,40]
    //     log(initial_average_scale) + log_current_factor \in [-40,40]
    //                                  log_current_factor \in [-40 - log(initial_average_scale), 40 - log(initial_average_scale)]
    double shift = -log(initial_average_scale);

    b = between<double>(-40+shift,40+shift);

#ifndef NDEBUG
    std::clog<<"bounds on t are "<<b<<std::endl;
#endif
}

void constant_sum_modifiable_slice_function::set_value(double t)
{
    auto& P = static_cast<Parameters&>(C);
    const int N = indices.size();

    vector<double> x(N);
    for(int i=0;i<N;i++)
	x[i] = P.get_modifiable_value(indices[i]).as_double();

    double total = sum(x);

    double factor = (total - t)/(total - x[n]);

    for(int i=0;i<indices.size();i++)
	if (i == n)
	    x[i] = t;
	else
	    x[i] *= factor;

    assert(std::abs(sum(x) - total) < 1.0e-9);

    for(int i=0;i<N;i++)
	P.set_modifiable_value(indices[i], x[i] );
}


double constant_sum_modifiable_slice_function::operator()()
{
    auto& P = static_cast<Parameters&>(C);
    const int N = indices.size();

    double total = 0;
    for(int i=0;i<N;i++)
	total += P.get_modifiable_value(indices[i]).as_double();

    double t = current_value();

    // return pi * (1-x)^(N-1)
    return context_slice_function::operator()() + (N-1)*log(total-t);
}

double constant_sum_modifiable_slice_function::current_value() const
{
    auto& P = static_cast<Parameters&>(C);
    return P.get_modifiable_value(indices[n]).as_double();
}

constant_sum_modifiable_slice_function::constant_sum_modifiable_slice_function(context_ref& c, const vector<int>& indices_,int n_)
    :context_slice_function(c),
     indices(indices_),
     n(n_)
{ 
    auto x = (vector<double>)C.get_modifiable_values(indices);
    double total = sum(x);

    set_lower_bound(0);
    set_upper_bound(total);
}

// Note: In debugging slice sampling, remember that results will be different if anywhere
//       else has sampled an extra random number.  This can occur if a parameter does not
//       change the likelihood, but roundoff errors in the last decimal place affect whether
//       the new state is accepted.

std::pair<double,double> 
find_slice_boundaries_stepping_out(double x0,slice_function& g,double logy, double w,int m)
{
    assert(x0 + w > x0);
    assert(g.in_range(x0));

    double u = uniform()*w;
    double L = x0 - u;
    double R = L + w;
    assert(L < x0);
    assert(x0 < R);

    // Expand the interval until its ends are outside the slice, or until
    // the limit on steps is reached.

    //  std::cerr<<"!!    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";
    if (m>1) {
	int J = uniform_int(0,m);
	int K = m-J;

	while (J>0 and (not g.below_lower_bound(L)) and g(L)>logy) {
	    L -= w;
	    J--;
	    //      std::cerr<<" g("<<L<<") = "<<g()<<" > "<<logy<<"\n";
	    //      std::cerr<<"<-    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";
	}

	while (K>0 and (not g.above_upper_bound(R)) and g(R)>logy) {
	    R += w;
	    K--;
	    //      std::cerr<<" g("<<R<<") = "<<g()<<" > "<<logy<<"\n";
	    //      std::cerr<<"->    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";
	}
    }
    else {
	while ((not g.below_lower_bound(L)) and g(L)>logy)
	    L -= w;

	while ((not g.above_upper_bound(R)) and g(R)>logy)
	    R += w;
    }


    assert(L < R);

    //  std::cerr<<"[]    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";

    return {L,R};
}

std::tuple<double,double,std::optional<double>,std::optional<double>>
find_slice_boundaries_doubling(double x0,slice_function& g,double logy, double w, int K)
{
    assert(x0 + w > x0);
    assert(g.in_range(x0));

    double u = uniform()*w;
    double L = x0 - u;
    double R = L + w;
    assert(L < x0);
    assert(x0 < R);

    std::optional<double> gL_cached;
    auto gL = [&]() {
        if (not gL_cached)
            gL_cached = (g.in_range(L))?g(L):log_0;
        return *gL_cached;
    };

    std::optional<double> gR_cached;
    auto gR = [&]() {
        if (not gR_cached)
            gR_cached = (g.in_range(R))?g(R):log_0;
        return *gR_cached;
    };

    auto too_large = [](double L, double R, double w)
    {
        double M = (L+R)/2;
        double W = R-L;
        assert(W > 0);
        bool ok = (L < M) and (M < R) and (W+w > W) and (L-w < L) and (R+w>R);
        return not ok;
    };

    while ( K > 0 and (gL() > logy or gR() > logy))
    {
        if (log_verbose >= 4)
            std::cerr<<"!!    L0 = "<<L<<" (g(L) = "<<gL()<<")  x0 = "<<x0<<"   R0 = "<<R<<" (g(R) = "<<gR()<<")\n";

        double W2 = (R-L);
        if (uniform() < 0.5)
        {
            double L2 = L - W2;
            if (too_large(L2, R, w))
                break;
            L = L2;
            gL_cached = {};
        }
        else
        {
            double R2 = R + W2;
            if (too_large(L, R2, w))
                break;
            R = R2;
            gR_cached = {};
        }

        K --;
    }

    assert(L < R);
    assert( L < (L+R)/2 and (L+R)/2 < R);

    //  std::cerr<<"[]    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";

    return {L,R,gL_cached,gR_cached};
}

// Does this x0 really need to be the original point?
// I think it just serves to let you know which way the interval gets shrunk...

double search_interval(double x0,double L, double R, slice_function& g, double logy)
{
    // Shrink interval to lower and upper bounds.
    if (g.below_lower_bound(L)) L = *g.lower_bound;
    if (g.above_upper_bound(R)) R = *g.upper_bound;

    //  assert(g(x0) > g(L) and g(x0) > g(R));
    assert(g(x0) >= logy);
    assert(L < R);
    assert(L <= x0 and x0 <= R);

    double L0 = L, R0 = R;

    if (log_verbose >= 4)
	std::cerr<<"**    L0 = "<<L0<<"   x0 = "<<x0<<"   R0 = "<<R0<<std::endl;
    for(int i=0;i<200;i++)
    {
	double x1 = L + uniform()*(R-L);
	double gx1 = g(x1);
	if (log_verbose >= 4)
	    std::cerr<<"    L  = "<<L <<"   x = "<<g.current_value()<<"   x = "<<x1<<"  R  = "<<R<<"     g(x) = "<<gx1<<std::endl;

	if (gx1 >= logy)
	    return x1;

	if (x1 > x0) 
	    R = x1;
	else
	    L = x1;
    }
    std::cerr.precision(17);
    std::cerr<<"Warning!  Is size of the interval really ZERO?"<<std::endl;
    double logy_x0 = g(x0);  
    std::cerr<<"    L0 = "<<L0<<"   x0 = "<<x0<<"   R0 = "<<R0<<std::endl;
    std::cerr<<"    L  = "<<L <<"   x = "<<g.current_value()<<"   R  = "<<R<<std::endl;
    std::cerr<<"    log(f(x0)*U)  = "<<logy<<"  log(f(x0)) = "<<logy_x0<<"  log(f(x_current)) = "<<g()<<std::endl;

    std::abort();

    return x0;
}

bool pre_slice_sampling_check_OK(double x0, slice_function& g)
{
    // If x is not in the range then this could be a range that is reduced to avoid loss of precision.
    if (not g.in_range(x0))
    {
        if (log_verbose >= 4) std::cerr<<x0<<" not in range!";
        return false;
    }

    assert(g.in_range(x0));

    double gx0 = g();
    double gx0_v2 = gx0;
    if (log_verbose >= 4)
        gx0_v2 = g(x0);
#ifndef NDEBUG
    else
        gx0_v2 = g(x0);
#endif
    if (std::abs(gx0 - gx0_v2) > 1.0e-9)
        throw myexception()<<"Error: slice_sampling: g() = "<<gx0<<"   g(x0) = "<<gx0_v2<<"   diff = "<<std::abs(gx0 - gx0_v2);

    return true;
}

bool can_propose_same_interval_doubling(double x0, double x1, double w, double L, double R, std::optional<double> gL_cached, std::optional<double> gR_cached, slice_function& g, double log_y)
{
    bool D = false;

    auto gL = [&]() {
        if (not gL_cached)
            gL_cached = (g.in_range(L))?g(L):log_0;
        return *gL_cached;
    };

    auto gR = [&]() {
        if (not gR_cached)
            gR_cached = (g.in_range(R))?g(R):log_0;
        return *gR_cached;
    };

    bool ok = true;
    while (ok and R-L > 1.1*w)
    {
        double M = (R+L)/2;
        assert( L < M and M < R);

        // Check if x0 and x1 are in different halves of the interval.
        if ((x0 < M and x1 >= M) or (x0 >= M and x1 < M))
            D = true;

        if (x1 < M)
        {
            R = M;
            gR_cached = {};
        }
        else
        {
            L = M;
            gL_cached = {};
        }

        if (D and log_y >= gL() and log_y >= gR())
            ok = false;
    }

    // FIXME - this is clunky.  Do we really want to set x by evaluate g( )?
    if (D)
    {
        // We may have set x to L or R, so reset it to the right values.
        if (ok)
            g(x1);
        else
            g(x0);
    }

    return ok;
}

double slice_sample_stepping_out_(double x0, slice_function& g, double w, int m)
{
    // 0. Check that the values are OK
    if (not pre_slice_sampling_check_OK(x0, g))
        return x0;

    // 1. Determine the slice level, in log terms.
    double logy = g() - exponential(1);

    // 2. Find the initial interval to sample from.
    auto [L,R] = find_slice_boundaries_stepping_out(x0,g,logy,w,m);

    // 3. Sample from the interval, shrinking it on each rejection
    return search_interval(x0,L,R,g,logy);
}

// We need to SET the value INSIDE this routine.
// Are we assuming that calling g sets the value?
double slice_sample_doubling_(double x0, slice_function& g, double w, int m)
{
    // 0. Check that the values are OK
    if (not pre_slice_sampling_check_OK(x0, g))
        return x0;

    // 1. Determine the slice level, in log terms.
    double logy = g() - exponential(1);

    // 2. Find the initial interval to sample from.
    auto [L,R,gL_cached,gR_cached] = find_slice_boundaries_doubling(x0,g,logy,w,m);

    // 3. Sample from the interval, shrinking it on each rejection
    double x1 = search_interval(x0,L,R,g,logy);

    // 4. Check that we can propose the same interval from x2
    // We need to SET the value INSIDE this routine if we recompute g().
    if (can_propose_same_interval_doubling(x0, x1, w, L, R, gL_cached, gR_cached, g, logy))
        return x1;
    else
        return x0;
}

double slice_sample(double x0, slice_function& g,double w, int m)
{
    try
    {
        return slice_sample_doubling_(x0, g, w, m);
    }
    catch (variables_changed_exception& e)
    {
        if (log_verbose >= 3) std::cerr<<e.what()<<"\n";
        g.reset();
    }
    return x0;
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
	x = *g.lower_bound;
	hit_lower_bound=true;
    }
}

inline static void inc(double& x, double w, const slice_function& g, bool& hit_upper_bound)
{
    x += w;
    if (g.above_upper_bound(x)) {
	x = *g.upper_bound;
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
