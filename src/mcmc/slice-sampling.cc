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

#include <cmath>
#include <numeric>
#include <tuple>
#include <utility>
#include "util/assert.hh"
#include "util/log-level.H"
#include "util/range.H"
#include "slice-sampling.H"
#include "util/rng.H"
#include "probability/choice-weight.H"
#include "mcmc/sample-alignment.H"

extern int log_verbose;

using std::vector;
using std::optional;

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
{}

// Store the base density ratio of the live context C relative to C0.
void context_slice_function::set_context_density_ratio(ProbDensity ratio)
{
    context_density_ratio = std::move(ratio);
}

optional<ProbDensity> context_slice_function::operator()(double x)
{
    if (not std::isfinite(x) or not in_range(x)) return {};

    count++;

    // We are intentionally only calling context::operator==( ) here.
    // Maybe we should actually call merely context::operator==( ) though?
    if (count == 1) C0.evaluate_program();
    C = C0;
    set_value(x);

    auto ratio = C.heated_probability_ratios(C0);
    if (ratio.variables_changed)
        throw variables_changed_exception("Variable changed during slice sampling!");
    else
        set_context_density_ratio(ratio.total_ratio());

    return operator()();
}

ProbDensity context_slice_function::operator()()
{
    return context_density_ratio;
}

// Restore both the live context and its base density ratio relative to C0.
void context_slice_function::reset()
{
    C = C0;
    set_context_density_ratio(1);
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

// ******************************* branch length-or-duration slice function *************************** //

void branch_length_or_duration_slice_function::set_value(double x)
{
    static_cast<Parameters&>(C).t().set_branch_length_or_duration(b,x);
}

double branch_length_or_duration_slice_function::current_value() const
{
    return static_cast<const Parameters&>(C).t().branch_length_or_duration(b);
}

branch_length_or_duration_slice_function::branch_length_or_duration_slice_function(Parameters& P,int b_)
    :context_slice_function(P),b(b_)
{ 
    set_lower_bound(0);
}

// ******************************* node time length slice function *************************************** //

void node_time_slice_function::set_value(double t)
{
    static_cast<Parameters&>(C).t().set_node_time(n,t);
}

double node_time_slice_function::current_value() const
{
    return static_cast<Parameters&>(C).t().node_time(n);
}

node_time_slice_function::node_time_slice_function(Parameters& P,int n_)
    :context_slice_function(P),n(n_)
{
}

// ******************************* alignment and branch length-or-duration slice function ************* //

optional<ProbDensity> alignment_branch_length_or_duration_slice_function::operator()(double x)
{
    if (not std::isfinite(x) or not in_range(x)) return {};

    count++;

    // We are intentionally only calling context::operator==( ) here.
    // Maybe we should actually call merely context::operator==( ) though?
    if (count == 1) C0.evaluate_program();
    C = C0;
    set_value(x);

    // Pass 'false' because the initial alignment may have zero probability under the new branch value x.
    // Without this, check_sampling_probabilities may throw an exception.
    auto alignment_sum_ratio_1 = sample_alignment(static_cast<Parameters&>(C), b, false);
    if (not alignment_sum_ratio_1)
    {
        C = C0;
        set_context_density_ratio(1);
        return std::nullopt;
    }

    assert(alignment_sum_ratio_0);
    set_context_density_ratio(C.heated_probability_ratio(C0) *
                              (*alignment_sum_ratio_1 / *alignment_sum_ratio_0));
    return operator()();
}

void alignment_branch_length_or_duration_slice_function::set_value(double x)
{
    static_cast<Parameters&>(C).t().set_branch_length_or_duration(b,x);
}

double alignment_branch_length_or_duration_slice_function::current_value() const
{
    return static_cast<Parameters&>(C).t().branch_length_or_duration(b);
}

alignment_branch_length_or_duration_slice_function::alignment_branch_length_or_duration_slice_function(Parameters& P,
                                                                                                       int b_)
    :context_slice_function(P),b(b_)
{ 
    set_lower_bound(0);
    alignment_sum_ratio_0 = sample_alignment(P, b);
    C0 = P;
}

// Convert a log coordinate ratio back to two stored lengths or durations while preserving their sum.
// Computing changes from the initial pair retains precision for nearby proposals.
void slide_node_slice_function::set_value(double z)
{
    const double delta = z - initial_z;
    const double total = initial_x + initial_y;
    double x;
    double y;

    if (delta < 0)
    {
        const double ratio_change = expm1(delta);
        const double denominator = initial_y + initial_x * (1 + ratio_change);
        const double dx = initial_x * (initial_y / denominator) * ratio_change;
        x = initial_x + dx;
        y = initial_y - dx;
    }
    else
    {
        const double ratio_change = expm1(-delta);
        const double denominator = initial_x + initial_y * (1 + ratio_change);
        const double dy = initial_y * (initial_x / denominator) * ratio_change;
        x = initial_x - dy;
        y = initial_y + dy;
    }

    assert(0 <= x and x <= total);
    assert(0 <= y and y <= total);

    auto tree = static_cast<Parameters&>(C).t();
    tree.set_branch_length_or_duration(b1,x);
    tree.set_branch_length_or_duration(b2,y);
}

// Return the log ratio of the two current stored lengths or durations.
double slide_node_slice_function::current_value() const
{
    const auto& tree = static_cast<const Parameters&>(C).t();
    return log(tree.branch_length_or_duration(b1)) - log(tree.branch_length_or_duration(b2));
}

// Include dx/dz = xy/(x+y) when evaluating the density in log-ratio coordinates.
ProbDensity slide_node_slice_function::operator()()
{
    const auto& tree = static_cast<const Parameters&>(C).t();
    const double x = tree.branch_length_or_duration(b1);
    const double y = tree.branch_length_or_duration(b2);
    return context_slice_function::operator()() * ProbDensity(x) * ProbDensity(y) / total_density;
}

slide_node_slice_function::slide_node_slice_function(Parameters& P,int b0)
    :context_slice_function(P)
{
    vector<int> b = P.t().branches_after(b0);

    if (b.size() != 2)
	throw myexception()<<"pointing to leaf node!";

    b1 = b[0];
    b2 = b[1];

    initial_x = P.t().branch_length_or_duration(b[0]);
    initial_y = P.t().branch_length_or_duration(b[1]);
    total_density = ProbDensity(initial_x + initial_y);
    initial_z = log(initial_x) - log(initial_y);
}

/*
 * Joint scaling of branch lengths and scales so that the T*R=D remains constant.
 */

void scale_groups_slice_function::set_value(double t)
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

log_double_t scale_groups_slice_function::current_average_scale() const
{
    return initial_average_scale * exp_to_log_space(log_current_factor);
}

ProbDensity scale_groups_slice_function::operator()()
{
    // Using the average instead of the sum omits only a fixed factor n^(n-B),
    // so the transformed density is proportional to pi * average(mu)^(n-B).
    return context_slice_function::operator()()
         * pow(ProbDensity(current_average_scale()), n_scales() - n_branch_lengths());
}

double scale_groups_slice_function::current_value() const
{
    return log_current_factor;
}

scale_groups_slice_function::scale_groups_slice_function(context_ref& C, const std::vector<int>& ss, const std::vector<int>& ls)
    :context_slice_function(C),
     r_scales(ss),
     r_branch_lengths(ls)
{ 
    // 1. Set up initial scales
    if (n_scales() == 0)
        throw myexception()<<"Can't do scale_means_only_slice function if there are no scales!";

    double average_scale = 0;
    initial_scales.resize(n_scales());
    for(int i=0;i<initial_scales.size();i++)
    {
        initial_scales[i] = C.get_modifiable_value(r_scales[i]).as_double();
        average_scale += std::abs(initial_scales[i]);
    }
    average_scale /= initial_scales.size();
    initial_average_scale = average_scale;

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
    double shift = -initial_average_scale.log();
    assert(std::isfinite(shift));

    b = between<double>(-40+shift,40+shift);

#ifndef NDEBUG
    if (log_verbose >= 4) std::clog<<"bounds on t are "<<b<<std::endl;
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


ProbDensity constant_sum_modifiable_slice_function::operator()()
{
    const int N = indices.size();
    // The transformation preserves C.  Subtract before taking a logarithm: C-t
    // is exact near the boundary, whereas distinct values can have equal rounded logs.
    const double remaining_total = fixed_total - current_value();
    return context_slice_function::operator()() * pow(ProbDensity(remaining_total), N-1);
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
    // NOTE: context_ref still returns a structural RVector; remove this
    // extraction loop when it exposes a direct numeric reduction.
    auto values = C.get_modifiable_values(indices);
    double total = 0;
    for(const auto& value: values)
        total += value.as_double();
    fixed_total = total;

    set_lower_bound(0);
    set_upper_bound(total);
}

// Note: In debugging slice sampling, remember that results will be different if anywhere
//       else has sampled an extra random number.  This can occur if a parameter does not
//       change the likelihood, but roundoff errors in the last decimal place affect whether
//       the new state is accepted.

namespace
{
// Return the ordinary log ratio only when both values have the same exceptional-density rank.
// A different rank cannot be compared within one scalar slice.
optional<double> slice_log_ratio(const ProbDensity& candidate, const ProbDensity& reference)
{
    auto ratio = ChoiceWeight(candidate) / ChoiceWeight(reference);
    if (not std::isfinite(ratio.log()))
        return {};
    return ratio.log();
}

// Test inclusive slice membership within the reference rank.  Out-of-range and
// rank-changing candidates are both outside the scalar slice.
bool inside_slice(const optional<ProbDensity>& candidate, const ProbDensity& slice_level)
{
    if (not candidate)
        return false;
    auto ratio = slice_log_ratio(*candidate, slice_level);
    return ratio and *ratio >= 0;
}

// Draw a slice level without changing the starting exceptional rank.  Treat an
// exceptional floating-point result as the boundary draw U=1.
ProbDensity draw_slice_level(ProbDensity starting_density)
{
    double offset = exponential(1);
    if (not std::isfinite(offset))
        return starting_density;

    ProbDensity candidate = starting_density;
    candidate *= exp_to_log_space(-offset);
    return candidate.log().isvalid() ? candidate : starting_density;
}
}

std::pair<double,double> 
find_slice_boundaries_stepping_out(double x0, slice_function& g, const ProbDensity& slice_level,
                                   double w, int m)
{
    assert(std::isfinite(x0));
    assert(std::isfinite(w) and w > 0);
    assert(std::isfinite(x0 - w) and x0 - w < x0);
    assert(std::isfinite(x0 + w) and x0 + w > x0);
    assert(g.in_range(x0));

    double u = uniform()*w;
    double L = x0 - u;
    double R = L + w;
    assert(std::isfinite(L) and std::isfinite(R));
    assert(L < R);
    assert(L <= x0 and x0 <= R);

    // Expand the interval until its ends are outside the slice, or until
    // the limit on steps is reached.

    //  std::cerr<<"!!    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";
    if (m>1) {
	int J = uniform_int(0,m);
	int K = m-J;

	while (J>0 and (not g.below_lower_bound(L)) and inside_slice(g(L), slice_level)) {
	    double L2 = L - w;
	    if (not std::isfinite(L2) or not (L2 < L)) break;
	    L = L2;
	    J--;
	    //      std::cerr<<" g("<<L<<") = "<<g()<<" >= "<<slice_level<<"\n";
	    //      std::cerr<<"<-    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";
	}

	while (K>0 and (not g.above_upper_bound(R)) and inside_slice(g(R), slice_level)) {
	    double R2 = R + w;
	    if (not std::isfinite(R2) or not (R < R2)) break;
	    R = R2;
	    K--;
	    //      std::cerr<<" g("<<R<<") = "<<g()<<" >= "<<slice_level<<"\n";
	    //      std::cerr<<"->    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";
	}
    }
    else {
	while ((not g.below_lower_bound(L)) and inside_slice(g(L), slice_level))
	{
	    double L2 = L - w;
	    if (not std::isfinite(L2) or not (L2 < L)) break;
	    L = L2;
	}

	while ((not g.above_upper_bound(R)) and inside_slice(g(R), slice_level))
	{
	    double R2 = R + w;
	    if (not std::isfinite(R2) or not (R < R2)) break;
	    R = R2;
	}
    }


    assert(L < R);

    //  std::cerr<<"[]    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";

    return {L,R};
}

std::ostream& operator<<(std::ostream& o, optional<ProbDensity> l)
{
    if (l)
        o<<l.value();
    else
        o<<"unavailable";
    return o;
}

std::tuple<double, double, optional<optional<ProbDensity>>, optional<optional<ProbDensity>>>
find_slice_boundaries_doubling(double x0, slice_function& g, const ProbDensity& slice_level,
                               double w, int K)
{
    assert(std::isfinite(x0));
    assert(std::isfinite(w) and w > 0);
    assert(std::isfinite(x0 - w) and x0 - w < x0);
    assert(std::isfinite(x0 + w) and x0 + w > x0);
    assert(g.in_range(x0));

    double u = uniform()*w;
    double L = x0 - u;
    double R = L + w;
    assert(std::isfinite(L) and std::isfinite(R));
    assert(L < R);
    assert(L <= x0 and x0 <= R);

    optional<optional<ProbDensity>> gL_cached;
    auto gL = [&]() {
        if (not gL_cached)
            gL_cached = g(L);
        return *gL_cached;
    };

    optional<optional<ProbDensity>> gR_cached;
    auto gR = [&]() {
        if (not gR_cached)
            gR_cached = g(R);
        return *gR_cached;
    };

    // Stop before doubling would make an endpoint or the interval width nonrepresentable.
    auto too_large = [](double L, double R, double w)
    {
        if (not std::isfinite(L) or not std::isfinite(R) or not (L < R))
            return true;

        double M = std::midpoint(L, R);
        double W = R-L;
        if (not std::isfinite(W))
            return true;

        bool ok = (L < M) and (M < R) and (W+w > W) and (L-w < L) and (R+w>R);
        return not ok;
    };

    while (K > 0 and (inside_slice(gL(), slice_level) or inside_slice(gR(), slice_level)))
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
    assert(L < std::midpoint(L, R) and std::midpoint(L, R) < R);

    //  std::cerr<<"[]    L0 = "<<L<<"   x0 = "<<x0<<"   R0 = "<<R<<"\n";

    return {L,R,gL_cached,gR_cached};
}

// Does this x0 really need to be the original point?
// I think it just serves to let you know which way the interval gets shrunk...

double search_interval(double x0, double L, double R, slice_function& g,
                       const ProbDensity& slice_level)
{
    // Shrink interval to lower and upper bounds.
    if (g.below_lower_bound(L)) L = *g.lower_bound;
    if (g.above_upper_bound(R)) R = *g.upper_bound;

    assert(std::isfinite(L) and std::isfinite(R));
    //  assert(g(x0) > g(L) and g(x0) > g(R));
    assert(inside_slice(g(x0), slice_level));
    assert(L < R);
    assert(L <= x0 and x0 <= R);

    double L0 = L, R0 = R;

    if (log_verbose >= 4)
	std::cerr<<"**    L0 = "<<L0<<"   x0 = "<<x0<<"   R0 = "<<R0<<std::endl;
    for(int i=0;i<100;i++)
    {
	double x1 = std::lerp(L, R, uniform());
	if (not std::isfinite(x1))
	{
	    if (log_verbose >= 2)
		std::cerr<<"slice sampling: nonfinite trial coordinate; retaining the current state\n";
	    g.reset();
	    return x0;
	}
	auto gx1 = g(x1);
	if (log_verbose >= 4)
	    std::cerr<<"    L  = "<<L <<"   x = "<<g.current_value()<<"   x = "<<x1<<"  R  = "<<R<<"     g(x) = "<<gx1<<std::endl;

	if (inside_slice(gx1, slice_level))
	    return x1;

	if (x1 > x0) 
	    R = x1;
	else
	    L = x1;
    }
    std::cerr.precision(17);
    std::cerr<<"Warning!  Is size of the interval really ZERO?"<<std::endl;
    auto density_x0 = g(x0);
    std::cerr<<"    L0 = "<<L0<<"   x0 = "<<x0<<"   R0 = "<<R0<<std::endl;
    std::cerr<<"    L  = "<<L <<"   x = "<<g.current_value()<<"   R  = "<<R<<std::endl;
    std::cerr<<"    log(f(x0)*U)  = "<<slice_level<<"  log(f(x0)) = "<<density_x0
             <<"  log(f(x_current)) = "<<g()<<std::endl;

    g.reset();

    return x0;
}

// Validate the recoverable state and numeric preconditions required by the
// interval algorithms, while allowing any internally valid exceptional rank.
bool pre_slice_sampling_check_OK(double x0, slice_function& g, double w)
{
    // Report one concise reason when a reachable state makes this kernel inapplicable.
    auto decline = [](const char* reason)
    {
        if (log_verbose >= 2)
            std::cerr<<"slice sampling: "<<reason<<"; retaining the current state\n";
        return false;
    };

    if (not g.can_slice_sample())
        return decline("slice function cannot be sampled from its current state");

    if (not std::isfinite(x0))
        return decline("starting coordinate is not finite");

    if (not std::isfinite(w) or not (w > 0))
        return decline("window is not finite and positive");

    if (not std::isfinite(x0 - w) or not (x0 - w < x0) or
        not std::isfinite(x0 + w) or not (x0 < x0 + w))
        return decline("window cannot form a finite interval around the starting coordinate");

    if ((g.lower_bound and std::isnan(*g.lower_bound)) or
        (g.upper_bound and std::isnan(*g.upper_bound)))
        return decline("range contains a NaN bound");

    if (g.lower_bound and g.upper_bound and not (*g.lower_bound < *g.upper_bound))
        return decline("range is empty or reversed");

    // If x is not in the range then this could be a range that is reduced to avoid loss of precision.
    if (not g.in_range(x0))
        return decline("starting coordinate is outside the range");

    assert(g.in_range(x0));

    auto gx0 = g();
    if (not gx0.log().isvalid())
        return decline("starting density has invalid exceptional-value bookkeeping");

    bool check_reevaluation = log_verbose >= 4;
#ifndef NDEBUG
    check_reevaluation = true;
#endif
    if (check_reevaluation)
    {
        auto gx0_v2 = g(x0);
        if (not gx0_v2)
            return decline("starting density cannot be reevaluated");

        auto difference = slice_log_ratio(gx0, *gx0_v2);
        if (not difference)
            throw myexception()<<"Error: slice_sampling: g() = "<<gx0<<" and g(x0) = "<<*gx0_v2
                               <<" have different exceptional-density ranks";
        if (std::abs(*difference) > 1.0e-9)
            throw myexception()<<"Error: slice_sampling: g() = "<<gx0<<"   g(x0) = "<<*gx0_v2
                               <<"   diff = "<<std::abs(*difference);
    }

    return true;
}

bool can_propose_same_interval_doubling(double x0, double x1, double w, double L, double R,
                                        optional<optional<ProbDensity>> gL_cached,
                                        optional<optional<ProbDensity>> gR_cached,
                                        slice_function& g, const ProbDensity& slice_level)
{
    bool D = false;

    auto gL = [&]() {
        if (not gL_cached)
            gL_cached = g.in_range(L) ? g(L) : optional<ProbDensity>{};
        return *gL_cached;
    };

    auto gR = [&]() {
        if (not gR_cached)
            gR_cached = g.in_range(R) ? g(R) : optional<ProbDensity>{};
        return *gR_cached;
    };

    bool ok = true;
    while (ok and (R-L)/w > 1.1)
    {
        double M = std::midpoint(L, R);
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

        // Unavailable and rank-changing boundaries are outside the scalar slice;
        // equality remains inside under the inclusive slice definition.
        if (D and not inside_slice(gL(), slice_level) and not inside_slice(gR(), slice_level))
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
    if (not pre_slice_sampling_check_OK(x0, g, w))
    {
        g.reset();
        return x0;
    }

    // 1. Determine the slice level.
    auto slice_level = draw_slice_level(g());

    // 2. Find the initial interval to sample from.
    auto [L,R] = find_slice_boundaries_stepping_out(x0, g, slice_level, w, m);

    // 3. Sample from the interval, shrinking it on each rejection
    return search_interval(x0, L, R, g, slice_level);
}

// We need to SET the value INSIDE this routine.
// Are we assuming that calling g sets the value?
double slice_sample_doubling_(double x0, slice_function& g, double w, int m)
{
    if (log_verbose >= 4)
        std::cerr<<"slice_sampling_doubling_: x0 = "<<x0<<" w = "<<w<<" Pr(x0) = "<<g()<<"\n";

    // 1. Determine the slice level
    auto slice_level = draw_slice_level(g());

    // 2. Find the initial interval to sample from.
    auto [L,R,gL_cached,gR_cached] =
        find_slice_boundaries_doubling(x0, g, slice_level, w, m);

    // 3. Sample from the interval, shrinking it on each rejection
    double x1 = search_interval(x0, L, R, g, slice_level);

    // 4. Check that we can propose the same interval from x2
    // We need to SET the value INSIDE this routine if we recompute g().
    if (can_propose_same_interval_doubling(x0, x1, w, L, R, gL_cached, gR_cached,
                                           g, slice_level))
        return x1;
    else
        return x0;
}

double slice_sample(double x0, slice_function& g,double w, int m)
{
    try
    {
        if (not pre_slice_sampling_check_OK(x0, g, w))
        {
            g.reset();
            return x0;
        }
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
