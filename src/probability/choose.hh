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

#include <algorithm>
#include <vector>
#include <unordered_map>
#include <type_traits>
#include <utility>
#include "util/rng.hh"
#include "util/myexception.hh"
#include "util/range.hh"
#include "util/math/log-double.hh"
#include "probability/availability.hh"
#include "probability/choice-weight.hh"

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
    ~choose_exception() {}
};

template <> choose_exception<log_double_t>::choose_exception(int i, const std::vector<log_double_t>& V);

template <typename I, typename F>
class uchoose_exception: public myexception {
public:
    /// What is the current choice?
    I i;

    /// What are the probabilities of each of the options?
    std::unordered_map<I,F> Pr;

    uchoose_exception(const I& i, const std::unordered_map<I,F>& V)
        :Pr(V)
        {
            (*this)<<"No option chosen! (current = "<<i<<")\n";
            for(auto& [j,f]: Pr)
	    {
                if (i == j) (*this)<<"*";
                (*this)<<"Pr["<<j<<"] = "<<Pr[j]<<"\n";
            }
            (*this)<<show_stack_trace();
            //    std::abort();
        }
    ~uchoose_exception() {}
};


/// Select from choice 0 or 1 in proportion to the probabilities given.
int choose2(log_double_t, log_double_t);

log_double_t choose2_P(int, log_double_t, log_double_t);

namespace choose_detail
{
template <typename T>
struct is_choice_weight: std::is_same<T, ChoiceWeight> {};

template <typename T>
struct is_choice_weight<Availability<T>>: std::is_same<T, ChoiceWeight> {};

template <typename T>
inline constexpr bool is_choice_weight_v = is_choice_weight<T>::value;
}

/// Select from choices [0,P.size-1] in proportion to the probabilities given, using pre-allocated scratch space.
template <typename F>
int choose_scratch(const std::vector<F>& P, std::vector<F>& sum) 
{
    assert(P.size() == sum.size());

    sum[0] = P[0];
    for(int i=1;i<sum.size();i++)
        sum[i] = sum[i-1] + P[i];

    double u = uniform();
    if constexpr (choose_detail::is_choice_weight_v<F>)
        // A positive scalar has neutral defect rank, whereas ChoiceWeight(0)
        // represents an actual zero-density factor and is not a valid threshold.
        while (u == 0)
            u = uniform();

    auto r = [&]()
    {
        if constexpr (is_availability_v<F>)
            return u * sum.back();
        else
            return F(u) * sum.back();
    }();

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
    // Copying avoids requiring a default value that acts as an additive
    // identity.  ChoiceWeight deliberately has no such universal value.
    std::vector<F> sum = P;

    return choose_scratch(P,sum);
}

namespace choose_detail
{
// Compute 1-p without defining subtraction on Availability, where absence
// propagates rather than behaving as either numeric zero or one.
template <typename Probability>
Probability probability_complement(const Probability& probability)
{
    return Probability(1.0) - probability;
}

// Apply the Availability-specific complement without adding subtraction to its algebra.
template <typename T>
Availability<T> probability_complement(const Availability<T>& probability)
{
    return complement(probability);
}
}

// Choose directly on the original candidate vector.  Unavailable is the
// additive identity, so no index compaction or translation is needed.
template <typename F>
inline Availability<int> choose(const std::vector<Availability<F>>& weights)
{
    if (std::none_of(weights.begin(), weights.end(), [](const auto& weight) {return bool(weight);}))
        return unavailable;

    std::vector<Availability<F>> sum = weights;
    return available(choose_scratch(weights, sum));
}

template <typename Weight>
using choice_probability_t = decltype(std::declval<const Weight&>() /
                                      std::declval<const Weight&>());

template <typename F>
choice_probability_t<F> choose_P(int s,const std::vector<F>& P) {
    assert(s >= 0 and s < P.size());

    F sum = P[0];
    for(int i=1;i<P.size();i++)
        sum = sum + P[i];

    if constexpr (is_availability_v<F>)
        if (not sum)
            return unavailable;

    // Preserve the ordinary-weight precondition.  A collection of symbolic
    // zero weights is meaningful for ChoiceWeight because their coefficients
    // can still be compared within the common zero stratum.
    if constexpr (not choose_detail::is_choice_weight_v<F>)
        assert(sum > F(0.0));

    return P[s]/sum;
}

namespace choose_detail
{
// Return S[k] = weights[order[k+1]] + ... + weights[order.back()].
// Construct every total from real candidates because ChoiceWeight has no
// universal additive identity below every possible defect rank.
template <typename Weight>
std::vector<Weight> suffix_totals_after(const std::vector<Weight>& weights,
                                        const std::vector<int>& order)
{
    assert(not weights.empty());
    assert(weights.size() == order.size());

    std::vector<Weight> reversed_totals;
    reversed_totals.reserve(weights.size() - 1);
    if (weights.size() == 1)
        return reversed_totals;

    Weight total = weights[order.back()];
    reversed_totals.push_back(total);
    for(int k = order.size() - 2; k > 0; k--)
    {
        total = total + weights[order[k]];
        reversed_totals.push_back(total);
    }
    std::reverse(reversed_totals.begin(), reversed_totals.end());
    return reversed_totals;
}
}

template <typename F>
inline int choose_MH_core(int i, const std::vector<F>& P)
{
    const int N = P.size();
    assert(N > 0);
    assert(i >= 0 and i < N);
    using probability_type = choice_probability_t<F>;

    // Get the order of the probabilities
    std::vector<int> o = iota<int>(N);
    sequence_order<F> A(P);
    std::sort(o.begin(), o.end(), A);

    // Compute sums of elements larger than the k-th smallest as weights.
    auto total_greater_than = choose_detail::suffix_totals_after(P, o);

    probability_type U{uniform()};

    probability_type prod{1.0};
    probability_type sum{0.0};
    int I = -1;
    for(int k=0; k<N; k++)
    {
        if (o[k] == i) I=k;

        probability_type accept_k{0.0};

        if (I == -1) { // k<I
            accept_k = P[o[k]] / total_greater_than[k];   // MH
        }
        else if (k == I)
        {
            accept_k = probability_type{double(I == N-1)}; // MH
        }
        else if (k > I)
        {
            accept_k = P[o[k]] / total_greater_than[I];   // Gibbs
        }
        else
            std::abort();

        if constexpr (choose_detail::is_choice_weight_v<F>)
        {
            assert(accept_k >= probability_type(0.0));
            assert(accept_k <= probability_type(1.0));
        }
        probability_type Pr_k = prod * accept_k;

        sum += Pr_k;
        // uniform() is in [0,1), so half-open cumulative intervals exclude
        // both unavailable and available-zero transitions at the lower endpoint.
        if (U < sum)
            return o[k];

        if (I == -1)
            prod *= choose_detail::probability_complement(accept_k);
    }

    choose_exception<F> c(i, P);
    c.prepend(":\n");
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
}

template <typename F>
inline int choose_MH(int i, const std::vector<F>& P) {return choose_MH_core(i, P);}

// An unavailable current state has no transition kernel.  When the current
// state is available, unavailable entries form a zero-probability prefix and
// the remaining MH calculation is identical to the compacted available list.
template <typename F>
inline Availability<int> choose_MH(int i, const std::vector<Availability<F>>& P)
{
    assert(i >= 0 and i < P.size());
    if (not P[i])
        return unavailable;
    return available(choose_MH_core(i, P));
}

template <typename F>
inline choice_probability_t<F> choose_MH_P_core(int i, int j,const std::vector<F>& P)
{
    const int N = P.size();
    assert(N > 0);
    assert(i >= 0 and i < N);
    assert(j >= 0 and j < N);
    using probability_type = choice_probability_t<F>;

    // Get the order of the probabilities
    std::vector<int> o = iota<int>(N);
    sequence_order<F> A(P);
    std::sort(o.begin(), o.end(), A);

    //  if (i==j and o[N-1] != i) return 0;

    // Compute sums of elements larger than the k-th smallest as weights.
    auto total_greater_than = choose_detail::suffix_totals_after(P, o);

    probability_type prod{1.0};
    int I = -1;
    for(int k=0; k<N; k++)
    {
        if (o[k] == i) I=k;

        probability_type accept_k{0.0};

        if (I == -1) { // k<I
            accept_k = P[o[k]] / total_greater_than[k];   // MH
        }
        else if (k == I)
        {
            accept_k = probability_type{double(I == N-1)}; // MH
        }
        else if (k > I)
        {
            accept_k = P[o[k]] / total_greater_than[I];   // Gibbs
        }
        else
            std::abort();

        if constexpr (choose_detail::is_choice_weight_v<F>)
        {
            assert(accept_k >= probability_type(0.0));
            assert(accept_k <= probability_type(1.0));
        }
        probability_type Pr_k = prod * accept_k;

        if (o[k] == j) return Pr_k;

        if (I == -1)
            prod *= choose_detail::probability_complement(accept_k);
    }

    choose_exception<F> c(i, P);
    c.prepend(": \n");
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
}

template <typename F>
inline choice_probability_t<F> choose_MH_P(int i, int j, const std::vector<F>& P) {return choose_MH_P_core(i, j, P);}

template <typename F>
inline choice_probability_t<Availability<F>>
choose_MH_P(int i, int j, const std::vector<Availability<F>>& weights)
{
    assert(i >= 0 and i < weights.size());
    assert(j >= 0 and j < weights.size());
    if (not weights[i])
        return unavailable;
    return choose_MH_P_core(i, j, weights);
}

/// Select from choices [0,P.size-1] in proportion to the probabilities given, using pre-allocated scratch space.
template <typename I, typename F>
I choose(const std::unordered_map<I,F>& P)
{
    F total = 0;
    for(auto& [i,f]: P)
	total += f;

    F r = F(uniform() * total);

    F sum = 0;
    for(auto& [i,f]: P)
    {
	sum += f;
	if (r <= sum) return i;
    }

    uchoose_exception<I,F> c(P.begin()->first, P);
    c.prepend(":\n");
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
}

#endif
