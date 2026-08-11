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
#include <optional>
#include <type_traits>
#include <utility>
#include "util/rng.H"
#include "util/myexception.H"
#include "util/range.H"
#include "util/math/log-double.H"
#include "probability/choice-weight.H"

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

/// Select from choices [0,P.size-1] in proportion to the probabilities given, using pre-allocated scratch space.
template <typename F>
int choose_scratch(const std::vector<F>& P, std::vector<F>& sum) 
{
    assert(P.size() == sum.size());

    sum[0] = P[0];
    for(int i=1;i<sum.size();i++)
        sum[i] = sum[i-1] + P[i];

    F r = F(uniform()) * sum[sum.size()-1];

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
// Compact present optional choices while retaining enough information to
// translate between the caller's indices and the ordinary choice interface.
template <typename F>
struct present_choices
{
    std::vector<int> original_indices;
    std::vector<F> weights;
    int original_size;

    // Build the compact weight vector and its mapping back to caller indices.
    explicit present_choices(const std::vector<std::optional<F>>& optional_weights)
        :original_size(optional_weights.size())
    {
        original_indices.reserve(optional_weights.size());
        weights.reserve(optional_weights.size());
        for(int i = 0; i < optional_weights.size(); i++)
            if (optional_weights[i])
            {
                original_indices.push_back(i);
                weights.push_back(*optional_weights[i]);
            }
    }

    // Return an original entry's compact index, or absence when that entry is unavailable.
    std::optional<int> compact_index(int original_index) const
    {
        assert(original_index >= 0 and original_index < original_size);
        for(int i = 0; i < original_indices.size(); i++)
            if (original_indices[i] == original_index)
                return i;
        return {};
    }

    // Translate a compact choice back to the corresponding caller index.
    int original_index(int compact_index) const
    {
        assert(compact_index >= 0 and compact_index < original_indices.size());
        return original_indices[compact_index];
    }
};
}

// Choose among present entries and return the corresponding original index.
// Absence is distinct from a present entry whose mathematical weight is zero.
template <typename F>
inline std::optional<int> choose(const std::vector<std::optional<F>>& optional_weights)
{
    choose_detail::present_choices choices(optional_weights);
    if (choices.weights.empty())
        return {};
    return choices.original_index(choose(choices.weights));
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

    // Preserve the ordinary-weight precondition.  A collection of symbolic
    // zero weights is meaningful for ChoiceWeight because their coefficients
    // can still be compared within the common zero stratum.
    if constexpr (not std::is_same_v<F, ChoiceWeight>)
        assert(sum > F(0.0));

    return P[s]/sum;
}

// Compute a categorical probability on the same filtered set used by the
// optional choose overload.  An absent destination has probability zero.
template <typename F>
std::optional<choice_probability_t<F>>
choose_P(int s, const std::vector<std::optional<F>>& optional_weights)
{
    choose_detail::present_choices choices(optional_weights);
    if (choices.weights.empty())
        return {};

    auto compact_s = choices.compact_index(s);
    if (not compact_s)
        return choice_probability_t<F>(0.0);
    return choose_P(*compact_s, choices.weights);
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
inline int choose_MH(int i, const std::vector<F>& P) 
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

    probability_type U = uniform();

    probability_type prod = 1;
    probability_type sum = 0;
    int I = -1;
    for(int k=0; k<N; k++)
    {
        if (o[k] == i) I=k;

        probability_type accept_k = 0;

        if (I == -1) { // k<I
            accept_k = P[o[k]] / total_greater_than[k];   // MH
        }
        else if (k == I)
        {
            accept_k = (I==N-1)?1:0;                      // MH
        }
        else if (k > I)
        {
            accept_k = P[o[k]] / total_greater_than[I];   // Gibbs
        }
        else
            std::abort();

        if constexpr (std::is_same_v<F, ChoiceWeight>)
        {
            assert(accept_k >= probability_type(0.0));
            assert(accept_k <= probability_type(1.0));
        }
        probability_type Pr_k = prod * accept_k;

        sum += Pr_k;
        if (U <= sum) return o[k];

        if (I == -1)
            prod *= (1.0 - accept_k);
    }

    choose_exception<F> c(i, P);
    c.prepend(":\n");
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
}

template <typename F>
inline choice_probability_t<F> choose_MH_P(int i, int j,const std::vector<F>& P)
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

    probability_type prod = 1;
    int I = -1;
    for(int k=0; k<N; k++)
    {
        if (o[k] == i) I=k;

        probability_type accept_k = 0;

        if (I == -1) { // k<I
            accept_k = P[o[k]] / total_greater_than[k];   // MH
        }
        else if (k == I)
        {
            accept_k = (I==N-1)?1:0;                      // MH
        }
        else if (k > I)
        {
            accept_k = P[o[k]] / total_greater_than[I];   // Gibbs
        }
        else
            std::abort();

        if constexpr (std::is_same_v<F, ChoiceWeight>)
        {
            assert(accept_k >= probability_type(0.0));
            assert(accept_k <= probability_type(1.0));
        }
        probability_type Pr_k = prod * accept_k;

        if (o[k] == j) return Pr_k;

        if (I == -1)
            prod *= (1.0 - accept_k);
    }

    choose_exception<F> c(i, P);
    c.prepend(": \n");
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
}

// Run choose_MH on present entries while retaining the caller's candidate
// indices.  An absent current entry makes the operation unavailable.
template <typename F>
inline std::optional<int> choose_MH(int i, const std::vector<std::optional<F>>& optional_weights)
{
    choose_detail::present_choices choices(optional_weights);
    auto compact_i = choices.compact_index(i);
    if (not compact_i)
        return {};
    return choices.original_index(choose_MH(*compact_i, choices.weights));
}

// Match optional choose_MH exactly: an absent current entry is unavailable,
// while an absent destination has transition probability zero.
template <typename F>
inline std::optional<choice_probability_t<F>>
choose_MH_P(int i, int j, const std::vector<std::optional<F>>& optional_weights)
{
    choose_detail::present_choices choices(optional_weights);
    auto compact_i = choices.compact_index(i);
    if (not compact_i)
        return {};

    auto compact_j = choices.compact_index(j);
    if (not compact_j)
        return choice_probability_t<F>(0.0);
    return choose_MH_P(*compact_i, *compact_j, choices.weights);
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
