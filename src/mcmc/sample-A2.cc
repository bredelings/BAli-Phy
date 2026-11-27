/*
   Copyright (C) 2004-2007,2009-2010 Benjamin Redelings

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

#include "sample.hh"
#include "dp/1way.hh"
#include "dp/2way.hh"
#include "dp/alignment-sums.hh"
#include "dp/dp-matrix.hh"
#include "alignment/alignment-constraint.hh"
#include "substitution/likelihood.hh"
#include "util/log-level.hh"
#include "util/myexception.hh"
#include <cmath>
#include <iostream>
#include <new>

namespace {

// Compose left->middle and middle->right, summing over the unobserved middle sequence.
// Glue orders unrelated events canonically, so each pair of branch alignments is counted once.
// Middle-only emissions are silent in the two boundary coordinates; their loops are summed by
// dp_HMM and expanded after sampling. The tree-prior length factor is corrected by the shared driver.
std::pair<std::shared_ptr<DPengine>, Availability<log_double_t>>
sample_A2_base(mutable_data_partition P, const std::vector<int>& nodes)
{
    assert(P.variable_alignment());
    auto t = P.t();
    assert(nodes.size() == 3 and t.degree(nodes[0]) == 2);
    assert(not P.get_node_CLV(nodes[0]));
    int b1 = t.find_branch(nodes[1], nodes[0]);
    int b2 = t.find_branch(nodes[2], nodes[0]);

    HMM m1 = P.get_branch_HMM(b1);
    m1.remap_bits({0, 2});
    HMM m2 = P.get_branch_HMM(t.reverse(b2));
    m2.remap_bits({2, 1});
    HMM m12 = Glue(m1, m2);
    m12.hidden_bits.set(2);
    m12.B = P.get_beta();

    auto F = P.WeightedFrequencyMatrix(nodes[0]);
    auto dists1 = substitution::shift(*P.cache(b1), 2);
    auto dists2 = substitution::shift(*P.cache(b2), 2);
    std::shared_ptr<DPmatrixConstrained> matrices;
    try
    {
        MatrixSize size{dists1.n_columns(), dists2.n_columns()};
        auto boundaries = yboundaries_everything(size.size1-2, size.size2-2);
        matrices = std::make_shared<DPmatrixConstrained>(
            MatrixShape(size, std::move(boundaries)), m12, std::move(dists1), std::move(dists2), *F);
        matrices->emit1 = 1;
        matrices->emit2 = 2;
        // Even the padding column needs the state order when used as a predecessor of a boundary cell.
        for(int j=0;j<size.size2;j++)
            matrices->states(j) = matrices->dp_order();
    }
    catch (const dp_table_allocation_failure&)
    {
        throw;
    }
    catch (const std::bad_alloc&)
    {
        throw dp_table_allocation_failure{};
    }
    matrices->forward_band();

    auto generalized_path = matrices->sample_path();
    if (not generalized_path)
        return {matrices, unavailable};
    auto path = matrices->ungeneralize(*generalized_path);
    // Include the probability of expanding silent runs, not just choosing the generalized path.
    auto sampling_pr = matrices->path_P(*generalized_path) * matrices->generalize_P(path);
    if (not std::isfinite(sampling_pr.log()))
        return {matrices, unavailable};

    // Both alignments must be installed before evaluating a candidate with a new middle length.
    for(int i=0;i<2;i++)
        P.set_pairwise_alignment(t.find_branch(nodes[0], nodes[i+1]),
                                 get_pairwise_alignment_from_path(path, *matrices, 2, i));
    return {matrices, available(sampling_pr)};
}

// Only the per-partition DP differs from the existing tri/cube sampling driver.
struct sample_A2_multi_calculation: sample_star_multi_calculation
{
    using sample_star_multi_calculation::sample_star_multi_calculation;

    std::pair<std::shared_ptr<DPengine>, Availability<log_double_t>> compute_matrix(int i, int j) override
    {
        return sample_A2_base(p[i][j], nodes[i]);
    }
};

}

// A2 changes the middle length and requires both boundary likelihoods at that node.
// Reject unsupported observations or fixed roots before modifying any alignment or root.
bool can_sample_A2(const Parameters& P, int middle)
{
    if (P.t().degree(middle) != 2)
        return false;
    for(int j=0;j<P.n_data_partitions();j++)
    {
        auto partition = P[j];
        if (partition.variable_alignment() and partition.get_node_CLV(middle))
            return false;
    }
    return P.can_set_root(middle);
}

// Propose both incident alignments in a separate context, then apply the middle-length correction.
void sample_A2(Parameters& P, int middle, int left)
{
    if (not can_sample_A2(P, middle))
        throw myexception()<<"A2 sampling requires an unobserved degree-two node compatible with the substitution roots.";
    P.set_root_(middle);
    auto nodes = A2::get_nodes_branch(P.t(), middle, left);
    try
    {
        std::vector<Parameters> p(1, P);
        sample_A2_multi_calculation calculation(p, {nodes}, {});
        calculation.run_dp();
        if (calculation.Pr[0])
        {
            calculation.set_proposal_probabilities({1});
            if (auto choice = calculation.choose())
                P = p[*choice];
        }
    }
    catch (const dp_table_allocation_failure& e)
    {
        if (log_verbose >= 1)
            std::cerr<<"Allocation failed in sample_A2: "<<e.what()<<"!  Proceeding."<<std::endl;
    }
    P.evaluate_program();
}
