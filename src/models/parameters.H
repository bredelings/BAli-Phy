/*
  Copyright (C) 2004-2012 Benjamin Redelings

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
/// \file parameters.H
///
/// \brief This file defines the Parameters class which holds the model and state
///        for the MCMC.
///

#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <algorithm>                                // for copy
#include <boost/smart_ptr/intrusive_ptr.hpp>        // for intrusive_ptr
#include <filesystem>                               // for std::filesystem::path
#include <memory>                                   // for shared_ptr, alloc...
#include <optional>                                 // for optional
#include <string>                                   // for string
#include <tuple>                                    // for tuple
#include <vector>                                   // for vector
#include "TreeInterface.H"                          // for TreeInterface
#include "alignment/alignment.H"                    // for alignment
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "computation/object.H"                     // for Object, intrusive...
#include "computation/param.H"                      // for param
#include "sequence/sequence.H"                      // for sequence
#include "tools/partition.H"                        // for Partition
#include "tree/sequencetree.H"                      // for SequenceTree
#include "tree/tree.H"                              // for tree_edge
#include "util/assert.hh"                           // for assert
#include "util/math/log-double.H"                   // for log_double_t
#include "util/matrix.H"                            // for Matrix, matrix
#include "imodel/imodel.H"
class Likelihood_Cache_Branch;
class Parameters;
class alphabet;
class context;
class model_t;
class module_loader;
class pairwise_alignment_t;

//------------------------------- parameter-containing class --------------------------//

struct data_partition_constants
{
    int properties_reg;

    std::optional<int> alignment_properties_reg;

    int likelihood_calculator;

    data_partition_constants(context_ref& C, int r_data);
};

/// Each data_partition is a model with one parameter: mu (the branch mean)
class data_partition
{
    friend class Parameters;

protected:
    const Parameters* P;

    int partition_index;

    const data_partition_constants& DPC() const;

    int likelihood_calculator() const;

    context_ptr properties() const;

    context_ptr property(int i) const;

    context_ptr alignment_properties() const;

    context_ptr alignment_property(int i) const;

public:

    bool has_pairwise_alignments() const;

    /// Is the alignment allowed to vary?
    bool variable_alignment() const;

    ParametersTreeInterface t() const;

    /// Cached Transition Probabilities
    object_ptr<const EVector> transition_P(int b) const;   // used only to force transition_Ps in sample-topology-SPR.cc [ SPR_search_attachment_points ]
  
    expression_ref get_node_CLV(int i) const;   // used in substitution.cc [sample_subst_history], parsimony.cc [peel_muts_leaf_branch, accumulate_root_leaf]

    int n_base_models() const;                  // used in substitution.cc for get_column_likelihoods

    int n_states() const;                       // used in substitution.cc for get_column_likelihoods

    object_ptr<const Box<Matrix>> WeightedFrequencyMatrix(int n) const;     // used in sample-alignment.cc, sample-cube.cc, sample-tri.cc

    object_ptr<const Likelihood_Cache_Branch> cache(int b) const;  // used in get_column_likelihoods( ), other_subst( ), sample_subst_history( )

    int subst_root() const;

    /// cached branch HMMs
    const indel::PairHMM& get_branch_HMM(int b) const;
    std::vector<indel::PairHMM> get_branch_HMMs(const std::vector<int>&) const;

    log_double_t sequence_length_pr(int i) const;     // used in dp/3way.cc, dp/5way.cc, dp/alignment-sums.cc

    double get_beta() const;

    // get the alphabet for partition i
    std::shared_ptr<const alphabet> get_alphabet() const;

    bool alignment_is_random() const;

    int seqlength(int n) const;

    expression_ref get_pairwise_alignment_(int b) const;

    const pairwise_alignment_t& get_pairwise_alignment(int b) const;

    bool pairwise_alignment_is_unset(int b) const;

    // The prior, likelihood, and probability
    log_double_t prior_alignment() const;
    log_double_t likelihood() const;
    log_double_t heated_likelihood() const;

    data_partition(const Parameters* P, int i);
};

struct mutable_data_partition: public data_partition
{
    void set_pairwise_alignment(int b, const pairwise_alignment_t&);

    void unset_pairwise_alignment(int b);

    mutable_data_partition(const Parameters* P, int i);
};

struct parameters_constants: public Object
{
    std::vector<data_partition_constants> DPC;

    parameters_constants* clone() const {return new parameters_constants(*this);}
};

/// A class to contain all the MCMC state except the alignment
class Parameters: public context
{
    friend class data_partition;
    friend class ParametersTreeInterface;
public:
    object_ptr<parameters_constants> PC;
    object_ptr<tree_constants> TC;

    bool variable_alignment_ = false;

            data_partition get_data_partition(int i) const;

    mutable_data_partition get_data_partition(int i);

    int n_data_partitions() const {return PC->DPC.size();}

            data_partition operator[](int i) const {return get_data_partition(i);}

    mutable_data_partition operator[](int i)       {return get_data_partition(i);}

    ParametersTreeInterface t() const;

    void reconnect_branch(int s1, int t1, int t2);
    void prune_subtree(const tree_edge& b_subtree);
    void regraft_subtree(const tree_edge& b_subtree, const tree_edge& b_target);

private:
    void exchange_subtrees(int b1, int b2);
public:
    void NNI(const tree_edge& b1, const tree_edge& b2, bool disconnect_subtree=false);
    void NNI(int b1, int b2, bool disconnect_subtree=false);
    void NNI_discard_alignment(int b1, int b2);
    void show_h_tree() const;

    /// Did we most recently hit beta==1 (1) or beta=0 (0)
    int updown;
  
    // The prior, likelihood, and probability
    log_double_t prior_alignment() const;

    void cache_likelihood_branches() const;

    /// Set branch 'b' to have length 'l', and compute the transition matrices
    void setlength_unsafe(int b, double l);
    void setlength(int b,double l); 
  
    // bool
    bool variable_alignment() const;

    // int
    void select_root(int b) const;
    void set_root_(int n) const;
    void set_root(int n) const;
    int subst_root(int i) const;
    int subst_root() const;

    double branch_mean() const;

    std::string name() const {return "Full Model";}

    Parameters* clone() const override {return new Parameters(*this);}

    Parameters& operator=(const Parameters&) = default;
    Parameters& operator=(Parameters&&) = default;

    Parameters(const Parameters&) = default;
    Parameters(Parameters&&) = default;

    // Can we create a Parameters from a context and a tree_reg?
    Parameters(const context_ref& C, int tree_reg, const std::vector<int>& alignments_regs = {});
};

std::string write_newick(const Parameters&, bool=true);

int alignment_length(const data_partition& P);

int alignment_length(const Parameters& P);

#endif
