#ifndef SETUP_H
#define SETUP_H

#include <valarray>
#include "smodel.H"
#include "arguments.H"
#include "alignment.H"
#include "sequencetree.H"

/// Reorder internal sequences of A to correspond to standardized node names for T
alignment standardize(const alignment& A, const SequenceTree& T);

/// Estimate the empirical frequencies of different letters from the alignment, with pseudocounts
std::valarray<double> empirical_frequencies(Arguments& args,const alignment& A);

//----------------- Load trees and alignments ---------------------//

/// Load an alignment from command line args align=filename
void load_A(Arguments& args, alignment& A);

/// Load a tree from command line args align=filename
void load_T(Arguments& args,const alignment& A,SequenceTree& T,bool random_tree_ok);

/// Check that internal nodes don't have letters 
void check_internal_sequences_composition(const alignment& A,int n_leaves);

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const tree& T,const vector<int>& ignore=vector<int>());

/// Hook up the leaf nodes of T to the leaf of A
/// Also, make sure that A has the right number of internal sequences.
void link(alignment& A,SequenceTree& T,bool internal_sequences=true);

void load_A_and_T(Arguments& args,alignment& A,SequenceTree& T,bool internal_sequences=true);

//------------------------ Substitution stuff ----------------------//

/// Return a substitution::Model specified by args
substitution::MultiRateModel* get_smodel(Arguments& args,const alphabet& a,const std::valarray<double>& default_frequencies);
/// Return a substitution::Model specified by args
substitution::MultiRateModel* get_smodel(Arguments& args,const alignment& A);

#endif 
