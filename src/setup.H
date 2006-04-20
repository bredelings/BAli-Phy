#ifndef SETUP_H
#define SETUP_H

#include <valarray>
#include "smodel.H"
#include "imodel.H"
#include "alignment.H"
#include "sequencetree.H"
#include "clone.H"
#include <boost/program_options.hpp>

/// Reorder internal sequences of A to correspond to standardized node names for T
alignment standardize(const alignment& A, const SequenceTree& T);

/// Estimate the empirical frequencies of different letters from the alignment, with pseudocounts
std::valarray<double> empirical_frequencies(const boost::program_options::variables_map& args,
					    const alignment& A);

//----------------- Load trees and alignments ---------------------//


SequenceTree get_random_T(const alignment& A);

/// Map leaf nodes of T to the leaf sequences of A
void link(alignment& A,SequenceTree& T,bool internal_sequences=true);

/// Map leaf nodes of T to the leaf sequences of A
void link(alignment& A,RootedSequenceTree& T,bool internal_sequences=true);

/// Load and link() an alignment A and SequenceTree T
void load_A_and_T(const boost::program_options::variables_map& args,
		  alignment& A,SequenceTree& T,bool internal_sequences=true);

/// Load and link() an alignment A and SequenceTree T
void load_A_and_T(const boost::program_options::variables_map& args,
		  alignment& A,RootedSequenceTree& T,bool internal_sequences=true);

/// Load an alignment A, generate a random SequenceTree T, and then link them.
void load_A_and_random_T(const boost::program_options::variables_map& args,
			 alignment& A,SequenceTree& T,bool internal_sequences=true);

/// Count how many times each letter occurs in A
std::valarray<double> letter_counts(const alignment& A);

/// Count how many times letter 'l' occurss in A (including *,-,?)
int letter_count(const alignment& A,int l);

//------------------------ Substitution stuff ----------------------//

/// Return a substitution::Model specified by args
OwnedPointer<substitution::MultiModel> get_smodel(const boost::program_options::variables_map& args,
						  const alphabet& a,const std::valarray<double>& default_frequencies);
/// Return a substitution::Model specified by args
OwnedPointer<substitution::MultiModel> get_smodel(const boost::program_options::variables_map& args,
						  const alignment& A);

//--------------------------- Indel stuff --------------------------//
OwnedPointer<IndelModel> get_imodel(const boost::program_options::variables_map& args);

#endif 
