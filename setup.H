#ifndef SETUP_H
#define SETUP_H

#include <valarray>
#include "smodel.H"
#include "arguments.H"
#include "alignment.H"
#include "sequencetree.H"

/// Return a substitution::Model specified by args
substitution::MultiRateModel* get_smodel(Arguments& args,const alphabet& a,const std::valarray<double>& default_frequencies);
/// Return a substitution::Model specified by args
substitution::MultiRateModel* get_smodel(Arguments& args,const alignment& A);

/// Hook up the leaf nodes of T to the leaf of A

/// Hook up the leaf nodes of T to the leaf of A
/// Also, make sure that A has the right number of internal sequences.
void link(alignment& A,SequenceTree& T);

void load_A_and_T(Arguments& args,alignment& A,SequenceTree& T,bool random_tree_ok=false);

#endif 
