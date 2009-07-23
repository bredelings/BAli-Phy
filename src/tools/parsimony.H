#include <vector>
#include "mytypes.H"
#include "alphabet.H"
#include "alignment.H"
#include "sequencetree.H"

ublas::matrix<int> unit_cost_matrix(unsigned size);

ublas::matrix<int> unit_cost_matrix(const alphabet&);

ublas::matrix<int> nucleotide_cost_matrix(const Triplets& T);

ublas::matrix<int> amino_acid_cost_matrix(const Codons& C);

template <class B>
B n_mutations(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const ublas::matrix<B>& cost);

template <typename B>
B n_mutations(const alignment& A, const SequenceTree& T,const ublas::matrix<B>& cost);

int n_mutations(const alignment& A, const SequenceTree& T);

std::vector<int> get_parsimony_letters(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const ublas::matrix<int>& cost);

std::vector<std::vector<int> > get_all_parsimony_letters(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const ublas::matrix<int>& cost);
