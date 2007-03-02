#include <vector>
#include "mytypes.H"
#include "alphabet.H"
#include "alignment.H"
#include "sequencetree.H"

Matrix unit_cost_matrix(unsigned size);

Matrix unit_cost_matrix(const alphabet&);

Matrix nucleotide_cost_matrix(const Triplets& T);

Matrix amino_acid_cost_matrix(const Codons& C);


unsigned n_mutations(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const Matrix& cost);
unsigned n_mutations(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T);

unsigned n_mutations(const alignment& A, const SequenceTree& T,const Matrix& cost);
unsigned n_mutations(const alignment& A, const SequenceTree& T);
