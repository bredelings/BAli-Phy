#include <vector>
#include "alphabet.H"
#include "alignment.H"
#include "sequencetree.H"

unsigned n_mutations(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T);
unsigned n_mutations(const alignment& A, const SequenceTree& T);
