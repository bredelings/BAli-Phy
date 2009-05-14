#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include "alignment.H"
#include "sequencetree.H"

ublas::matrix<int> load_alignment_constraint(const string& filename,SequenceTree& T);

std::vector< std::vector<int> > get_pins(const ublas::matrix<int>& constraint,const alignment& A,
					 const boost::dynamic_bitset<>& group1,const boost::dynamic_bitset<>& group2,
					 const std::vector<int>& seq1,const std::vector<int>& seq2);

boost::dynamic_bitset<> constraint_satisfied(const ublas::matrix<int>& constraint,const alignment& A);

void report_constraints(const boost::dynamic_bitset<>& s1, const boost::dynamic_bitset<>& s2);

bool any_branches_constrained(const std::vector<int>&,const SequenceTree& T,const SequenceTree& TC, const std::vector<int>&);
