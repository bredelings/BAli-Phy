#include <boost/program_options.hpp>
#include "alignment.H"
#include "sequencetree.H"

ublas::matrix<int> load_alignment_constraint(const boost::program_options::variables_map& args,
					     const alignment& A,SequenceTree& T);

std::vector< std::vector<int> > get_pins(const ublas::matrix<int>& constraint,const alignment& A,
					 const std::valarray<bool>& group1,const std::valarray<bool>& group2,
					 const std::vector<int>& seq1,const std::vector<int>& seq2);

std::valarray<bool> constraint_satisfied(const ublas::matrix<int>& constraint,const alignment& A);

void report_constraints(const std::valarray<bool>& s1, const std::valarray<bool>& s2);

