#ifndef TREE_UTIL_H
#define TREE_UTIL_H
#include <boost/program_options.hpp>
#include <valarray>
#include "sequencetree.H"

/// Load a tree from command line args --tree filename
RootedSequenceTree load_T(const boost::program_options::variables_map& args);


std::vector<std::string> load_lines(std::istream& file,int skip=0,int subsample=1, int max=-1);
std::vector<SequenceTree> load_trees(const std::vector<std::string>& lines);
std::vector<SequenceTree> load_trees(std::istream& file,int skip=0,int subsample=1,int max=-1) ;

bool extends(const Tree& T,const Tree& Q);

std::vector<int> extends_map(const Tree& T,const Tree& Q);

/// A comparison operator for complete (and directed) partitions
struct compare_complete_partitions 
{
  bool operator()(const std::valarray<bool>& p1,
		  const std::valarray<bool>& p2) const;
};


#endif
