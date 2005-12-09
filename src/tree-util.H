#ifndef TREE_UTIL_H
#define TREE_UTIL_H
#include "sequencetree.H"
#include <boost/program_options.hpp>

/// Load a tree from command line args --tree filename
RootedSequenceTree load_T(const boost::program_options::variables_map& args);


std::vector<std::string> load_lines(std::istream& file,int skip=0,int subsample=1, int max=-1);
std::vector<SequenceTree> load_trees(const std::vector<std::string>& lines);
std::vector<SequenceTree> load_trees(std::istream& file,int skip=0,int subsample=1,int max=-1) ;

#endif
