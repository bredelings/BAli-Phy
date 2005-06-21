#ifndef TREE_UTIL_H
#define TREE_UTIL_H
#include "sequencetree.H"
#include <boost/program_options.hpp>

/// Load a tree from command line args align=filename
RootedSequenceTree load_T(const boost::program_options::variables_map& args);


#endif
