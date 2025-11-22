/*
   Copyright (C) 2005-2007,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef TREE_UTIL_H
#define TREE_UTIL_H
#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include <filesystem>

#include "tree/sequencetree.H"

/// Load a tree from command line args --tree filename
RootedSequenceTree load_T(const boost::program_options::variables_map& args);
RootedSequenceTree load_tree_from_file(const std::filesystem::path& filename);

std::optional<SequenceTree> parse_sequence_tree(const std::string& text);

/// Validate that all branch lengths in a tree are positive
void validate_initial_tree_branch_lengths(const SequenceTree& tree);

std::vector<SequenceTree> load_trees(const std::vector<std::string>& lines);
std::vector<SequenceTree> load_trees(std::istream& file,int skip=0,int subsample=1,int max=-1) ;

bool extends(const Tree& T,const Tree& Q);

std::vector<int> extends_map(const Tree& T,const Tree& Q);

/// A comparison operator for complete (and directed) partitions
struct compare_complete_partitions 
{
  bool operator()(const boost::dynamic_bitset<>& p1,
		  const boost::dynamic_bitset<>& p2) const;
};


#endif
