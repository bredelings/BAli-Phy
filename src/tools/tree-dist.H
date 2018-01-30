/*
  Copyright (C) 2004-2009 Benjamin Redelings

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

#ifndef TREE_DIST_H
#define TREE_DIST_H

#include <vector>
#include <valarray>
#include <boost/shared_ptr.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>

#include <string>
#include <iostream>

#include <map>

#include "partition.H"
#include "tree/tree.H"
#include "tree/sequencetree.H"
#include "util.H"
#include "io.H"

/// The information we store about each topology
struct tree_record 
{
    /// how many leaves does the tree have
    int n_leaves_;

    /// the internal branches for this topology
    std::vector<boost::dynamic_bitset<> > partitions;
  
    std::vector<double> branch_lengths;

    int n_leaves() const {return n_leaves_;}
    int n_leaf_branches() const {return n_leaves();}
    int n_internal_branches() const {return partitions.size();}
    int n_branches() const {return n_leaf_branches() + n_internal_branches();}

    tree_record(const Tree&);
};

int cmp(const tree_record&, const tree_record&);

bool operator<(const tree_record&, const tree_record&);

bool operator>(const tree_record&, const tree_record&);

/// A class for loading tree distributions - somewhat biased towards tree-dist-compare
class tree_sample 
{
    std::vector<std::string> leaf_names;

public:

    /// Add an tree with indices following leaf_names
    void add_tree(const tree_record& T);
    void add_tree(Tree& T);
    void add_tree(RootedTree& T);

    // the list of topologies, and associated info
    std::vector<tree_record> trees;

    std::vector<std::string> names() const {return leaf_names;}

    SequenceTree T(int i) const;

    const tree_record& operator[](int i) const {return trees[i];}
    tree_record& operator[](int i)       {return trees[i];}

    unsigned size() const {return trees.size();}

    std::valarray<bool> support(const partition& P) const;

    std::valarray<bool> support(const std::vector<partition>&) const;

    unsigned count(const partition& P) const;
    unsigned count(const std::vector<partition>&) const;

    double PP(const partition& P) const;
    double PP(const std::vector<partition>&) const;

    operator std::vector<tree_record>& () {return trees;}
    operator const std::vector<tree_record>& () const {return trees;}

    int load_file(std::istream&,      int skip=0, boost::optional<int> list=boost::none, int subsample = 1, boost::optional<int> max=boost::none, const std::vector<std::string>& prune=std::vector<std::string>());
    int load_file(const std::string&, int skip=0, boost::optional<int> last=boost::none, int subsample = 1, boost::optional<int> max=boost::none, const std::vector<std::string>& prune=std::vector<std::string>());
    int append_trees(const tree_sample&);

    tree_sample() {}
    tree_sample(std::istream&,int skip=0,boost::optional<int> last=boost::none,int subsample=1,boost::optional<int> max=boost::none,const std::vector<std::string>& prune=std::vector<std::string>());
    tree_sample(const std::string& filename,int skip=0,boost::optional<int> last=boost::none,int subsample=1,boost::optional<int> max=boost::none,const std::vector<std::string>& prune=std::vector<std::string>());
};

void scan_trees(std::istream&, int skip, boost::optional<int> last, int subsample, accumulator<SequenceTree>& op);
void scan_trees(std::istream&, int skip, boost::optional<int> last, int subsample, const std::vector<std::string>& prune, accumulator<SequenceTree>& op);
void scan_trees(std::istream&, int skip, boost::optional<int> last, int subsample, const std::vector<std::string>& prune, const std::vector<std::string>& leaf_order, accumulator<SequenceTree>& op);

tree_sample read_trees(boost::program_options::variables_map& args);
#endif
