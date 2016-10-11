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

#include <string>
#include <iostream>

#include <map>

#include "partition.H"
#include "tree/tree.H"
#include "tree/sequencetree.H"
#include "util.H"
#include "io.H"

namespace trees_format {

    /// A structure to read and parse a collection of trees with the same leaf set
    struct reader_t
    {
	std::vector<std::string> leaf_names;
	virtual bool next_tree_(Tree&,int&)=0;
	int lines_;

    public:
	virtual reader_t* clone() const=0;
	virtual const std::vector<std::string>& names() const;
	virtual bool next_tree(Tree&);
	virtual bool next_tree(RootedTree&);
	virtual bool next_tree(SequenceTree&);
	virtual bool next_tree(RootedSequenceTree&);
	int lines() const;
	virtual bool skip(int) = 0;
	virtual bool done() const = 0;
	reader_t():lines_(0) {}
	virtual ~reader_t() {}
    };

    class Newick: public reader_t
    {
	std::string line;
	std::istream* file;

	void initialize();

	bool next_tree_(Tree&,int&);
    public:
	Newick* clone() const {return new Newick(*this);}

	bool skip(int);
	bool done() const;

	Newick(const std::string& filename);
	Newick(std::istream&);
	~Newick();
    };

    class NEXUS: public reader_t
    {
	std::string line;
	std::istream* file;
	bool translate;

	void parse_translate_command(const std::string&);
	void initialize();

	bool next_tree_(Tree&,int&);
    public:
	NEXUS* clone() const {return new NEXUS(*this);}

	bool skip(int);
	bool done() const;

	NEXUS(const std::string& filename);
	NEXUS(std::istream&);
	~NEXUS();
    };

    class wrapped_reader_t: public reader_t
    {
    protected:
	boost::shared_ptr<reader_t> tfr;

	bool next_tree_(Tree&,int&);
    public:
	virtual wrapped_reader_t* clone() const=0;
	virtual const std::vector<std::string>& names() const;

	bool skip(int);
	bool done() const;

	wrapped_reader_t();
	wrapped_reader_t(const reader_t&);
    };

    class Newick_or_NEXUS: public wrapped_reader_t
    {
    public:
	Newick_or_NEXUS* clone() const {return new Newick_or_NEXUS(*this);}

	Newick_or_NEXUS(const std::string&);
	Newick_or_NEXUS(std::istream&);
    };

    class Prune: public wrapped_reader_t
    {
	std::vector<std::string> prune;
	std::vector<int> prune_index;

	bool next_tree_(Tree&,int&);

    public:
	Prune* clone() const {return new Prune(*this);}
	const std::vector<std::string>& names() const;

	Prune(const std::vector<std::string>&, const reader_t&);
    };

    class Skip: public wrapped_reader_t
    {
    public:
	Skip* clone() const {return new Skip(*this);}

	Skip(int, const reader_t&);
    };

    class Subsample: public wrapped_reader_t
    {
	int subsample;

	bool next_tree_(Tree&,int& r);
    public:
	Subsample* clone() const {return new Subsample(*this);}
    
	Subsample(int, const reader_t&);
    };

    class Max: public wrapped_reader_t
    {
	int m;

	bool next_tree_(Tree&,int& r);
    public:
	Max* clone() const {return new Max(*this);}
    
	Max(int, const reader_t&);
    };


    class Fixroot: public wrapped_reader_t
    {
	bool next_tree_(Tree&,int& r);
    public:
	Fixroot* clone() const {return new Fixroot(*this);}
    
	Fixroot(const reader_t&);
    };

    class ReorderLeaves: public wrapped_reader_t
    {
	std::vector<int> mapping;
	bool next_tree_(Tree&,int& r);
    public:
	ReorderLeaves* clone() const {return new ReorderLeaves(*this);}
	const std::vector<std::string>& names() const;

	ReorderLeaves(const std::vector<std::string>& leaf_order, const reader_t&);
    };

}

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

    int load_file(std::istream&,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());
    int load_file(const std::string&,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());
    int append_trees(const tree_sample&);

    tree_sample() {}
    tree_sample(std::istream&,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());
    tree_sample(const std::string& filename,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());
};

void scan_trees(std::istream&,int skip,int subsample,int max,accumulator<SequenceTree>& op);
void scan_trees(std::istream&,int skip,int subsample,int max,const std::vector<std::string>& prune,accumulator<SequenceTree>& op);
void scan_trees(std::istream&,int skip,int subsample,int max,const std::vector<std::string>& prune,const std::vector<std::string>& leaf_order, accumulator<SequenceTree>& op);

#endif
