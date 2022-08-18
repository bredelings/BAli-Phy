/*
  Copyright (C) 2004-2009,2017 Benjamin Redelings

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

#ifndef READ_TREES_H
#define READ_TREES_H

#include <vector>
#include <string>
#include <istream>
#include <filesystem>

#include "util/io.H"
#include "tree/tree.H"

namespace trees_format {

    /// A structure to read and parse a collection of trees with the same leaf set
    struct reader_t
    {
	std::vector<std::string> leaf_names;
	virtual bool current_tree(Tree&,int&)=0;

    public:
	virtual reader_t* clone() const=0;
	virtual const std::vector<std::string>& names() const;
	bool current_tree(Tree&);
	bool current_tree(RootedTree&);
	virtual bool next_tree(Tree&);
	virtual bool next_tree(RootedTree&);
	int lines() const;
	virtual bool skip(int) = 0;
	virtual bool done() const = 0;
	reader_t() = default;
	virtual ~reader_t() {}
    };

    class Newick: public reader_t
    {
	std::string line;
	std::shared_ptr<checked_ifstream> fileptr;
	std::istream& fileref;

	void initialize();

	bool current_tree(Tree&,int&);
    public:
	Newick* clone() const {return new Newick(*this);}

	bool skip(int);
	bool done() const;

	Newick(const std::filesystem::path& filename);
	Newick(std::istream&);
	~Newick();
    };

    class NEXUS: public reader_t
    {
	std::string line;
	std::shared_ptr<checked_ifstream> fileptr;
	std::istream& fileref;
	bool translate;

	void parse_translate_command(const std::string&);
	void initialize();

	bool current_tree(Tree&,int&);
    public:
	NEXUS* clone() const {return new NEXUS(*this);}

	bool skip(int);
	bool done() const;

	NEXUS(const std::filesystem::path& filename);
	NEXUS(std::istream&);
	~NEXUS();
    };

    class wrapped_reader_t: public reader_t
    {
    protected:
	std::shared_ptr<reader_t> tfr;

	bool current_tree(Tree&,int&);
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

	Newick_or_NEXUS(const std::filesystem::path&);
	Newick_or_NEXUS(std::istream&);
    };

    class Prune: public wrapped_reader_t
    {
	std::vector<std::string> prune;
	std::vector<int> prune_index;

	bool current_tree(Tree&,int&);

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
    public:
	bool skip(int);
	Subsample* clone() const {return new Subsample(*this);}
    
	Subsample(int, const reader_t&);
    };

    class Last: public wrapped_reader_t
    {
	int last;
	int lines=0;
    public:
	bool skip(int n);
	Last* clone() const {return new Last(*this);}
    
	Last(int, const reader_t&);
    };


    class Fixroot: public wrapped_reader_t
    {
	bool current_tree(Tree&,int& r);
    public:
	Fixroot* clone() const {return new Fixroot(*this);}
    
	Fixroot(const reader_t&);
    };

    class ReorderLeaves: public wrapped_reader_t
    {
	std::vector<int> mapping;
	bool current_tree(Tree&,int& r);
    public:
	ReorderLeaves* clone() const {return new ReorderLeaves(*this);}
	const std::vector<std::string>& names() const;

	ReorderLeaves(const std::vector<std::string>& leaf_order, const reader_t&);
    };

}

#endif
