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


#include "read-trees.H"
#include <fstream>
#include "tree-dist.H"
#include "util/string/strip.H"
#include "util/string/pred.H"
#include "util/set.H"
#include "util/mapping.H"

using std::vector;
using std::valarray;

using std::string;
using std::endl;
using std::cerr;
using std::cout;
using std::istream;

using boost::dynamic_bitset;
using std::shared_ptr;

namespace fs = std::filesystem;

// FIXME: current_tree_(T) may invoke virtual functions of T that cause trouble.
// EXAMPLE: T.prune_leaves( ) messeses with sequence names if not prevented by
//          calling T.Tree::prune_leaves( ).
// INTENSION: current_tree(T) is supposed to treat T only as a Tree&, and therefore
//             NOT call not call any virtual functions.  
// QUESTION: should I fix the code that calls it, or the code itself, to not have this
//             assumption?
// QUESTION: is there some virtual function that should be added to class Tree to handle
//             some of the things going on here?

namespace trees_format 
{
    const vector<string>& reader_t::names() const
    {
	return leaf_names;
    }

    bool reader_t::current_tree(Tree& T)
    {
	int r;
	return current_tree(T,r);
    }

    bool reader_t::current_tree(RootedTree& T)
    {
	int r;
	if (current_tree(T,r))
	{
	    T.reroot(r);
	    return true;
	}
	else
	    return false;
    }

    bool reader_t::next_tree(Tree& T)
    {
	return current_tree(T) and skip(1);
    }

    bool reader_t::next_tree(RootedTree& T)
    {
	return current_tree(T) and skip(1);
    }
  
    bool Newick::current_tree(Tree& T,int& r)
    {
	if (not line.size())
	    while (portable_getline(fileref,line) and not line.size());
	if (not line.size()) return false;
	try {
	    r = T.parse_with_names(line,leaf_names);
	}
	catch (std::exception& e) {
	    cerr<<" Error! "<<e.what()<<endl;
	    cerr<<" Quitting read of tree file."<<endl;
	    fileref.setstate(std::ios::badbit);
	    return false;
	}
	return not done();
    }


    bool Newick::skip(int n)
    {
	if (line.size()) {
	    line.clear();
	    n--;
	}
	for(int i=0;i<n and fileref;i++)
	{
	    portable_getline(fileref,line);
	}
	line.clear();
	return not done();
    }

    bool Newick::done() const
    {
	return (not fileref);
    }

    void Newick::initialize()
    {
	SequenceTree T;
	portable_getline(fileref,line);
	T.parse(line);
	leaf_names = T.get_leaf_labels();
	std::sort(leaf_names.begin(),leaf_names.end());
    
	//FIXME - this loses the first line!
    }

    Newick::Newick(const fs::path& filename)
	:fileptr(new checked_ifstream(filename,"Newick tree file")),
	 fileref(*fileptr)
	 
    {
	initialize();
    }
  
    Newick::Newick(istream& i)
	:fileref(i)
    {
	initialize();
    }

    Newick::~Newick()
    {
    }

    bool NEXUS::skip(int n)
    {
	if (line.size()) {
	    line.clear();
	    n--;
	}
	for(int i=0;i<n and fileref;i++)
	{
	    getline(fileref, line, ';');
	}
	line.clear();
	return not done();
    }

    bool NEXUS::done() const
    {
	return (not fileref);
    }

    istream& get_NEXUS_command(istream& file,std::string& s)
    {
	static const string eol = "\n\r";

	istream& is = getline(file,s,';');
	s = strip(s,eol);
	return is;
    }

    string strip_NEXUS_comments(const string& s)
    {
	string s2;
	s2.resize(s.size());

	bool in_comment = false;

	int j=0;
	for(int i=0;i<s.size();i++)
	{
	    if (s[i] == '[')
		in_comment = true;
	    else if (s[i] == ']')
		in_comment = false;
	    else if (not in_comment)
		s2[j++] = s[i];
	}

	s2.resize(j);
	return s2;
    }

    void NEXUS_skip_ws(int& i, const string& s)
    {
	static const string whitespace = "\t\n\r ";

	bool in_comment = false;

	while(contains_char(whitespace,s[i]) or in_comment or s[i]=='[') 
	{
	    if (s[i] == '[')
		in_comment = true;
	    else if (s[i] == ']')
		in_comment = false;

	    i++;

	    if (i >= s.size()) 
		return;
	} 
    }


    bool get_word_NEXUS(string& word, int& i, const string& s)
    {
	//  static const string delimiters = "()[]{}/\\,;:=*`'\"+-<>";
	// REMOVE apostrophe... how to handle this?
	static const string delimiters = "(){}/\\,;:=*`\"+-<>";
	static const string whitespace = "\t\n\r ";

	if (i >= s.size()) 
	    return false;

	NEXUS_skip_ws(i,s);

	if (i >= s.size()) 
	    return false;

	int start = i;
	if (contains_char(delimiters,s[i])) {
	    word = s.substr(i,1);
	    i++;
	    return true;
	}

	do { i++; }
	while(not contains_char(delimiters,s[i]) and not contains_char(whitespace,s[i])
	      and i < s.size());

	word = s.substr(start,i-start);

	return true;
    }

    vector<string> NEXUS_parse_line(const string& line)
    {
	string word;
	vector<string> words;
	int pos=0;
	while (get_word_NEXUS(word,pos,line))
	    words.push_back(word);

	return words;
    }

    string uppercase(string word) 
    {
	for(int i=0;i<word.size();i++)
	    word[i] = std::toupper(word[i]);
	return word;
    }

    bool NEXUS::current_tree(Tree& T,int& r)
    {
	if (not line.size())
	    get_NEXUS_command(fileref,line);
	if (not line.size()) return false;
	try {
	    string word;
	    int pos=0;
	    get_word_NEXUS(word,pos,line);
	    if (uppercase(word) == "END" or uppercase(word) == "ENDBLOCK") {
		fileref.setstate(std::ios::badbit);
		return false;
	    }
	    get_word_NEXUS(word,pos,line);
	    if (word == "*")
		get_word_NEXUS(word,pos,line);
	    if (not (word == "=")) {
		get_word_NEXUS(word,pos,line);
		assert(word == "=");
	    }
	    NEXUS_skip_ws(pos,line);
      
	    string t = strip_NEXUS_comments(line.substr(pos,line.size()-pos)) + ";";

	    // if we have no leaf names, for some reason, numbers will be allowed.
	    r = T.parse_with_names_or_numbers(t, leaf_names);
	}
	catch (std::exception& e) {
	    cerr<<" Error! "<<e.what()<<endl;
	    cerr<<" Quitting read of tree file."<<endl;
	    fileref.setstate(std::ios::badbit);
	    return false;
	}
	return not done();
    }

    void NEXUS::parse_translate_command(const std::string& s)
    {
	translate = true;
  
	//  cerr<<"translating: "<<line<<endl;
  
	vector<string> words = NEXUS_parse_line(s);

	if (words.size()%3 != 2)
	    throw myexception()<<"Malformed 'TRANSLATE' command: wrong number of tokens.";

	leaf_names.resize(words.size()/3+1);
	for(int i=0;i<leaf_names.size();i++) 
	{
	    leaf_names[i] = words[i*3+1];

	    if (i>0) assert(words[i*3-1] == ",");
	}
    }

    void NEXUS::initialize()
    {
	// Check #NEXUS
	portable_getline(fileref,line);
	if (line != "#NEXUS")
	    throw myexception()<<"NEXUS trees reader: File does not begin with '#NEXUS' and may not be a NEXUS file.";
  
	// [ and ] do not break words

	string word;

	bool in_trees_block=false;

	while(get_NEXUS_command(fileref,line))
	{
	    //    cerr<<"line: "<<line<<endl;
	    int pos=0;
      
	    if (not get_word_NEXUS(word,pos,line)) continue;

	    //      cerr<<"NEXUS: command = :"<<word<<":"<<"     in_trees_block = "<<in_trees_block<<endl;
      
	    // Parse BEGIN TREES
	    if (uppercase(word) == "BEGIN") {
		if (not get_word_NEXUS(word,pos,line)) continue;
		if (uppercase(word) == "TREES")
		    in_trees_block = true;
	    }
      
	    if (not in_trees_block) continue;
      
	    // Parse TRANSLATE ...
	    if (uppercase(word) == "TRANSLATE") {
		parse_translate_command(line.substr(pos,line.size()-pos));
		//      cerr<<"leaf names = "<<join(leaf_names,',')<<endl;
		line.clear();
		return;
	    }
	    else if (uppercase(word) == "TREE" or uppercase(word) == "UTREE") {
		try {
		    get_word_NEXUS(word,pos,line);
		    if (word == "*")
			get_word_NEXUS(word,pos,line);
		    if (not (word == "="))
			get_word_NEXUS(word,pos,line);
		    NEXUS_skip_ws(pos,line);

		    SequenceTree T;
		    string t = strip_NEXUS_comments(line.substr(pos,line.size()-pos)) + ";";
		    T.parse(t);
		    leaf_names = T.get_leaf_labels();
		    std::sort(leaf_names.begin(),leaf_names.end());
		    return;
		}
		catch (std::exception& e) {
		    cerr<<" Error! "<<e.what()<<endl;
		    cerr<<" Quitting read of tree file."<<endl;
		    fileref.setstate(std::ios::badbit);
		}
	    }
	}
    }

    NEXUS::NEXUS(const fs::path& filename)
	:fileptr(new checked_ifstream(filename,"NEXUS tree file")),
	 fileref(*fileptr),
	 translate(false)
    {
	initialize();
    }

    NEXUS::NEXUS(istream& f)
	:fileref(f),
	 translate(false)
    {
	initialize();
    }

    NEXUS::~NEXUS()
    {
    }

    bool wrapped_reader_t::current_tree(Tree& T,int& r)
    {
	return tfr->current_tree(T,r);
    }

    const vector<string>& wrapped_reader_t::names() const
    {
	return tfr->names();
    }

    bool wrapped_reader_t::skip(int i) {
	return tfr->skip(i);
    }

    bool wrapped_reader_t::done() const {
	return tfr->done();
    }

    wrapped_reader_t::wrapped_reader_t() {}

    wrapped_reader_t::wrapped_reader_t(const reader_t&r) 
	:tfr(r.clone())
    { }


    Newick_or_NEXUS::Newick_or_NEXUS(const fs::path& filename)
    {
	checked_ifstream file(filename,"tree file");

	if (file.peek() == '#')
	    tfr = shared_ptr<reader_t>(new NEXUS(filename));
	else
	    tfr = shared_ptr<reader_t>(new Newick(filename));
    }

    Newick_or_NEXUS::Newick_or_NEXUS(istream& file)
    {
	if (file.peek() == '#') 
	    tfr = shared_ptr<reader_t>(new NEXUS(file));
	else
	    tfr = shared_ptr<reader_t>(new Newick(file));
    }

    const vector<string>& Prune::names() const
    {
	return leaf_names;
    }

    bool Prune::current_tree(Tree& T,int &r)
    {
	bool success = tfr->current_tree(T,r);
	if (success and prune_index.size()) {
	    T.Tree::prune_leaves(prune_index);
	    // FIXME -- there has to be a more intelligent way to do this...
	    r = T.n_nodes()-1;
	}
	return success;
    }

    Prune::Prune(const vector<string>& p,const reader_t& r)
	:wrapped_reader_t(r),prune(p)
    {
	vector<string> all_names = tfr->names();
	for(int i=0;i<all_names.size();i++) {
	    if (not includes(prune,all_names[i]))
		leaf_names.push_back(all_names[i]);
	}

	for(int i=0;i<prune.size();i++) {
	    if (auto index = find_index(all_names,prune[i]))
		prune_index.push_back(*index);
	    else
		throw myexception()<<"Cannot find leaf '"<<prune[i]<<"' in sampled tree.";
	}
    }

    Skip::Skip(int skip, const reader_t& r)
	:wrapped_reader_t(r)
    {
	wrapped_reader_t::skip(skip);
    }

    bool Subsample::skip(int n)
    {
	return tfr->skip(subsample*n);
    }

    Subsample::Subsample(int s, const reader_t& r)
	:wrapped_reader_t(r),subsample(s)
    { }

    bool Last::skip(int n)
    {
	lines += n;
	return (tfr->skip(n) and lines < last);
    }

    Last::Last(int l, const reader_t& r)
	:wrapped_reader_t(r),last(l)
    { }

    bool Fixroot::current_tree(Tree& T,int& r)
    {
	if (wrapped_reader_t::current_tree(T,r)) {
	    if (T.node(r).degree() == 2) {
		T.remove_node_from_branch(r);
		r = T.n_nodes()-1;
	    }
	    return true;
	}
	else
	    return false;
    }

    Fixroot::Fixroot(const reader_t& r)
	:wrapped_reader_t(r)
    { }

    const vector<string>& ReorderLeaves::names() const
    {
	return leaf_names;
    }

    bool ReorderLeaves::current_tree(Tree& T,int& r)
    {
	if (wrapped_reader_t::current_tree(T,r)) {
	    if (mapping.size()) T.standardize(mapping);
	    return true;
	}
	else
	    return false;
    }

    ReorderLeaves::ReorderLeaves(const vector<string>& leaf_order, const reader_t& r)
	:wrapped_reader_t(r)
    {
	leaf_names = leaf_order;
	mapping = compute_mapping(tfr->names(), leaf_order);
    }
}

