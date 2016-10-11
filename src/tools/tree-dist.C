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

#include <fstream>
#include "tree-dist.H"
#include "io.H"

using std::vector;
using std::valarray;

using std::string;
using std::endl;
using std::cerr;
using std::cout;
using std::istream;

using boost::dynamic_bitset;
using boost::shared_ptr;

// FIXME: next_tree_(T) may invoke virtual functions of T that cause trouble.
// EXAMPLE: T.prune_leaves( ) messeses with sequence names if not prevented by
//          calling T.Tree::prune_leaves( ).
// INTENSION: next_tree_(T) is supposed to treat T only as a Tree&, and therefore
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

    int reader_t::lines() const {
	return lines_;
    }
  
    bool reader_t::next_tree(Tree& T)
    {
	int r;
	if (next_tree_(T,r)) {
	    lines_++;
	    return true;
	}
	else
	    return false;
    }

    bool reader_t::next_tree(RootedTree& T)
    {
	int r;
	Tree& T2 = static_cast<Tree&>(T);
	if (next_tree_(T2,r)) {
	    lines_++;
	    T.reroot(r);
	    return true;
	}
	else
	    return false;
    }
  
    bool reader_t::next_tree(SequenceTree& T)
    {
	return next_tree(static_cast<Tree&>(T));
    }
  
    bool reader_t::next_tree(RootedSequenceTree& T)
    {
	return next_tree(static_cast<RootedTree&>(T));
    }
  
    bool Newick::next_tree_(Tree& T,int& r)
    {
	if (not line.size())
	    while (portable_getline(*file,line) and not line.size());
	if (not line.size()) return false;
	try {
	    r = T.parse_with_names(line,leaf_names);
	}
	catch (std::exception& e) {
	    cerr<<" Error! "<<e.what()<<endl;
	    cerr<<" Quitting read of tree file."<<endl;
	    file->setstate(std::ios::badbit);
	    return false;
	}
	line.clear();
	return not done();
    }


    bool Newick::skip(int n)
    {
	if (line.size()) {
	    line.clear();
	    n--;
	}
	for(int i=0;i<n and *file;i++)
	    portable_getline(*file,line);
	line.clear();
	return not done();
    }

    bool Newick::done() const
    {
	return (not *file);
    }

    void Newick::initialize()
    {
	SequenceTree T;
	portable_getline(*file,line);
	T.parse(line);
	leaf_names = T.get_leaf_labels();
	std::sort(leaf_names.begin(),leaf_names.end());
    
	//FIXME - this loses the first line!
    }

    Newick::Newick(const std::string& filename)
	:file(new checked_ifstream(filename,"Newick tree file"))
    {
	initialize();
    }
  
    Newick::Newick(istream& i)
	:file(&i)
    {
	initialize();
    }

    Newick::~Newick()
    {}

    bool NEXUS::skip(int n)
    {
	if (line.size()) {
	    line.clear();
	    n--;
	}
	for(int i=0;i<n and *file;i++)
	    getline(*file,line,';');
	line.clear();
	return not done();
    }

    bool NEXUS::done() const
    {
	return (not *file);
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

    bool NEXUS::next_tree_(Tree& T,int& r)
    {
	if (not line.size())
	    get_NEXUS_command(*file,line);
	if (not line.size()) return false;
	try {
	    string word;
	    int pos=0;
	    get_word_NEXUS(word,pos,line);
	    if (uppercase(word) == "END" or uppercase(word) == "ENDBLOCK") {
		file->setstate(std::ios::badbit);
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
	    file->setstate(std::ios::badbit);
	    return false;
	}
	line.clear();
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
	portable_getline(*file,line);
	if (line != "#NEXUS")
	    throw myexception()<<"NEXUS trees reader: File does not begin with '#NEXUS' and may not be a NEXUS file.";
  
	// [ and ] do not break words

	string word;

	bool in_trees_block=false;

	while(get_NEXUS_command(*file,line))
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
		    file->setstate(std::ios::badbit);
		}
	    }
	}
    }

    NEXUS::NEXUS(const std::string& filename)
	:file(new checked_ifstream(filename,"NEXUS tree file")),translate(false)
    {
	initialize();
    }

    NEXUS::NEXUS(istream& f)
	:file(&f),translate(false)
    {
	initialize();
    }

    NEXUS::~NEXUS()
    {}

    bool wrapped_reader_t::next_tree_(Tree& T,int& r) {
	if (tfr->next_tree_(T,r)) {
	    lines_++;
	    return true;
	}
	else
	    return false;
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


    Newick_or_NEXUS::Newick_or_NEXUS(const string& filename)
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

    bool Prune::next_tree_(Tree& T,int &r)
    {
	bool success = tfr->next_tree_(T,r);
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
	    int index = find_index(all_names,prune[i]);
	    if (index == -1)
		throw myexception()<<"Cannot find leaf '"<<prune[i]<<"' in sampled tree.";
	    prune_index.push_back(index);
	}
    }

    Skip::Skip(int skip, const reader_t& r)
	:wrapped_reader_t(r)
    {
	wrapped_reader_t::skip(skip);
    }

    bool Subsample::next_tree_(Tree& T,int& r)
    {
	bool success = wrapped_reader_t::next_tree_(T,r);
	wrapped_reader_t::skip(subsample-1);
	return success;
    }

    Subsample::Subsample(int s, const reader_t& r)
	:wrapped_reader_t(r),subsample(s)
    { }

    bool Max::next_tree_(Tree& T,int& r)
    {
	if (wrapped_reader_t::lines() < m)
	    return wrapped_reader_t::next_tree_(T,r);
	else
	    return false;
    }

    Max::Max(int i, const reader_t& r)
	:wrapped_reader_t(r),m(i)
    { }

    bool Fixroot::next_tree_(Tree& T,int& r)
    {
	if (wrapped_reader_t::next_tree_(T,r)) {
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

    bool ReorderLeaves::next_tree_(Tree& T,int& r)
    {
	if (wrapped_reader_t::next_tree_(T,r)) {
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

int cmp(const tree_record& t1, const tree_record& t2)
{
    int x = (int)t1.n_leaves() - (int)t2.n_leaves();

    if (x != 0) return x;

    x = (int)t1.n_internal_branches() - (int)t2.n_internal_branches();

    if (x != 0) return x;

    for(int i=0;i<t1.n_internal_branches();i++)
    {
	if (t1.partitions[i] == t2.partitions[i])
	    continue;
	else if (t1.partitions[i] < t2.partitions[i])
	    return -1;
	else
	    return 1;
    }
    return 0;
}

bool operator<(const tree_record& t1, const tree_record& t2)
{
    return cmp(t1,t2) < 0;
}

bool operator>(const tree_record& t1, const tree_record& t2)
{
    return cmp(t1,t2) > 0;
}

SequenceTree tree_sample::T(int i) const 
{
    return get_mf_tree(leaf_names,trees[i].partitions, trees[i].branch_lengths);
}

valarray<bool> tree_sample::support(const partition& p) const 
{
    valarray<bool> result(size());

    for(int i=0;i<result.size();i++) 
    {
	// Get a tree with the same topology
	const vector<dynamic_bitset<> > & T = trees[i].partitions;
    
	result[i] = implies(T,p);
    }
    return result;
}

valarray<bool> tree_sample::support(const vector<partition>& partitions) const 
{
    valarray<bool> result(size());

    vector<partition> informative_partitions = select(partitions,informative);

    for(int i=0;i<result.size();i++) 
    {
	// Get a tree with the same topology
	const vector<dynamic_bitset<> >& T = trees[i].partitions;
    
	result[i] = implies(T,informative_partitions);
    }
    return result;
}

unsigned tree_sample::count(const partition& P) const 
{
    unsigned count=0;
    for(int t=0;t<trees.size();t++) 
	if (implies(trees[t].partitions,P))
	    count ++;
   
    return count;
}

unsigned tree_sample::count(const vector<partition>& partitions) const 
{
    unsigned count=0;
    for(int t=0;t<trees.size();t++) {
	if (implies(trees[t].partitions,partitions))
	    count ++;
    }
   
    return count;
}

double tree_sample::PP(const partition& P) const 
{
    return double(count(P))/size();
}

double tree_sample::PP(const vector<partition>& partitions) const 
{
    return double(count(partitions))/size();
}

tree_record::tree_record(const Tree& T)
    :n_leaves_(T.n_leaves()),
     partitions(T.n_branches()-T.n_leafbranches()),
     branch_lengths(T.n_branches())
{ 
    vector<dynamic_bitset<> > temp(partitions.size());

    const int L = T.n_leafbranches();
    for(int i=L;i<T.n_branches();i++) {
	temp[i-L] = branch_partition(T,i);
	if (not temp[i-L][0])
	    temp[i-L].flip();
    }

    vector<int> order = iota<int>(partitions.size());
    std::sort(order.begin(),order.end(),sequence_order<dynamic_bitset<> >(temp));

    for(int i=0;i<L;i++)
    {
	if (T.branch(i).has_length())
	    branch_lengths[i] = T.branch(i).length();
	else
	    branch_lengths[i] = 1.0;
    }

    for(int i=L;i<T.n_branches();i++)
    {
	int o = order[i-L];
	partitions[i-L] = temp[o];
	if (T.branch(o+L).has_length())
	    branch_lengths[i] = T.branch(o+L).length();
	else
	    branch_lengths[i] = 1.0;
    }
}


void tree_sample::add_tree(const tree_record& T)
{
    trees.push_back(T);
}

void tree_sample::add_tree(Tree& T)
{
    //------------ check tree ---------------//
    if (has_sub_branches(T))
	throw myexception()<<"Tree has node of degree 2";

    // Compute the standardized representation
    T.standardize();
    add_tree(tree_record(T));
}

void tree_sample::add_tree(RootedTree& T)
{
    if (T.root().degree() == 2)
	T.remove_node_from_branch(T.root());

    add_tree(static_cast<Tree&>(T));
}

// What we actually want is a standardized STRING representation.
//  - We need to have ways of *reading* a Newick or NEXUS file and then
//  - ... converting to some intermediate record structure..
//  - ... which in this case in the standard string.

// We should be able to avoid sorting the leaves with every tree... in both cases!

// Reading an individual tree requires (a) one line, and (b) some state: ordered leaf names.

// Because we have to prune everything anyway, let's make the standard intermediate format
// .. be a (unrooted) Tree -- the names should be determined and given beforehand.



int tree_sample::load_file(istream& file,int skip,int subsample,int max,const vector<string>& prune)
{
    using namespace trees_format;

    //----------- Construct File Reader / Filter -----------//
    shared_ptr<reader_t> trees_in(new Newick_or_NEXUS(file));

    if (skip > 0)
	trees_in = shared_ptr<reader_t>(new Skip(skip,*trees_in));

    if (subsample > 1)
	trees_in = shared_ptr<reader_t>(new Subsample(subsample,*trees_in));

    if (max > 0)
	trees_in = shared_ptr<reader_t>(new Max(max,*trees_in));

    trees_in = shared_ptr<reader_t>(new Fixroot(*trees_in));

    if (prune.size())
	trees_in = shared_ptr<reader_t>(new Prune(prune,*trees_in));

    if (not leaf_names.size())
	leaf_names = trees_in->names();
    else 
    {
	vector<string> leaf_names2 = trees_in->names();
	if (leaf_names2.size() != leaf_names.size())
	    throw myexception()<<"New trees with "<<leaf_names2.size()<<" leaves conflict with current trees with "<<leaf_names.size()<<" leaves.";

	try {
	    compute_mapping(leaf_names, leaf_names2);
	}
	catch (bad_mapping<string>& b) {
	    throw myexception()<<"New trees are missing leaf '"<<b.missing<<"'";
	}

    }

    //------------------- Process Trees --------------------//
    RootedTree T;
    int t=0;
    while (trees_in->next_tree(T)) {
	add_tree(T);
	t++;
    }

    if (size() == 0)
	throw myexception()<<"No trees were read in!";

    return t;
}

int tree_sample::load_file(const string& filename,int skip,int subsample,int max,const vector<string>& prune)
{
    checked_ifstream file(filename,"tree samples file");
  
    int count = load_file(file,skip,subsample,max,prune);
    return count;
}

int tree_sample::append_trees(const tree_sample& trees)
{
    if (not leaf_names.size())
	leaf_names = trees.names();
    else 
    {
	if (trees.names().size() != leaf_names.size())
	    throw myexception()<<"New trees with "<<trees.names().size()<<" leaves conflict with current trees with "<<leaf_names.size()<<" leaves.";

	try {
	    compute_mapping(leaf_names, trees.names());
	}
	catch (bad_mapping<string>& b) {
	    throw myexception()<<"New trees are missing leaf '"<<b.missing<<"'";
	}

    }

    for(int i=0;i<trees.size();i++) {
	add_tree(trees[i]);
    }

    return trees.size();
}

tree_sample::tree_sample(istream& file,int skip,int subsample,int max,const vector<string>& prune)
{
    load_file(file,skip,subsample,max,prune);
}

tree_sample::tree_sample(const string& filename,int skip,int subsample,int max,const vector<string>& prune)
{
    load_file(filename,skip,subsample,max,prune);
}

void scan_trees(istream& file,int skip,int subsample,int max, const vector<string>& prune,
		const vector<string>& leaf_order, accumulator<SequenceTree>& op)
{
    using namespace trees_format;

    //----------- Construct File Reader / Filter -----------//
    shared_ptr<reader_t> trees(new Newick_or_NEXUS(file));

    if (skip > 0)
	trees = shared_ptr<reader_t>(new Skip(skip,*trees));

    if (subsample > 1)
	trees = shared_ptr<reader_t>(new Subsample(subsample,*trees));

    if (max > 0)
	trees = shared_ptr<reader_t>(new Max(max,*trees));

    trees = shared_ptr<reader_t>(new Fixroot(*trees));

    if (prune.size())
	trees = shared_ptr<reader_t>(new Prune(prune,*trees));

    if (leaf_order.size())
	trees = shared_ptr<reader_t>(new ReorderLeaves(leaf_order,*trees));

    //------------------- Process Trees --------------------//
    RootedSequenceTree T;
    while (trees->next_tree(T))
	op(T);

    //---------------------- Finalize ----------------------//
    op.finalize();
}

void scan_trees(istream& file,int skip,int subsample,int max, const vector<string>& prune,
		accumulator<SequenceTree>& op)
{
    scan_trees(file,skip,subsample,max,prune,vector<string>(),op);
}

void scan_trees(istream& file,int skip,int subsample,int max,
		accumulator<SequenceTree>& op)
{
    scan_trees(file,skip,subsample,max,vector<string>(),op);
}

