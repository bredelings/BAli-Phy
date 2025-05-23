/*
  Copyright (C) 2009-2010 Benjamin Redelings

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

///
/// \file   partition.C
/// \brief  Provides routines to for handling splits: bi-partitions on trees.
///
/// In addition to implementing the class Partition, this file
/// provides numerous other routines for handling partitions.
///
/// \author Benjamin Redelings
/// 


#include "partition.H"

#include <fstream>

#include "util/mapping.H"
#include "util/set.H"
#include "util/io.H"
#include "util/string/split.H"

using std::string;
using std::vector;
using std::endl;
using std::pair;

using boost::dynamic_bitset;

bool directed_equals(const partition& p1, const partition& p2)
{
    return (p1.group1 == p2.group1) and (p1.group2 == p2.group2);
}

bool operator==(const partition& p1, const partition& p2) {
    return  ((p1.group1 == p2.group1) and (p1.group2 == p2.group2)) or
	    ((p1.group1 == p2.group2) and (p1.group2 == p2.group1))
	    ;
}

bool operator==(const Partition& p1, const Partition& p2) {
    return 
	(p1.names == p2.names) and 
	(
	    ((p1.group1 == p2.group1) and (p1.group2 == p2.group2)) or
	    ((p1.group1 == p2.group2) and (p1.group2 == p2.group1))
	    );
}

bool undirected_set_equals(const vector<partition>& P1,const vector<partition>& P2) 
{
    return includes(P1,P2) and includes(P2,P1);
}

bool operator==(const PTree& t1, const PTree& t2)
{
    return t1.names == t2.names and undirected_set_equals(t1.partitions, t2.partitions);
}

dynamic_bitset<> group_from_names(const vector<string>& names,const vector<string>& subset)
{
    assert(subset.size() <= names.size());

    dynamic_bitset<> group(names.size());

    for(auto& taxon: subset)
    {
	if (auto index = find_index(names, taxon))
	    group[*index] = true;
	else
	    throw myexception()<<"Can't find taxon '"<<taxon<<"' in taxa set.";
    }

    return group;
}

Partition partition_from_branch(const SequenceTree& T,int b) 
{
    dynamic_bitset<> group(T.n_leaves());
    const dynamic_bitset<>& with_internal = T.partition(b);

    for(int i=0;i<group.size();i++)
	group[i] = with_internal[i];

    return Partition(T.get_leaf_labels(), group);
}


Partition full_partition_from_names(const vector<string>& names, const vector<string>& names1) 
{
    dynamic_bitset<> group1 = group_from_names(names,names1);

    return Partition(names,group1);
}


Partition partition_from_names(const vector<string>& names, const vector<string>& names1,
			       const vector<string>& names2)
{
    dynamic_bitset<> group1 = group_from_names(names,names1);
    dynamic_bitset<> group2 = group_from_names(names,names2);

    return Partition(names, group1, group1 | group2);
}

vector<partition> internal_partitions_from_tree(const SequenceTree& T) 
{
    vector<partition> partitions;

    for(int b=T.n_leafbranches();b<T.n_branches();b++)
	partitions.push_back(partition_from_branch(T,b));

    return partitions;
}


vector<partition> all_partitions_from_tree(const SequenceTree& T) 
{
    vector<partition> partitions;

    for(int b=0;b<T.n_branches();b++)
	partitions.push_back(partition_from_branch(T,b));

    return partitions;
}


bool partition::full() const {
    for(int i=0;i<size();i++)
	if (not group1[i] and not group2[i])
	    return false;
    return true;
}

partition& partition::flip() {
    std::swap(group1,group2);
    return *this;
}

partition reverse(const partition& p1) {
    auto p2 = p1;
    p2.flip();
    return p2;
}

Partition reverse(const Partition& p1) {
    auto p2 = p1;
    p2.flip();
    return p2;
}

partition::partition(const partition& p,const dynamic_bitset<>& mask)
    :group1(p.group1 & mask),
     group2(p.group2 & mask)
{
    assert(mask.size() == p.group1.size());
    assert(not group1.intersects(group2));
}

partition::partition(const dynamic_bitset<>& g) 
    :group1(~g),group2(g)
{ 
    assert(not group1.intersects(group2));
}

Partition::Partition(const Partition& p,const dynamic_bitset<>& mask)
    :partition(p,mask),
     names(p.names)
{ }

Partition::Partition(const vector<string>& n, const partition& p)
    :partition(p),
     names(n)
{ }

Partition::Partition(const dynamic_bitset<>& g)
    :partition(g)
{ }

Partition::Partition(const vector<string>& n,const dynamic_bitset<>& g) 
    :partition(g),names(n)
{ }

Partition::Partition(const dynamic_bitset<>& g,const dynamic_bitset<>& mask) 
    :partition(g,mask)
{ }

Partition::Partition(const vector<string>& n,const dynamic_bitset<>& g,const dynamic_bitset<>& mask) 
    :partition(g,mask), names(n)
{
    assert(n.size() == g.size());
    assert(g.size() == mask.size());
    assert(not group1.intersects(group2));
}

// line -> word (space* word)*
// word = quoted_word | unquoted_word
// quoted_word = get_char(") >> letter* >> get_char(")
// letter = escaped_letter | unescaped_letter
// escaped_letter = Char('\\')>>get_char

// unquoted_word = unescaped_letter+
// unescaped_letter = char that is not '\\','"', or ' '.

struct parse_input
{
    const string* data;
    int pos;
    bool ok() const {return pos < data->size();}
    int operator++() {return pos++;}
    std::optional<char> get_ch() {
        if (ok())
            return (*data)[pos++];
        else
            return {};
    }
    parse_input(const string& s):data(&s),pos(0) {}
};

template <typename T>
struct parse_result_ok
{
    T output;
    parse_input next_input;
};

template <>
struct parse_result_ok<void>
{
    parse_input next_input;
};

template <typename T>
using parse_result = std::optional<parse_result_ok<T>>;

template <typename T>
using parser = std::function<parse_result<T>(parse_input)>;

parse_result<char> get_char_(parse_input input)
{
    if (auto c = input.get_ch())
        return {{*c,input}};
    else
        return {};
}


parser<char> get_char = get_char_;

parser<char> Char(char c)
{
    return [c](parse_input input) -> parse_result<char>
               {
                   auto result = get_char(input);
                   if (result and result->output == c)
                       return result;
                   else
                       return {};
               };
}

parser<string> String(const string& s)
{
    return [&](parse_input input) -> parse_result<string>
               {
                   for(char c: s)
                       if (not Char(c)(input))
                           return {};
                   return {};
               };
}

template <typename T1,typename T2>
constexpr parser<pair<T1,T2>> match_both(const parser<T1>& p1, const parser<T2>& p2)
{
    return [&](parse_input input) -> parse_result<pair<T1,T2>>
               {
                   auto result1 = p1(input);
                   if (result1)
                   {
                       auto result2 = p2(result1->next_input);
                       if (result2)
                           return {{{result1->output,result2->output},result2->next_input}};
                       else
                           return {};
                   }
                   else
                       return {};
               };
}

template <typename T1,typename T2>
constexpr parser<pair<T1,T2>> operator>>(const parser<T1>& p1, const parser<T2>& p2)
{
    return match_both(p1,p2);
}

template <typename T>
constexpr parser<T> match_first(const parser<T>& p1, const parser<T>& p2)
{
    return [&](parse_input input)
               {
                   if (auto result1 = p1(input))
                       return result1;
                   else
                       return p2(input);
               };
}

template <typename T>
constexpr parser<T> operator|(const parser<T>& p1, const parser<T>& p2)
{
    return match_first(p1,p2);
}


parse_result<char> escaped_letter_(parse_input input)
{
    auto result = (Char('\\')>>get_char)(input);
    if (result)
        return {{result->output.second, result->next_input}};
    else
        return {};
}

parse_result<char> unescaped_letter_(parse_input input)
{
    auto result = get_char(input);
    if (result)
    {
	char c = result->output;
	if (c == '\\' or c == '"' or c == ' ')
	    return {};
	else
	    return result;
    }
    else
	return {};
}

parser<char> escaped_letter = escaped_letter_;
parser<char> unescaped_letter = unescaped_letter_;


parser<char> letter = escaped_letter|unescaped_letter;

template <typename T>
struct vector_out
{
    typedef vector<T> value_type;
};

template <>
struct vector_out<char>
{
    typedef string value_type;
};

template <typename T>
parser<typename vector_out<T>::value_type> match_star(const parser<T>& p)
{
    return [&](parse_input input) -> parse_result<typename vector_out<T>::value_type>
               {
                   vector<T> results;
                   while (true)
                   {
                       auto result = p(input);
                       if (not result)
                           return {{results,input}};
                       input = result->next_input;
                       results.push_back(result->output);
                   }
               };
}

template <>
parser<typename vector_out<char>::value_type> match_star(const parser<char>& p)
{
    return [&](parse_input input) -> parse_result<typename vector_out<char>::value_type>
               {
                   string results;
                   while (true)
                   {
                       auto result = p(input);
                       if (not result)
                           return {{results,input}};
                       input = result->next_input;
                       results += result->output;
                   }
               };
}

template <typename T>
constexpr parser<typename vector_out<T>::value_type> operator*(const parser<T>& p)
{
    return match_star(p);
}


template <typename T>
parser<typename vector_out<T>::value_type> match_plus(const parser<T>& p)
{
    return [&](parse_input input) -> parse_result<typename vector_out<T>::value_type>
               {
                   auto result1 = p(input);
                   if (not result1)
                       return {};
                   input = result1->next_input;
                   vector<T> results;
                   results.push_back(result1->output);
                   while (true)
                   {
                       auto result2 = p(input);
                       if (not result2)
                           return {{results,input}};
                       input = result2->next_input;
                       results.push_back(result2->output);
                   }
               };
}

template <>
parser<typename vector_out<char>::value_type> match_plus(const parser<char>& p)
{
    return [&](parse_input input) -> parse_result<typename vector_out<char>::value_type>
               {
                   auto result1 = p(input);
                   if (not result1)
                       return {};
                   input = result1->next_input;
                   string results;
                   results += result1->output;
                   while (true)
                   {
                       auto result2 = p(input);
                       if (not result2)
                           return {{results,input}};
                       input = result2->next_input;
                       results += result2->output;
                   }
               };
}

template <typename T>
constexpr parser<typename vector_out<T>::value_type> operator+(const parser<T>& p)
{
    return match_plus(p);
}



parse_result<string> quoted_word_(parse_input input)
{
    auto result1 = Char('"')(input);
    if (not result1)
        return {};
    input = result1->next_input;

    auto result2 = (*(letter|Char(' ')))(input);
    if (not result2)
        return {};
    input = result2->next_input;

    auto result3 = Char('"')(input);
    if (not result3)
        return {};
    input = result3->next_input;
    
    return {{result2->output,input}};
}

parser<string> quoted_word = quoted_word_;

parser<string> unquoted_word = +unescaped_letter;

parser<string> word = quoted_word | unquoted_word;

template <typename T1, typename T2>
parser<vector<T1>> intercalate(const parser<T1>& p1, const parser<T2>& p2)
{
    return [&](parse_input input) -> parse_result<vector<T1>>
    {
        vector<T1> output;
        auto result1 = p1(input);
        if (result1)
        {
            output.push_back(result1->output);
            input = result1->next_input;

            auto result2 = (*(p2>>p1))(input);
            assert(result2);
            input = result2->next_input;
            for(auto& x: result2->output)
                output.push_back(x.second);
        }
        return {{output,input}};
    };
}

template <typename T>
std::optional<T> parse(const string& s, const parser<T>& p)
{
    parse_input input(s);
    auto result = p(input);
    if (result) // We might not consume the whole line here
        return result->output;
    else
        return {};
}

vector< vector<string> > parse_partition(const string& line)
{
    auto result = parse(line, intercalate(word,+Char(' ')));
    vector<string> all_names = *result;

    vector< vector<string> > names(3);

    int group = 0;
    for(int i=0; i< all_names.size(); i++) 
    {
	if (all_names[i] == "|") {
	    assert(group == 0);
	    group++; 
	}
	else if (all_names[i] == "[") {
	    assert(group == 1);
	    group++;
	}
	else if (all_names[i] == "]") {
	    assert(group == 2);
	    group++;
	}
	else if (all_names[i].size())
	    names[group].push_back(all_names[i]);
    }

    return names;
}


Partition::Partition(const vector<string>& n,const string& line) 
    :names(n)
{
    vector< vector<string> > name_groups = parse_partition(line);

    group1 = group_from_names(names,name_groups[0]);
    group2 = group_from_names(names,name_groups[1]);
    assert(not group1.intersects(group2));
}

Partition::Partition(const string& line)
{
    names.clear();
    vector< vector<string> > name_groups = parse_partition(line);
    for(int i=0;i<name_groups.size();i++)
	names.insert(names.end(), name_groups[i].begin(), name_groups[i].end());

    std::sort(names.begin(),names.end());
    group1.resize(names.size());
    group2.resize(names.size());
  
    group1 = group_from_names(names,name_groups[0]);
    group2 = group_from_names(names,name_groups[1]);
    assert(not group1.intersects(group2));
}

/// \brief Check if the split is informative
///
/// \param p The split
bool informative(const partition& p) {
    return (p.group1.count() >= 2) and (p.group2.count() >= 2);
}

/// \brief Check if the split is informative
///
/// \param p The split
bool informative(const boost::dynamic_bitset<>& p) {
    int N = p.size();
    int C = p.count();
    return (C >= 2) and ((N-C) >= 2);
}

bool valid(const partition& p) {
    return p.group1.any() and p.group2.any();
}

string quote(const string& s)
{
    if (s.find(' ') != std::string::npos)
        return '"'+s+'"';
    else
        return s;
}

std::ostream& operator<<(std::ostream& o, const Partition& P) 
{
    assert(not P.group1.intersects(P.group2));

    for(int i=0;i<P.size();i++)
	if (P.group1[i]) o<<quote(P.names[i])<<" ";
  
    o<<"| ";
  
    for(int i=0;i<P.size();i++)
	if (P.group2[i]) o<<quote(P.names[i])<<" ";

    dynamic_bitset<> rmask = ~(P.group1 | P.group2);
    if (rmask.any()) {
	o<<" [ ";
	for(int i=0;i<P.size();i++) {
	    if (rmask[i]) o<<quote(P.names[i])<<" ";
	}
	o<<"]";
    }
    return o;
}

bool consistent(const partition& p1, const partition& p2) {
    if (not p1.group1.intersects(p2.group1)) return true;
    if (not p1.group1.intersects(p2.group2)) return true;

    if (not p1.group2.intersects(p2.group1)) return true;
    if (not p1.group2.intersects(p2.group2)) return true;

    return false;
}


/// Does the grouping of all nodes bm, imply *this?
bool implies(const partition& p1, const partition& p2) 
{
    if (p2.group1.is_subset_of(p1.group1) and p2.group2.is_subset_of(p1.group2)) return true;

    if (p2.group2.is_subset_of(p1.group1) and p2.group1.is_subset_of(p1.group2)) return true;

    return false;
}

/// Does the grouping of all nodes bm, imply *this?
bool directed_implies(const partition& p1, const partition& p2) 
{
    if (p2.group1.is_subset_of(p1.group1) and p2.group2.is_subset_of(p1.group2)) return true;

    return false;
}

/// Does any branch in T imply the partition p?
bool implies(const SequenceTree& T,const partition& p) {
    bool result = false;
    for(int b=0;b<T.n_branches() and not result;b++) {
	dynamic_bitset<> bp = branch_partition(T,b);

	if (implies(bp,p)) return true;
    }
    return false;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
vector<partition> unimplied_partitions(const vector<partition>& partitions,const vector<partition>& delta) 
{
    vector<partition> unimplied;

    for(int i=0;i<delta.size();i++)
	if (not implies(partitions,delta[i]))
	    unimplied.push_back(delta[i]);

    return unimplied;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
bool merge_partition(vector<partition>& partitions,const partition& delta) 
{
    if (implies(partitions,delta)) 
	return false;

    for(int i=partitions.size()-1;i>=0;i--)
	if (implies(delta,partitions[i]))
	    partitions.erase(partitions.begin()+i);

    partitions.push_back(delta);

    return true;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
bool merge_partitions(vector<partition>& partitions,const vector<partition>& delta) 
{
    bool changed=false;

    for(int i=0;i<delta.size();i++) 
	if (merge_partition(partitions,delta[i]))
	    changed = true;

    return changed;
}

int which_branch(const SequenceTree& T, const partition& p) 
{
    for(int b=0; b<2*T.n_branches(); b++) {
	dynamic_bitset<> bp = branch_partition(T,b);
	if( directed_implies(bp,p) )
	    return b;
    }
    return -1;
}

/// \brief Load a list of partition lists from file 'filename'
///
/// \param filename    The file from which to load the partition lists
/// \param partitions  The list of partition lists
///
/// Load a list of partition lists from file 'filename'.  Blank
/// lines separate the collections of partitions. A collection 
/// can begin with a Newick tree, which is then decomposed into all
/// of its partitions (both informative and uninformative).
///
vector<PTree> load_partitions(const string& filename)
{
    checked_ifstream file(filename, "splits file");

    vector<PTree> trees;

    string line;
    while(file) {
	PTree tree;

	while(portable_getline(file,line) and line.size()) {
	    if (line[0] == '(') {
		SequenceTree T = standardized(line);
		tree.names = T.get_leaf_labels();
		vector<partition> TP = all_partitions_from_tree(T);
		tree.partitions.insert(tree.partitions.end(),TP.begin(),TP.end());
	    }
	    else
	    {
		auto P = Partition(line);
		if (tree.names.empty())
		    tree.names = P.names;
		tree.partitions.push_back(P);
	    }
	}

	if (not tree.names.empty() and not includes(trees,tree))
	    trees.push_back(tree);
    }

    return trees;
}

void write_partitions(std::ostream& o, const PTree& tree)
{
    auto& partitions = tree.partitions;
    vector<partition> full;
    vector<partition> sub;
    for(int i=0;i<partitions.size();i++)
	if (partitions[i].full())
	    full.push_back(partitions[i]);
	else
	    sub.push_back(partitions[i]);

    if (full.size()) {
	SequenceTree consensus = get_mf_tree(tree.names,full);
	o<<consensus.write(false)<<endl;
    }

    for(int i=0;i<sub.size();i++)
	o<<Partition(tree.names,sub[i])<<endl;
}


SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<partition>& partitions) 
{
    SequenceTree T = star_tree(names);

    int i=0;
    try {
	for(;i<partitions.size();i++)
	    T.induce_partition(partitions[i].group1);
    }
    catch(...) {
	throw myexception()<<"Partition ("<<Partition(names,partitions[i])<<") conflicts with tree "<<T;
    }

    for(int i=0;i<T.n_branches();i++)
	T.branch(i).set_length(1.0);

    return T;
}

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<dynamic_bitset<> >& partitions) 
{
    SequenceTree T = star_tree(names);

    for(int i=0;i<partitions.size();i++)
	T.induce_partition(partitions[i]);

    for(int i=0;i<T.n_branches();i++)
	T.branch(i).set_length(1.0);

    return T;
}

SequenceTree get_mf_tree(const PTree& T)
{
    return get_mf_tree(T.names, T.partitions);
}


SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<dynamic_bitset<> >& partitions,
			 const std::vector<double>& branch_lengths) 
{
    SequenceTree T = star_tree(names);
    for(int b=0;b<T.n_branches();b++)
	T.branch(b).set_length(branch_lengths[b]);

    const int LB = T.n_branches();

    assert(branch_lengths.size() == LB + partitions.size());

    for(int i=0;i<partitions.size();i++)
    {
	int b = T.induce_partition(partitions[i]);
	T.branch(b).set_length(branch_lengths[LB+i]);
    }

    return T;
}

/// Remove uninformative branches, then add leaf branches in front
void add_leaf_partitions(const vector<string>& names, vector<partition>& partitions)
{
    // remove uninformative branches
    for(int i=0;i<partitions.size();)
	if (not informative(partitions[i]))
	    partitions.erase(partitions.begin()+i);
	else
	    i++;

    if (names.size() < 2) return;

    // adds leaf branch for n=2
    if (names.size() == 2 and partitions.size() == 0)
    {
	dynamic_bitset<> m(names.size()); m.flip();
	m[0] = false;
	partitions.push_back({m});
	return;
    }


    // adds leaf branch for n > 2 leafs
    for(int i=0;i<names.size();i++) {
	dynamic_bitset<> m(names.size()); m.flip();
	m[i] = false;
	partitions.insert(partitions.begin()+i,{m});
    }
}

vector<partition> get_star_partitions(const vector<string>& names)
{
    vector<partition> partitions;
    add_leaf_partitions(names,partitions);
    return partitions;
}
