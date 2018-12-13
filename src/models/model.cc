/*
  Copyright (C) 2004-2006,2009-2012 Benjamin Redelings

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

#include <set>
#include <map>

#include <boost/variant.hpp>
#include "util/ptree.H"

#include "util.H"
#include "myexception.H"
#include "models/model.H"
#include "computation/program.H"
#include "computation/expression/expression.H"
#include "computation/module.H"
#include "computation/loader.H"
#include "parser/desugar.H"

using std::vector;
using std::string;
using std::set;
using std::map;
using std::multiset;

using boost::dynamic_pointer_cast;

const string model_separator = "/";

string model_extend_path(const string& path,const string& x)
{
    if (path.empty())
	return x;
    else
	return path + model_separator + x;
}

string model_path(const vector<string>& path)
{
    return join(path, model_separator);
}

vector<string> model_split_path(const string& path)
{
    return split(path, model_separator);
}

vector<expression_ref> model_parameter_expressions(const Model& M)
{
    vector< expression_ref > sub;
    for(int i=0;i<M.n_parameters();i++) 
	sub.push_back( parameter(M.parameter_name(i)) );
    return sub;
}

std::vector< expression_ref > Model::get_parameter_values(const std::vector<int>& indices) const
{
    std::vector< expression_ref > values(indices.size());
    
    for(int i=0;i<values.size();i++)
	values[i] = get_parameter_value(indices[i]);
  
    return values;  
}

bool Model::has_bounds(int i) const 
{
    auto e = get_parameter_range(i);

    return (e and e.is_a<Bounds<double>>());
}

const Bounds<double>& Model::get_bounds(int i) const 
{
    auto e = get_parameter_range(i);

    assert(e);

    if (not e.is_a<Bounds<double>>())
	throw myexception()<<"parameter '"<<parameter_name(i)<<"' doesn't have Bounds<double>.";

    return e.as_<Bounds<double>>();
}

bool Model::compute_expression_has_bounds(int index) const
{
    int R = compute_expression_as_modifiable_reg(index);
    auto e = get_range_for_reg(R);

    return (e and e.is_a<Bounds<double>>());
}

const Bounds<double>& Model::get_bounds_for_compute_expression(int index) const
{
    auto R = compute_expression_as_modifiable_reg(index);
    auto e = get_range_for_reg(R);

    assert(e);

    if (not e.is_a<Bounds<double>>())
	throw myexception()<<"compute expression "<<index<<" doesn't have Bounds<double>.";

    return e.as_<Bounds<double>>();
}

std::vector< expression_ref > Model::get_modifiable_values(const std::vector<int>& indices) const
{
    std::vector< expression_ref > values(indices.size());
    
    for(int i=0;i<values.size();i++)
	values[i] = get_modifiable_value(indices[i]);
  
    return values;  
}

void Model::set_parameter_values(const vector<int>& indices,const vector<expression_ref>& p)
{
    assert(indices.size() == p.size());

    for(int i=0;i<indices.size();i++)
	set_parameter_value(indices[i], p[i]);
}

log_double_t Model::heated_likelihood() const
{
    // Don't waste time calculating likelihood if we're sampling from the prior.
    if (get_beta() == 0)
	return 1;
    else
	return pow(likelihood(),get_beta());
}

log_double_t Model::heated_probability_ratio(const context& C1) const
{
    auto ratios = probability_ratios(C1);
    return ratios.prior_ratio * pow(ratios.likelihood_ratio, get_beta());
}

Model::Model(const std::shared_ptr<module_loader>& L, const key_map_t& k)
    :context(L),keys(new key_map_t(k))
{ }


void show_parameters(std::ostream& o,const Model& M, bool show_hidden) {
    for(int i=0;i<M.n_parameters();i++) {
	string name = M.parameter_name(i);
	if ((not show_hidden) and name.size() > 1 and name[0] == '*') continue;

	o<<"    "<<name<<" = ";

	string output = M.recursive_evaluate_parameter(i).print();
	if (output.find(10) != string::npos or output.find(13) != string::npos)
	    output = "[multiline]";
	o<<output;
    }
    o<<"\n";
}

std::string show_parameters(const Model& M, bool show_hidden)
{
    std::ostringstream oss;
    show_parameters(oss,M, show_hidden);
    return oss.str();
}

/// \brief Check if the model M has a parameter called name
///
/// \param M      The model
/// \param name   A parameter name
///
bool has_parameter(const Model& M, const string& name)
{
    for(int i=0;i<M.n_parameters();i++)
	if (M.parameter_name(i) == name)
	    return true;
    return false;
}

/// \brief Check if the string s1 matches a pattern s2
///
/// \param s1   The string
/// \param s2   The pattern
///
boost::optional<string> pattern_match(const string& s1, const string& s2)
{
    if (s2.size() and s2[s2.size()-1] == '*')
    {
	int L = s2.size() - 1;
	if (s1.substr(0,L) != s2.substr(0,L))
	    return boost::none;
	return s1.substr(L);
    }
    else if (s1 != s2)
	return boost::none;
    else
	return string("");
}

bool operator<(const vector<string>& p1, const vector<string>& p2)
{
    // less than
    if (p1.size() > p2.size()) return true;
    // greater than
    if (p1.size() < p2.size()) return false;
  
    for(int i=0;i<p1.size();i++)
    {
	int cmp = p1[i].compare(p2[i]);
	// less than
	if (cmp < 0) return true;
	// greater than
	if (cmp > 0) return false;
    }
  
    // equal
    return false;
}
// We can think of this collection of name lists as a tree.
// - Each name list is a path from the root to a tip.
// - Each node (except the root) has a string name associated with it.
// We consider all child nodes of internal node
//  If the set of grandchild lists under child node C does not overlap with the
//   grandchild lists under any other child node, then we can remove node C.
// We should always prefer to remove deeper nodes first.
//  Thus, leaf nodes should never be removed.
// We therefore consider all internal nodes of the tree, starting
//  with the ones furthest from the root, and remove their children
//  if it is allowable.

void copy_to_vec(const ptree& p, vector<string>& names2, const string& path = "")
{
    if (not p.value_is_empty())
    {
	int i=p.get_value<int>();
	names2[i] = path;
    }

    if (not p.empty())
	for(auto& x: p)
	    copy_to_vec(x.second, names2, model_extend_path(path, x.first));
}


// here we aren't handling '*' prefixes...
void simplify(ptree& p)
{
    if (p.empty()) return;

    // 1. First we simplify all the levels below this level.
    for(auto& x: p)
	simplify(x.second);

    // 2. In order to move child-level names up to the top level, we have to avoid
    //   a. clashing with the same name at the top level
    //   b. clashing with the same name a sibling.
    // We therefore count which names at these levels occur twice and avoid them.
    // NOTE: If we have a situation like {I1::S1, S2::I1} then this approach won't simplify to {S1,I1}.
    multiset<string> names;
    for(auto& x: p)
    {
	names.insert(x.first);
	for(auto& y: x.second)
	    names.insert(y.first);
    }

    // 3. If none of the names in an entry occur twice, then we can move all the
    //    names in that entry up to the top level.
    vector<bool> move_children(p.size(), false);
    for(int i=0; i<p.size(); i++)
    {
	auto& x = p[i];
	if (x.second.empty()) continue;

	bool ok = true;
	for(auto& y: x.second)
	{
	    if (names.count(y.first) > 1)
	    {
		ok = false;
		break;
	    }
	}
	if (ok)
	    move_children[i] = true;
    }

    // 4. Move the children
    ptree p2;
    for(int i=0; i<p.size(); i++)
    {
	auto& x = p[i];

	// 4a. Move the entry w/o changing it.
	if (not move_children[i])
	    p2.push_back(std::move(x));

	// 4b. Move the children
	else
	{
	    for(auto& y: x.second)
		p2.push_back(std::move(y));

	    // 4c. Maybe move the bare entry as well.
	    if (not x.second.value_is_empty())
	    {
		x.second.clear();
		p2.push_back(std::move(x));
	    }
	}
    }

    std::swap(p,p2);
}

void add_path(ptree& p, const vector<string>& path, int value, int first=0)
{
    if (first >= path.size())
	p.put_value(value);
    else
    {
	const string& x = path[first];
	auto child = p.get_child_optional(x);
	if (not child)
	{
	    p.push_back({x, ptree("yo")});
	    child = p.get_child_optional(x);
	}
	assert(child);
	add_path(*child, path, value, first+1);
    }
}

vector<string> short_parameter_names(const vector<string>& names)
{
    // construct the name paths
    ptree paths;
    vector<bool> hidden(names.size(),false);
    for(int i=0;i<names.size();i++)
    {
	string path = names[i];

	if (path.size() and path[0] == '*')
	{
	    path = path.substr(1);
	    hidden[i] = true;
	}

	add_path(paths, model_split_path(path), i);
    }

    // Remove levels that aren't needed for disambiguation
    simplify(paths);

    // Recreate a name vector
    vector<string> names2(names.size());
    copy_to_vec(paths, names2);

    // Put hidden-ness attributes back in.
    for(int i=0;i<names2.size();i++)
	if (hidden[i])
	    names2[i] = string("*") + names2[i];

//    for(int i=0;i<names2.size();i++)
//	std::cerr<<names[i]<<" -> "<<names2[i]<<std::endl;
    return names2;
}

vector<string> parameter_names(const Model& M)
{
    vector<string> names;
    for(int i=0;i<M.n_parameters();i++)
	names.push_back( M.parameter_name(i) );

    return names;
}

vector<string> short_parameter_names(const Model& M)
{
    return short_parameter_names( parameter_names( M ) );
}

// The key may have '*', '?', 'text*', and 'text'.  Here
// - '?' matches exactly one element.
// - '*' matches 0 or more elements
// - text* matches 1 element beginning with 'text'
// - test matches 1 element that is exactly 'text'.
bool path_match(const vector<string>& key, int i, const vector<string>& pattern, int j)
{
    assert(i <= key.size());

    assert(j <= pattern.size());

    // 1. If the key is empty
    if (i==key.size())
    {
	// nothing ~ nothing
	if(j==pattern.size()) return true;

	// nothing !~ something
	if (j<pattern.size()) return false;
    }

    assert(i<key.size());

    // 2. If the key is '*'
    if (key[i] == "*") 
    {
	// (* x y z ~ [nothing]) only (if x y z ~ [nothing])
	if (j == pattern.size())
	    return path_match(key,i+1,pattern,j);

	// (* matches 0 components of j) or (* matches 1 or more components of j)
	return path_match(key,i+1,pattern,j) or path_match(key,i,pattern,j+1);
    }

    // For anything else, FAIL if we are matching against [nothing], and key != '*' only.
    if (j == pattern.size()) return false;

    assert(j<pattern.size());

    // 3. If the key is '?'
    if (key[i] == "?")
	return path_match(key,i+1,pattern,j+1);

    // 4. If the key is 'text*' or 'text'
    if (pattern_match(pattern[j],key[i]))
	return path_match(key,i+1,pattern,j+1);
    else
	return false;
}

bool path_match(const vector<string>& key, const vector<string>& pattern)
{
    return path_match(key,0,pattern,0);
}

/// \brief Find the index of model parameters that match the pattern name
///
/// \param M      The model
/// \param name   The pattern
///
map<string,map<string,int>> parameters_with_extension(const vector<string>& M, string name)
{
    map<string,map<string,int>> indices;

    const vector<string> key = model_split_path(name);
    for(int i=0;i<M.size();i++)
    {
	vector<string> pattern = model_split_path(M[i]);

	assert(not pattern.empty());

	auto m = pattern_match(pattern.back(), name);

	if (m)
	{
	    pattern.pop_back();
	    indices[model_path(pattern)][*m] = i;
	}
    }

    return indices;
}

/// \brief Find the index of model parameters that match the pattern name
///
/// \param M      The model
/// \param name   The pattern
///
map<string,map<string,int>> parameters_with_extension(const Model& M, string name)
{
    return parameters_with_extension(parameter_names(M), name);
}

vector<int> flatten(const map<string,map<string,int>>& names)
{
    vector<int> indices;
    for(auto& x: names)
	for(auto& y: x.second)
	    indices.push_back(y.second);
    return indices;
}

Model::key_map_t parse_key_map(const vector<string>& key_value_strings)
{
    Model::key_map_t keys;

    for(const auto& key_value_pair: key_value_strings )
    {
	vector<string> parse = split(key_value_pair,'=');
	if (parse.size() != 2)
	    throw myexception()<<"Ill-formed key-value pair '"<<key_value_pair<<"'.";

	string key = parse[0];

	keys[key] = json::parse(parse[1]);
    }

    return keys;
}

Module read_model(const string& filename)
{
    // 1. Read module
    return module_loader({}).load_module_from_file(filename);
}

void read_add_model(Model& M, const std::string& filename)
{
    auto m = read_model(filename);
    M += m;
    add_model(M, m.name);
}

void execute_file(const std::shared_ptr<module_loader>& L, const std::string& filename)
{
    Program P(L);
    context C(P);
    auto m = read_model(filename);
    C += m;
    C.perform_expression(var(m.name+".main"));
}

void add_model(Model& M, const std::string& name)
{
    M += name;
    string prefix = name;
    expression_ref P = var(name+".main");
//    M.add_program( P );
//    auto result = M.evaluate_program();
//    std::cout<<result<<std::endl;
//    json j = M.get_logged_parameters();
//    std::cout<<j<<std::endl;
    M.perform_expression({var("Distributions.do_log"),prefix,{var("Distributions.gen_model_no_alphabet"), P}});
}
