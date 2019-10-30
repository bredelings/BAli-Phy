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

#include "util/string/join.H"
#include "util/string/split.H"
#include "util/myexception.H"
#include "models/model.H"
#include "models/path.H"
#include "computation/program.H"
#include "computation/expression/parameter.H"
#include "computation/module.H"
#include "computation/loader.H"
#include "computation/parser/desugar.H"

using std::vector;
using std::string;
using std::pair;
using std::set;
using std::map;
using std::multiset;

using boost::dynamic_pointer_cast;

template <typename T>
using Bounds = Box<bounds<T>>;

vector<expression_ref> model_parameter_expressions(const Model& M)
{
    vector< expression_ref > sub;
    for(int i=0;i<M.n_parameters();i++) 
	sub.push_back( parameter(M.parameter_name(i)) );
    return sub;
}

EVector Model::get_parameter_values(const std::vector<int>& indices) const
{
    EVector values(indices.size());
    
    for(int i=0;i<values.size();i++)
	values[i] = get_parameter_value(indices[i]);
  
    return values;  
}

bool Model::parameter_has_bounds(int i) const
{
    auto e = get_parameter_range(i);

    return (e and e.is_a<Bounds<double>>());
}

const bounds<double>& Model::get_parameter_bounds(int i) const
{
    auto e = get_parameter_range(i);

    assert(e);

    if (not e.is_a<Bounds<double>>())
	throw myexception()<<"parameter '"<<parameter_name(i)<<"' doesn't have bounds<double>.";

    return e.as_<Bounds<double>>();
}

EVector Model::get_modifiable_values(const std::vector<int>& indices) const
{
    EVector values(indices.size());
    
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


void simplify(json& j);
json flatten_me(const json& j);

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
    auto j = M.get_logged_parameters();
    simplify(j);
    j = flatten_me(j);
    for(auto& [key,j2]: j.items())
        o<<"   "<<key<<" = "<<j2;
    o<<"\n";
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
std::optional<string> pattern_match(const string& s1, const string& s2)
{
    if (s2.size() and s2[s2.size()-1] == '*')
    {
	int L = s2.size() - 1;
	if (s1.substr(0,L) != s2.substr(0,L))
	    return {};
	return s1.substr(L);
    }
    else if (s1 != s2)
	return {};
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

json* has_children(json& j)
{
    if (not j.is_object()) return nullptr;

    auto children = j.find("children");
    if (children == j.end()) return nullptr;
    if (not children->is_object()) return nullptr;
    return &(*children);
}

const json* has_children(const json& j)
{
    if (not j.is_object()) return nullptr;

    auto children = j.find("children");
    if (children == j.end()) return nullptr;
    if (not children->is_object()) return nullptr;
    return &(*children);
}

// This me be a good candidate for the range library.
vector<pair<string,json>> flatten_value(const string& name, const json& value)
{
    vector<pair<string,json>> values;
    if (value.is_object())
    {
        for(auto& [name2,v2]: value.items())
            for(auto& p: flatten_value(name+"["+name2+"]", v2))
                values.push_back(std::move(p));
    }
    else if (value.is_array())
    {
        for(int i=0;i<value.size();i++)
            for(auto& p: flatten_value(name+"["+std::to_string(i+1)+"]", value[i]))
                values.push_back(std::move(p));
    }
    else
        values = {{name,value}};
    return values;
}

json flatten_me(const json& j)
{
    json j2;
    for(auto& [name, obj]: j.items())
    {
        if (auto children = has_children(obj))
        {
            json c = flatten_me(*children);
            for(auto& [name2,j3]: c.items())
                j2[name+"/"+name2] = std::move(j3);
        }
        
        if (auto value = obj.find("value"); value != obj.end())
            for(auto& [name2,v2]: flatten_value(name, *value))
                j2[name2] = std::move(v2);
    }
    return j2;
}

void simplify(json& j)
{
    assert(j.is_object());

    if (j.empty()) return;

    // 1. First we simplify all the levels below this level.
    for(auto& [_, obj]: j.items())
        if (auto children = has_children(obj))
            simplify(*children);

    // 2. In order to move child-level names up to the top level, we have to avoid
    //   a. clashing with the same name at the top level
    //   b. clashing with the same name a sibling.
    // We therefore count which names at these levels occur twice and avoid them.
    // NOTE: If we have a situation like {I1/S1, S2/I1} then this approach won't simplify to {S1,I1}.
    multiset<string> names;
    for(auto& [name, obj]: j.items())
    {
        names.insert(name);
        if (auto children = has_children(obj))
            for(auto& [name2, j2]: children->items())
                names.insert(name2);
    }

    // 3. Check if we can move the children for each key up to the top level
    //    names in that entry up to the top level.
    vector<pair<string,json>> moved;
    for(auto iter = j.begin(); iter != j.end(); )
    {
        auto& obj = iter.value();

        if (auto children = has_children(obj))
        {
            bool collision = false;
            for(auto& [name2, _]: children->items())
            {
                if (names.count(name2) > 1)
                {
                    collision = true;
                    break;
                }
            }

            if (not collision)
            {
                for(auto& [name2, j2]: children->items())
                {
                    moved.push_back({name2,std::move(j2)});
                }
                obj.erase("children");
            }
        }

        if (obj.empty())
            iter = j.erase(iter);
        else
            ++iter;
    }

    json j2;
    for(auto& [name,obj]: moved)
        j[name] = std::move(obj);
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

int read_add_model(Model& M, const std::string& filename)
{
    auto m = read_model(filename);
    M += m;
    return add_model(M, m.name);
}

void execute_file(const std::shared_ptr<module_loader>& L, const std::string& filename)
{
    Program P(L);
    context C(P);
    auto m = read_model(filename);
    C += m;
    C.perform_expression(var(m.name+".main"));
}

int add_model(Model& M, const std::string& name)
{
    M += name;
    string prefix = name;
    expression_ref P = var(name+".main");
    return M.add_program( P );
}
