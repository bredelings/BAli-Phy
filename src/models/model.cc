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

#include "util/ptree.H"

#include "util/string/join.H"
#include "util/string/split.H"
#include "util/myexception.H"
#include "models/model.H"
#include "models/path.H"
#include "computation/program.H"
#include "computation/module.H"
#include "computation/loader.H"

#include "tools/stats-table.H"

using std::vector;
using std::string;
using std::pair;
using std::set;
using std::map;
using std::multiset;

using boost::dynamic_pointer_cast;

template <typename T>
using Bounds = Box<bounds<T>>;

namespace fs = boost::filesystem;

Model::Model(const context_ref& C, const key_map_t& k)
    :context(C),keys(new key_map_t(k))
{ }

Model::Model(const Program& P, const key_map_t& k)
    :context(P),keys(new key_map_t(k))
{ }

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

// Kind of like unnesting, but we call flatten_value on children.
json flatten_me(const json& j)
{
    json j2;
    for(auto& [name, obj]: j.items())
    {
        if (has_children(name))
        {
            json c = flatten_me(obj);
            for(auto& [name2,j3]: c.items())
                j2[name+name2] = std::move(j3);
        }
        else // Without this transofmration, we are just unnesting.
            for(auto& [name2,value2]: flatten_value(name, obj))
                j2[name2] = std::move(value2);
    }
    return j2;
}

void simplify(json& j)
{
    assert(j.is_object());

    if (j.empty()) return;

    // 1. First we simplify all the levels below this level.
    for(auto& [name, obj]: j.items())
        if (has_children(name))
            simplify(obj);

    // 2. In order to move child-level names up to the top level, we have to avoid
    //   a. clashing with the same name at the top level
    //   b. clashing with the same name a sibling.
    // We therefore count which names at these levels occur twice and avoid them.
    // NOTE: If we have a situation like {I1/S1, S2/I1} then this approach won't simplify to {S1,I1}.
    multiset<string> names;
    for(auto& [name, obj]: j.items())
    {
        names.insert(name);
        if (has_children(name))
            for(auto& [name2, j2]: obj.items())
                names.insert(name2);
    }

    // 3. Check if we can move the children for each key up to the top level
    //    names in that entry up to the top level.
    vector<pair<string,json>> moved;
    for(auto iter = j.begin(); iter != j.end(); )
    {
        auto& name = iter.key();
        auto& obj = iter.value();

        if (has_children(name))
        {
            bool collision = false;
            for(auto& [name2, _]: obj.items())
            {
                if (names.count(name2) > 1)
                {
                    collision = true;
                    break;
                }
            }

            if (not collision)
            {
                for(auto& [name2, j2]: obj.items())
                {
                    moved.push_back({name2,std::move(j2)});
                }
                iter = j.erase(iter);
            }
            else
                ++iter;
        }
        else
            ++iter;
    }

    json j2;
    for(auto& [name,obj]: moved)
        j[name] = std::move(obj);
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

void execute_file(const std::shared_ptr<module_loader>& L, const fs::path& filename)
{
    Program P(L);
    auto m = L->load_module_from_file(filename);
    P.add(m);
    P.main = m.name + ".main";

    context C(P);
}
