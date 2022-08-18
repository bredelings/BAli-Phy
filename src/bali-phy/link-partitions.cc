/*
  Copyright (C) 2004-2010 Benjamin Redelings

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

/**
 * @file setup.C
 *
 * @brief This file contains routines for parsing the command line and
 *        loading information from files in order to start the MCMC
 *        analysis.
 */

#include <vector>

#include <filesystem>

#include "link-partitions.H"
#include "util/mapping.H"
#include "sequence/alphabet.H"
#include "alignment/alignment-util.H"
#include "tree/tree-util.H"
#include "util/io.H"
#include "util/set.H"
#include "util/string/split.H"
#include "util/string/convert.H"

using std::ifstream;
using std::string;
using std::vector;
using std::set;
using std::valarray;
using std::cout;
using std::cerr;
using std::endl;

using std::shared_ptr;
using std::optional;

namespace fs = std::filesystem;
namespace po = boost::program_options;

using po::variables_map;

/// \brief Parse a string of the form int,int,int:string 
///
/// \param model The string to parse.
/// \param partitions The list of integers.
/// \return the string.
///
string parse_partitions_and_model(string s, vector<int>& partitions, int n, bool default_model = true)
{
    int colon = s.find(':');
    string value;
    if (colon == -1)
    {
	if (default_model)
	{
	    partitions.clear();
	    value = s;
	}
	else
	{
	    partitions = convertTo<int>(split(s,','));
	    value = "";
	}
    }
    else
    {
	partitions = convertTo<int>(split(s.substr(0,colon),','));
	value = s.substr(colon+1);
    }

    // Adjust numbers from [1,n] -> [0,n-1].   Also check for bad partition numbers.
    for(int& p: partitions)
    {
	if (p < 1 or p > n) throw myexception()<<"Partition "<<p<<" doesn't exist.";
	p--;
    }

    for(int i=1;i<partitions.size();i++)
	for(int j=0;j<i;j++)
	    if (partitions[i] == partitions[j])
		throw myexception()<<"Partition '"<<i<<"' occurs twice in group '"<<s<<"'";

    return value;
}


/// Handle commands like --link=1,2: or --link=1,2:alphabet,smodel
/// For a given attribute (imodel, smodel, alphabet, scale) a partition should not be mentioned more than once.

static set<string> valid_attributes {"smodel","imodel","scale","alphabet"};

vector<vector<int>> get_link_groups(const variables_map& args, const string& key, int n)
{
    if (not args.count("link")) return vector<vector<int>>();

    auto link_group_names = args["link"].as<vector<string>>();
    vector<string> relevant_link_group_names;

    vector<vector<int>> link_groups;
    for(auto& name: link_group_names)
    {
	vector<int> partitions;
	string keys = parse_partitions_and_model(name, partitions, n, false);
	if (partitions.size() < 2)
	    throw myexception()<<"You can't link < 2 partitions in link command:\n  '--link="<<name<<"'";

	auto attributes = split(keys,",");

	if (not keys.empty())
	{
	    for(auto& attribute: attributes)
		if (not valid_attributes.count(attribute))
		    throw myexception()<<"Attribute '"<<attribute<<"' not recognized in link command:\n  '--link="<<name<<"'";
	}

	if (keys.empty() or includes(attributes,key))
	{
	    relevant_link_group_names.push_back(name);
	    link_groups.push_back(partitions);
	}
    }

    vector<optional<int>> which_link_group(n);
    for(int i=0; i< link_groups.size();i++)
    {
	for(int j: link_groups[i])
	    if (which_link_group[j])
		throw myexception()<<"Partition "<<i+1<<" in two --link groups with attribute '"<<key<<"':\n  '--link="
				   <<relevant_link_group_names[*which_link_group[j]]<<"'\n  '--link="
				   <<relevant_link_group_names[i]<<"'"; // both group which_link_group[j] and i.
	    else
		which_link_group[j] = i;
    }
    return link_groups;
}

// {0,1,4} -> {0,1,-1,-1,2}
vector<int> mapping_from_order(const vector<int>& order, int n)
{
    assert(order.size() <= n);
    vector<int> mapping(n, -1);
    for(int i=0;i<order.size();i++)
	mapping[order[i]] = i;
    return mapping;
}

template<typename T>
vector<optional<T>> compose(const vector<optional<int>>& mapping1, const vector<T>& mapping2)
{
    assert(mapping1.size() == mapping2.size());

    vector<optional<T>> mapping3(mapping1.size());

    for(int i=0;i<mapping3.size();i++)
    {
	if (not mapping1[i]) continue;
	int j = *mapping1[i];
	assert(j >=0 and j < mapping2.size());
	mapping3[i] = mapping2[j];
    }

    return mapping3;
}

shared_items<string> remove_empty_partitions(const shared_items<string>& M)
{
    vector<int> order;
    for(int i=0;i<M.partitions_for_item.size();i++)
	if (not M.partitions_for_item[i].empty())
	    order.push_back(i);
    auto mapping = mapping_from_order(order, M.n_partitions());

    return shared_items<string>(apply_indices(M.get_items(), order), compose(M.item_for_partition, mapping));
}

shared_items<string> link_partitions(shared_items<string> M, const vector<vector<int>>& link_groups)
{
    // 0. Precondition: none of the link_groups contain the same elements.

    for(auto& link_group: link_groups)
    {
	// 1. Complain if trying to link elements that are already in a group > 1
	for(auto p: link_group)
	{
	    auto item_index = M.item_for_partition[p];
	    if (not item_index) continue;
	    int group_size = M.n_partitions_for_item(*item_index);
	    if (group_size > 1)
		throw myexception()<<"Partition "<<p+1<<" cannot be used in --link command: already linked!";
	}

	// 2. Complain if trying to link elements with different values
	for(int i=0;i<link_group.size();i++)
	{
	    int p0 = link_group[0];
	    int p1 = link_group[i];

	    auto item_index0 = M.item_for_partition[p0];
	    auto item_index1 = M.item_for_partition[p1];

	    if (not item_index0 and not item_index1)
		continue;

	    if (not item_index0 and item_index1)
		throw myexception()<<"Partitions "<<p0+1<<" and "<<p1+1<<" cannot be linked because they have differing values 'none' and '"<<M[p1]<<"'";
		
	    if (item_index0 and not item_index1)
		throw myexception()<<"Partitions "<<p0+1<<" and "<<p1+1<<" cannot be linked because they have differing values '"<<M[p0]<<"' and 'none'";
		
	    if (M[p0] != M[p1])
		throw myexception()<<"Partitions "<<p0+1<<" and "<<p1+1<<" cannot be linked because they have differing values '"<<M[p0]<<"' and '"<<M[p1]<<"'";
	}

	// 3. Move all items into the partition of the first item.
	for(int i=1;i<link_group.size();i++)
	{
	    int p0 = link_group[0];
	    int p1 = link_group[i];

	    auto item_index0 = M.item_for_partition[p0];
	    auto item_index1 = M.item_for_partition[p1];

	    assert((item_index0 and item_index1) or (not item_index0 and not item_index1));

	    if (item_index0 and item_index1)
	    {
		assert(M.partitions_for_item[*item_index1].size() == 1);
		M.partitions_for_item[*item_index1].clear();
		M.partitions_for_item[*item_index0].push_back(p1);
		M.item_for_partition[p1] = *item_index0;
	    }
	}
    }

    // 4. Remove empty partitions
    return remove_empty_partitions(M);
}
/// \brief Parse command line arguments of the form --key int,int,int:name1 --key int,int:name2
///
/// Here the integers refer to partitions 1..n and so cannot be referred to twice.
///
/// \param args The command line arguments.
/// \param key The key.
/// \param n The number of partitions that exist.
/// \return a mapping from partitions to names.
///

/// We should be able to modify this by doing something like: --link 1,2:alphabet --link 2,3:smodel
/// The rules are:
/// 1. We can only link things that are not already in a group, so --smodel=1,2: --link=2,3:smodel would not work.
/// 2. We can only link things are are both specified or both unspecified, so --smodel=1: --smodel=2:HKY --link=1,2:smodel would not work.
/// 3. For any given attribute to link, no partition can be mentioned twice.

shared_items<string> get_mapping(const variables_map& args, const string& key, int n)
{
    vector<string> models;
    if (args.count(key))
	models = args[key].as<vector<string> >();

    vector<optional<int>> mapping(n,-2);
    vector<string> model_names;

    // If we just have --key=name, then each partition gets a separate version of 'name'
    if (models.size() == 1) 
    {
	vector<int> partitions;
	string model_name = parse_partitions_and_model(models[0], partitions, n);
      
	if (partitions.size() == 0) 
	{
	    if (model_name == "none")
		mapping = vector<optional<int>>(n);
	    else 
		for(int i=0;i<mapping.size();i++)
		{
		    mapping[i] = i;
		    model_names.push_back(model_name);
		}
	    return shared_items<string>(model_names,mapping);
	}
    }

    /// For each argument --key {int}+:name
    for(int i=0;i<models.size();i++) 
    {
	// 1. Parse {int}+:name into partitions and model_name
	vector<int> partitions;

	optional<int> index;
	string model_name = parse_partitions_and_model(models[i], partitions, n);
	if (model_name != "none")
	{
	    index = model_names.size();
	    model_names.push_back(model_name);
	}

	// 2. Check that partitions have been specified ...
	if (partitions.size() == 0) 
	{
	    // unless there is only one partition, or ...
	    if (n == 1)
		partitions.push_back(1);
	    else {
		assert(models.size() > 1);
		throw myexception()<<"Failed to specify partition number(s) for '"<<key<<"' specification '"<<models[i];
	    }
	}

	// 3. Map partitions to this model, unless they are already mapped
	for(int j=0;j<partitions.size();j++) 
	{
	    // Check for partition already mapped.
	    if (mapping[partitions[j]] != -2)
		throw myexception()<<"Trying to set '"<<key<<"' for partition "<<partitions[j]+1<<" twice.";

	    // Map the partition to this model.
	    mapping[partitions[j]] = index;
	}
    }

    // Every unmentioned partition gets a mapping to a unique ""
    for(int i=0;i<mapping.size();i++)
	if (mapping[i] == -2) 
	{
	    mapping[i] = model_names.size();
	    model_names.push_back("");
	}

    auto M = shared_items<string>(model_names,mapping);

    return link_partitions(M, get_link_groups(args, key, n));
}
