/*
  Copyright (C) 2004-2007,2010 Benjamin Redelings

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
 * @file compile.H
 *
 * @brief This file contains routines for parsing the command line and
 *        loading information from files in order to start the MCMC
 *        analysis.
 */

#ifndef SETUP_H
#define SETUP_H

#include <optional>
#include <vector>
#include <string>
#include <boost/program_options.hpp>

template <typename T>
class shared_items
{
    // unique items
    std::vector<T> items;

    void compute_partitions_for_item()
    {
	partitions_for_item.clear();
	partitions_for_item.resize(items.size());

	for(int i=0;i<n_partitions();i++)
	    if (auto item = item_for_partition[i])
		partitions_for_item[*item].push_back(i);
    }


public:

    // from partition -> item
    std::vector<std::optional<int>> item_for_partition;  

    // from item -> partition
    std::vector<std::vector<int> > partitions_for_item;

    int n_unique_items() const {return items.size();}

    int n_partitions() const {return item_for_partition.size();}

    const T& unique(int i) const {return items[i];}
    T& unique(int i)       {return items[i];}

    const T& operator[](int i) const {return items[*item_for_partition[i]];}
    T& operator[](int i)       {return items[*item_for_partition[i]];}

    void remove_nth_item(int i)
    {
	// Remove the item
	items.erase(items.begin()+i);

	// Update item indices for each partition
	for(auto& item: item_for_partition)
	    if (item)
	    {
		if (*item == i)
		    item = {};
		else if (*item > i)
		    (*item)--;
	    }

	compute_partitions_for_item();
    }

    int n_partitions_for_item(int i) const {return partitions_for_item[i].size();}

    const std::vector<T>& get_items() const {return items;}

    shared_items(const std::vector<T>& v1, const std::vector<std::optional<int>>& v2)
	:items(v1),
	 item_for_partition(v2)
	{
	    compute_partitions_for_item();
	}
};

shared_items<std::string> get_mapping(const boost::program_options::variables_map& args, const std::string& key, int n);


#endif 
