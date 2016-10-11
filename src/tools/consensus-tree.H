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

#ifndef CONSENSUS_TREE_H
#define CONSENSUS_TREE_H

#include <utility>
#include <boost/dynamic_bitset.hpp>
#include <map>
#include <vector>
#include "tree-dist.H"

/// The count for a partition in several different tree samples
struct p_counts {
    std::vector<int> counts;
    p_counts(int n):counts(n,0) {}
};

/// The count and average length for a partition
struct count_and_length {
    unsigned count;
    double length;
    count_and_length():count(0),length(0) {}
    count_and_length(unsigned u, double l):count(u),length(l) {}
};

std::map< boost::dynamic_bitset<>, p_counts > get_multi_partitions_and_counts(const std::vector<tree_sample>& samples);

std::vector<partition> get_Ml_partitions(const tree_sample& sample,double l);
std::vector<partition> get_Ml_partitions(const tree_sample& sample,double l, const boost::dynamic_bitset<>&);

std::vector<std::pair<partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l, const boost::dynamic_bitset<>&);

std::vector<std::pair<partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l);

std::map<boost::dynamic_bitset<>,count_and_length>
get_partition_counts_and_lengths(const tree_sample& sample);

std::vector<partition> get_Ml_sub_partitions(const tree_sample& sample,double l,double,int search=1);

std::vector<std::pair<partition,unsigned> >
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,double,int search=1);

std::vector<std::pair<partition,unsigned> >
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,const boost::dynamic_bitset<>&,
				 double,int search=1);

#endif
