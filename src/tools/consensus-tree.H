#ifndef CONSENSUS_TREE_H
#define CONSENSUS_TREE_H

#include <utility>
#include <boost/dynamic_bitset.hpp>
#include <map>
#include <vector>
#include "tree-dist.H"

struct p_counts {
  std::vector<int> counts;
  p_counts(int n):counts(n,0) {}
};


std::map< boost::dynamic_bitset<>, p_counts > get_multi_partitions_and_counts(const std::vector<tree_sample>& samples);

std::vector<Partition> get_Ml_partitions(const tree_sample& sample,double l);
std::vector<Partition> get_Ml_partitions(const tree_sample& sample,double l, const boost::dynamic_bitset<>&);

std::vector<std::pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l, const boost::dynamic_bitset<>&);

std::vector<std::pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l);

std::vector<Partition> get_Ml_sub_partitions(const tree_sample& sample,double l,double,int search=1);

std::vector<std::pair<Partition,unsigned> >
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,double,int search=1);

std::vector<std::pair<Partition,unsigned> >
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,const boost::dynamic_bitset<>&,
				 double,int search=1);

#endif
