#ifndef CONSENSUS_TREE_H
#define CONSENSUS_TREE_H

#include <utility>

#include "tree-dist.H"

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
