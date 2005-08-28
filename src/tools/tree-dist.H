#ifndef TREE_DIST_H
#define TREE_DIST_H

#include <vector>
#include <utility>
#include <list>
#include <valarray>

#include <string>
#include <iostream>

#include <map>

#include "tree.H"
#include "sequencetree.H"
#include "util.H"

SequenceTree standardized(const std::string& t);
SequenceTree standardized(const std::string& t,const std::vector<std::string>& remove);

void standardize(SequenceTree& T);
void standardize(SequenceTree& T,const std::vector<std::string>& remove);

/// Represents a division of a subset of leaf taxa into 2 groups
struct Partition {
  /// the names of all taxa 
  std::vector<std::string> names;

  /// the taxa in the first group
  std::valarray<bool> group1;

  /// the taxa in the second group
  std::valarray<bool> group2;

  /// the total number of taxa
  unsigned size() const {return group1.size();}

  std::valarray<bool> mask() const {return group1 or group2;}

  /// Create a partition from p, but masked by @mask
  Partition(const Partition& p,const std::valarray<bool>& mask);

  /// Create a partition of all taxa (g=0->g1,g=1->g2)
  Partition(const std::valarray<bool>& g);

  /// Create a partition of all taxa (g=0->g1,g=1->g2)
  Partition(const std::vector<std::string>& n,const std::valarray<bool>& g); 
 
  /// Create a partition of taxa present in @mask (g=0->g1,g=1->g2) 
  Partition(const std::valarray<bool>& g,const std::valarray<bool>& mask); 

  /// Create a partition of taxa present in @mask (g=0->g1,g=1->g2) 
  Partition(const std::vector<std::string>& n,const std::valarray<bool>& g,const std::valarray<bool>& mask);
};

Partition partition_from_names(const std::vector<std::string>&,const std::vector<std::string>&);

/// are both partitions the same?
bool operator==(const Partition& p1, const Partition& p2);

/// are both partitions consistent with each other?
bool consistent(const Partition& p1, const Partition& p2);

/// does partition @p1 imply the partition @p2?
bool implies(const Partition& p1, const Partition& p2);

/// Does any branch in @T imply the partition @p?
bool implies(const SequenceTree& T,const Partition& p);

/// Does any branch in T imply the partition p?
template <typename T>
bool implies(const std::vector<T>& partitions,const Partition& p) 
{
  bool result = false;
  for(int i=0;i<partitions.size() and not result;i++)
    if (implies(partitions[i],p)) return true;
  return false;
}

/// Does tree T imply all the partitions in @partitions?
template <typename T1,typename T2>
bool implies(const T1& T,const std::vector<T2>& partitions) 
{
  for(int i=0;i<partitions.size();i++)
    if (not implies(T,partitions[i]))
      return false;
  return true;
}

/// Which branch in @T implies partition @p?
int which_partition(const SequenceTree& T, const Partition& p);

/// Output the partition P to stream o (requires 'names' to be set)
std::ostream& operator<<(std::ostream& o, const Partition& P);

bool informative(const Partition& p);


/// A class for loading tree distributions - somewhat biased towards tree-dist-compare
class tree_sample 
{
  mutable std::map<std::string,unsigned> index;

  std::vector<std::string> leaf_names;

public:

  /// The information we store about each topology
  struct topology_record 
  {
    // the topology string
    std::string topology;

    // a tree structure for this topology
    std::vector<std::valarray<bool> > partitions;

    // how many times did we see this topology
    unsigned count;

    topology_record(const SequenceTree&,const std::string&);
  };

  // mapping from index in the sample to topology record
  std::vector<unsigned> which_topology;

  std::vector<unsigned> order;

  // the list of topologies, and associated info
  std::vector<topology_record> topologies;

  std::vector<std::string> names() const {return leaf_names;}

  SequenceTree T(int i) const;

  // this should equal trees.size(), but should be MUCH faster!
  unsigned size() const {return which_topology.size();}

  std::valarray<bool> supports_topology(const std::string& t) const;

  std::valarray<bool> supports_partition(const Partition& P) const;

  std::valarray<bool> supports_partitions(const std::vector<Partition>&) const;

  unsigned count(const Partition& P) const;
  unsigned count(const std::vector<Partition>&) const;

  double PP(const Partition& P) const;
  double PP(const std::vector<Partition>&) const;

  tree_sample(std::istream&,const std::vector<std::string>& remove,int skip=0,int max=-1,int subsample=1);
};

std::vector<Partition> get_Ml_partitions(const tree_sample& sample,double l);
std::vector<Partition> get_Ml_partitions(const tree_sample& sample,double l, const std::valarray<double>&);

std::vector<std::pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l, const std::valarray<double>&);

std::vector<std::pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l);

std::vector<Partition> get_Ml_sub_partitions(const tree_sample& sample,double l,double,int search=1);

std::vector<std::pair<Partition,unsigned> >
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,double,int search=1);

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<Partition>& partitions);

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<std::valarray<bool> >& partitions);

bool merge_partition(std::vector<Partition>& partitions,const Partition& delta);

#endif
