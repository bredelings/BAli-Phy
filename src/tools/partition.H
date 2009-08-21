#ifndef PARTITION_H
#define PARTITION_H

#include <vector>
#include <string>
#include <boost/dynamic_bitset.hpp>

#include "tree.H"
#include "sequencetree.H"

/// Represents a division of a subset of leaf taxa into 2 groups
struct Partition {
  /// the names of all taxa 
  std::vector<std::string> names;

  /// the taxa in the first group
  boost::dynamic_bitset<> group1;

  /// the taxa in the second group
  boost::dynamic_bitset<> group2;

  /// Is this a full partition?
  bool full() const; 

  /// the total number of taxa
  unsigned size() const {return group1.size();}

  Partition& flip();
  
  Partition reverse() const;

  boost::dynamic_bitset<> mask() const {return group1 | group2;}

  /// Create a partition from p, but masked by @mask
  Partition(const Partition& p,const boost::dynamic_bitset<>& mask);

  /// Create a partition of all taxa (g=0->g1,g=1->g2)
  Partition(const boost::dynamic_bitset<>& g);

  /// Create a partition of all taxa (g=0->g1,g=1->g2)
  Partition(const std::vector<std::string>& n,const boost::dynamic_bitset<>& g); 
 
  /// Create a partition of taxa present in @mask (g=0->g1,g=1->g2) 
  Partition(const boost::dynamic_bitset<>& g,const boost::dynamic_bitset<>& mask); 

  /// Create a partition of taxa present in @mask (g=0->g1,g=1->g2) 
  Partition(const std::vector<std::string>& n,const boost::dynamic_bitset<>& g,const boost::dynamic_bitset<>& mask);
  /// Create a partition from its description
  Partition(const std::string& line);
  /// Create a partition from its description with names already ordered
  Partition(const std::vector<std::string>& n,const std::string& line);
};

boost::dynamic_bitset<> group_from_names(const std::vector<std::string>& names,const std::vector<std::string>& subset);

Partition partition_from_branch(const SequenceTree& T,int b);

std::vector<Partition> all_partitions_from_tree(const SequenceTree& T);
std::vector<Partition> internal_partitions_from_tree(const SequenceTree& T);

Partition full_partition_from_names(const std::vector<std::string>&,const std::vector<std::string>&);

Partition partition_from_names(const std::vector<std::string>&,const std::vector<std::string>&,const std::vector<std::string>&);

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

/// Write the partition P to stream o (requires 'names' to be set)
std::ostream& operator<<(std::ostream& o, const Partition& P);

bool informative(const Partition& p);
bool valid(const Partition& p);

/// load a collection of partition sets from a file
void load_partitions(const std::string& filename, std::vector<std::vector<Partition> >&);

void write_partitions(std::ostream& o,const std::vector<Partition>& partitions);

bool merge_partition(std::vector<Partition>& partitions,const Partition& delta);

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<Partition>& partitions);

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<boost::dynamic_bitset<> >& partitions);

#endif 
