#ifndef TREE_DIST_H
#define TREE_DIST_H

#include <vector>
#include <valarray>
#include <utility>
#include <list>
#include <boost/dynamic_bitset.hpp>

#include <string>
#include <iostream>

#include <map>

#include "tree.H"
#include "sequencetree.H"
#include "util.H"

RootedSequenceTree standardized(const std::string& t);

void standardize(RootedSequenceTree& T);

void standardize(SequenceTree& T);


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

struct trees_file_reader_t
{
  std::vector<std::string> leaf_names;
public:
  virtual const std::vector<std::string>& names() const {return leaf_names;}
  virtual bool next_tree(Tree&) = 0;
  virtual bool next_tree(RootedTree&) = 0;
  virtual bool skip(int) = 0;
  virtual bool done() const = 0;
  virtual ~trees_file_reader_t() {}
};

class NEWICK_trees_file_reader: public trees_file_reader_t
{
  std::string line;
  std::istream* file;
public:
  bool next_tree(Tree&);
  bool next_tree(RootedTree&);
  bool skip(int);
  bool done() const;
  NEWICK_trees_file_reader(const std::string& filename);
  NEWICK_trees_file_reader(std::istream&);
  ~NEWICK_trees_file_reader();
};

class NEXUS_trees_file_reader: public trees_file_reader_t
{
  std::string line;
  std::istream* file;
  bool translate;
  void parse_translate_command(const std::string&);
public:
  bool next_tree(Tree&);
  bool next_tree(RootedTree&);
  bool skip(int);
  bool done() const;
  NEXUS_trees_file_reader(const std::string& filename);
  NEXUS_trees_file_reader(std::istream&);
  ~NEXUS_trees_file_reader();
};

class pruned_trees_file_reader: public trees_file_reader_t
{
  trees_file_reader_t& tfr; 
  std::vector<std::string> prune;
  std::vector<int> prune_index;
public:
  bool next_tree(Tree&);
  bool next_tree(RootedTree&);
  bool skip(int);
  bool done() const;
  pruned_trees_file_reader(trees_file_reader_t& t,const std::vector<std::string>&);
};

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
    std::vector<boost::dynamic_bitset<> > partitions;

    // how many times did we see this topology
    unsigned count;

    topology_record(const Tree&,const std::string&);
  };

  /// Add an tree with indices following leaf_names
  void add_tree(Tree& T);
  void add_tree(RootedTree& T);

  // mapping from index in the sample to topology record
  std::vector<unsigned> which_topology;

  std::vector<unsigned> order;

  // the list of topologies, and associated info
  std::vector<topology_record> topologies;

  std::vector<std::string> names() const {return leaf_names;}

  SequenceTree T(int i) const;

  // this should equal trees.size(), but should be MUCH faster!
  unsigned size() const {return which_topology.size();}

  std::valarray<bool> support(const std::string& t) const;

  std::valarray<bool> support(const Partition& P) const;

  std::valarray<bool> support(const std::vector<Partition>&) const;

  unsigned count(const Partition& P) const;
  unsigned count(const std::vector<Partition>&) const;

  double PP(const Partition& P) const;
  double PP(const std::vector<Partition>&) const;

  tree_sample(std::istream&,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());
  tree_sample(const std::string& filename,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());
};

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

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<Partition>& partitions);

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<boost::dynamic_bitset<> >& partitions);

bool merge_partition(std::vector<Partition>& partitions,const Partition& delta);

/// load a collection of partition sets from a file
void load_partitions(const std::string& filename, std::vector<std::vector<Partition> >&);

void write_partitions(std::ostream& o,const std::vector<Partition>& partitions);

void scan_trees(std::istream&,int skip,int subsample,int max,accumulator<SequenceTree>& op);

#endif
