#ifndef TREE_DIST_H
#define TREE_DIST_H

#include <vector>
#include <valarray>
#include <boost/shared_ptr.hpp>

#include <string>
#include <iostream>

#include <map>

#include "partition.H"
#include "tree.H"
#include "sequencetree.H"
#include "util.H"

namespace trees_format {

  struct reader_t
  {
    std::vector<std::string> leaf_names;
    virtual bool next_tree_(Tree&,int&)=0;
    int lines_;

  public:
    virtual reader_t* clone() const=0;
    virtual const std::vector<std::string>& names() const;
    virtual bool next_tree(Tree&);
    virtual bool next_tree(RootedTree&);
    virtual bool next_tree(SequenceTree&);
    virtual bool next_tree(RootedSequenceTree&);
    int lines() const;
    virtual bool skip(int) = 0;
    virtual bool done() const = 0;
    virtual ~reader_t() {}
  };

  class Newick: public reader_t
  {
    std::string line;
    std::istream* file;

    void initialize();

    bool next_tree_(Tree&,int&);
  public:
    Newick* clone() const {return new Newick(*this);}

    bool skip(int);
    bool done() const;

    Newick(const std::string& filename);
    Newick(std::istream&);
    ~Newick();
  };

  class NEXUS: public reader_t
  {
    std::string line;
    std::istream* file;
    bool translate;

    void parse_translate_command(const std::string&);
    void initialize();

    bool next_tree_(Tree&,int&);
  public:
    NEXUS* clone() const {return new NEXUS(*this);}

    bool skip(int);
    bool done() const;

    NEXUS(const std::string& filename);
    NEXUS(std::istream&);
    ~NEXUS();
  };

  class wrapped_reader_t: public reader_t
  {
  protected:
    boost::shared_ptr<reader_t> tfr;

    bool next_tree_(Tree&,int&);
  public:
    virtual wrapped_reader_t* clone() const=0;
    const std::vector<std::string>& names() const;

    bool skip(int);
    bool done() const;

    wrapped_reader_t();
    wrapped_reader_t(const reader_t&);
  };

  class Newick_or_NEXUS: public wrapped_reader_t
  {
  public:
    Newick_or_NEXUS* clone() const {return new Newick_or_NEXUS(*this);}

    Newick_or_NEXUS(const std::string&);
    Newick_or_NEXUS(std::istream&);
  };

  class Prune: public wrapped_reader_t
  {
    std::vector<std::string> prune;
    std::vector<int> prune_index;

    bool next_tree_(Tree&,int&);

  public:
    Prune* clone() const {return new Prune(*this);}
    const std::vector<std::string>& names() const;

    Prune(const std::vector<std::string>&, const reader_t&);
  };

  class Skip: public wrapped_reader_t
  {
  public:
    Skip* clone() const {return new Skip(*this);}

    Skip(int, const reader_t&);
  };

  class Subsample: public wrapped_reader_t
  {
    int subsample;

    bool next_tree_(Tree&,int& r);
  public:
    Subsample* clone() const {return new Subsample(*this);}
    
    Subsample(int, const reader_t&);
  };

  class Max: public wrapped_reader_t
  {
    int m;

    bool next_tree_(Tree&,int& r);
  public:
    Max* clone() const {return new Max(*this);}
    
    Max(int, const reader_t&);
  };


  class Fixroot: public wrapped_reader_t
  {
    bool next_tree_(Tree&,int& r);
  public:
    Fixroot* clone() const {return new Fixroot(*this);}
    
    Fixroot(const reader_t&);
  };

}

/// A class for loading tree distributions - somewhat biased towards tree-dist-compare
class tree_sample 
{
  mutable std::map<std::string,unsigned> index;

  std::vector<std::string> leaf_names;

  void load_file(std::istream&,int skip=0,int max=-1,int subsample=1,const std::vector<std::string>& prune=std::vector<std::string>());

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

void scan_trees(std::istream&,int skip,int subsample,int max,accumulator<SequenceTree>& op);

#endif
