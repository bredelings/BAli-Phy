#ifndef MCTREE_H
#define MCTREE_H

#include <vector>
#include <string>
#include "tree-dist.H"
#include "mytypes.H"

struct edge {
  int from;
  int to;
  int type;
  int partition;

  edge(int f,int t,int T,int p)
    :from(f),to(t),type(T),partition(p)
  {}
};

struct MC_tree 
{
  int N;
  std::vector<std::string> names_;

  std::vector<int> reverse_;

public:
  SequenceTree T;

  std::vector<Partition> partitions;
  int n_nodes;
  std::vector<edge> edges;
  std::vector<int> mapping;

  // partition properties
  ublas::matrix<int> left_of;
  ublas::matrix<int> wanders_over;
  ublas::matrix<int> directly_left_of;
  ublas::matrix<int> directly_wanders_over;
  ublas::matrix<int> connected_to;
  vector<bool> directly_wanders;

  // node properties
  ublas::matrix<int> connected;

  const std::vector<std::string>& names() const {return names_;}

  int n_leaves() const {return names_.size();}
  int n_branches() const {return N;}
  int reverse(int i) const {return reverse_[i];}

  MC_tree(const std::vector<Partition>&);
};

void draw_graph(const MC_tree& T,const std::string& name);

MC_tree load_MC_tree(const std::string&);

#endif
