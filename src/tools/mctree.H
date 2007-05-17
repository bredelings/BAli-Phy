#ifndef MCTREE_H
#define MCTREE_H

#include <vector>
#include <string>
#include "tree-dist.H"

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

public:
  std::vector<Partition> partitions;
  int n_nodes;
  std::vector<edge> edges;
  std::vector<int> mapping;

  const std::vector<std::string>& names() const {return names_;}

  int n_leaves() const {return names_.size();}
  int n_branches() const {return N;}

  MC_tree(const std::vector<Partition>&);
};

void draw_graph(const MC_tree& T,const std::string& name);

#endif
