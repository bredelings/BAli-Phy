#ifndef POSSIBILITIES_H
#define POSSIBILITIES_H

#include "mytypes.H"
#include "tree.H"

struct edge {
  int node1;
  int node2;
  edge() {node1=-1;node2=-1;}
  edge(int n1,int n2): node1(n1),node2(n2) {}
};

bool operator==(const edge&, const edge&);

typedef vector<edge>  possibility;

int num_shared(const possibility&,const possibility&);

//which indels could generate the pattern present/missing leaves?
vector<possibility> get_possibilities(const vector<int>&,const tree&);

vector<edge> get_children(const tree&,const edge&,const TreeFunc<int>&);

#endif
