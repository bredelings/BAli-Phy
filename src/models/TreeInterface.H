#ifndef TREE_INTERFACE_H
#define TREE_INTERFACE_H

#include <vector>
#include <boost/dynamic_bitset.hpp>
#include "tree/tree.H"
#include "object.H"

class Parameters;

class TreeInterface
{
  void read_tree_node(const Tree&, int n);
  void end_modify_node(int n);
public:
  const Parameters* P;

  int n_nodes() const;
  int n_branches() const;
  int n_leaves() const;
  int n_leaf_branches() const;

  int degree(int n) const;
  int branch_out(int n, int i) const;
  int branch_in(int n, int i) const;
  int neighbor(int n, int i) const;
  std::vector<int> neighbors(int n) const;
  std::vector<int> branches_out(int n) const;
  std::vector<int> branches_in(int n) const;
  void append_branches_before(int b, std::vector<int>&) const;
  void append_branches_after(int b, std::vector<int>&) const;
  std::vector<int> branches_before(int b) const;
  std::vector<int> branches_after(int b) const;
  std::vector<int> all_branches_before_inclusive(int b) const;
  std::vector<int> all_branches_after_inclusive(int b) const;
  std::vector<int> all_branches_from_node(int n) const;
  std::vector<int> all_branches_toward_node(int n) const;
  int source(int b) const;
  int target(int b) const;
  int reverse(int b) const;
  
  int undirected(int b) const;

  bool is_connected(int n1, int n2) const;

  bool subtree_contains(int b,int n) const;
  bool subtree_contains_branch(int b1,int b2) const;
  
  bool is_leaf_node(int n) const;
  bool is_internal_node(int n) const;
  
  bool is_leaf_branch(int b) const;
  bool is_internal_branch(int b) const;

  int search_branch(int n1, int n2) const;
  int find_branch(int n1, int n2) const;
  int find_undirected_branch(int n1, int n2) const;
  int find_branch(const tree_edge&) const;
  int find_undirected_branch(const tree_edge&) const;

  boost::dynamic_bitset<> partition(int b) const;
  
  tree_edge edge(int n1, int n2) const;
  tree_edge edge(int b) const;

  double branch_length(int b) const;
  void set_branch_length(int b, double l);

  void read_tree(const Tree&);

  void begin_modify_topology();
  void reconnect_branch(int s1, int t1, int t2);
  void end_modify_topology();

  constexpr TreeInterface(const Parameters* p):P(p) {};
};

double tree_length(const TreeInterface& t);

std::vector<int> branches_from_leaves(const TreeInterface& t);

std::vector<boost::dynamic_bitset<>> get_partitions(const TreeInterface& t);

unsigned topology_distance(const TreeInterface& T1, const TreeInterface& T2);

/// Return a Newick string representation of the tree 'T' with names 'names' and branch lengths L.
std::string write(const TreeInterface& T, const std::vector<double>& L, const std::vector<std::string>& names);
/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, const std::vector<std::string>& names, bool print_lengths=true); 

void check_tree(const Tree&, const TreeInterface&);
#endif
