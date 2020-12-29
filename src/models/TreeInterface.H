#ifndef TREE_INTERFACE_H
#define TREE_INTERFACE_H

#include <boost/dynamic_bitset/dynamic_bitset.hpp>  // for dynamic_bitset
#include <string>                                   // for string
#include <vector>                                   // for vector
#include "tree/tree.H"                              // for tree_edge, Tree (...
class context;
class Parameters;
class tree_constants;

class TreeInterface
{
    void read_tree_node(const Tree&, int n);
    void end_modify_node(int n);

    virtual const tree_constants& get_tree_constants() const = 0;
    virtual const context& get_const_context() const = 0;
    virtual       context& get_context() = 0;

    virtual       std::vector<int>& affected_nodes() = 0;
    virtual const std::vector<int>& affected_nodes() const = 0;

    virtual       std::vector<std::vector<int>*>& branches_from_affected_nodes() = 0;
    virtual const std::vector<std::vector<int>*>& branches_from_affected_nodes() const = 0;

    std::vector<int>*& branches_from_affected_node(int n)
    {
        return branches_from_affected_nodes()[n];
    }
    std::vector<int>* branches_from_affected_node(int n) const
    {
        return branches_from_affected_nodes()[n];
    }
public:
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
    void append_all_branches_before(int b, std::vector<int>&) const;
    void append_all_branches_after(int b, std::vector<int>&) const;
    std::vector<int> branches_before(int b) const;
    std::vector<int> branches_after(int b) const;
    std::vector<int> all_branches_before_inclusive(int b) const;
    std::vector<int> all_branches_after_inclusive(int b) const;
    std::vector<int> all_branches_before(int b) const;
    std::vector<int> all_branches_after(int b) const;
    std::vector<int> all_branches_from_node(int n) const;
    std::vector<int> all_branches_toward_node(int n) const;

    int source(int b) const;
    int source_index(int b) const;
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
    bool can_set_branch_length(int b);
    void set_branch_length(int b, double l);

    void read_tree(const Tree&);

    void begin_modify_topology();
    void reconnect_branch(int s1, int t1, int t2);
    void end_modify_topology();

    virtual ~TreeInterface() = default;
};

class ParametersTreeInterface: public TreeInterface
{
    const tree_constants& get_tree_constants() const;
    const context& get_const_context() const;
          context& get_context();

          std::vector<int>& affected_nodes();
    const std::vector<int>& affected_nodes() const;

          std::vector<std::vector<int>*>& branches_from_affected_nodes();
    const std::vector<std::vector<int>*>& branches_from_affected_nodes() const;

public:
    const Parameters* P;

    constexpr ParametersTreeInterface(const Parameters* p):P(p) {};
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
