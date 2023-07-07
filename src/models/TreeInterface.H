#ifndef TREE_INTERFACE_H
#define TREE_INTERFACE_H

#include <boost/dynamic_bitset/dynamic_bitset.hpp>  // for dynamic_bitset
#include <string>                                   // for string
#include <vector>                                   // for vector
#include "tree/tree.H"                              // for tree_edge, Tree (...
#include "computation/param.H"

class context_ref;
class Parameters;

// We need to be able to handle
// * rooted trees
// * time trees
// * labelled trees

struct tree_constants: public Object
{
    // The tree
    expression_ref tree_exp;
    std::map<int,param> parameters_for_tree_node;
    std::map<int,std::tuple<param,param,param>> parameters_for_tree_branch;
    int n_leaves;

    // Root info
    std::optional<int> root_reg;
    std::optional<int> away_from_root_reg;

    // Node labels
    std::optional<int> labels_reg;

    // Branch lengths
    std::optional<int> branch_durations_reg;

    // Node times
    std::optional<int> node_times_reg;

    tree_constants* clone() const {return new tree_constants(*this);}

    tree_constants(context_ref&, int tree_reg);
    tree_constants(context_ref&, const expression_ref&);
};

class TreeInterface
{
    std::optional<int> branch_durations_reg() const;

    std::optional<int> root_reg() const;

    std::optional<int> node_times_reg() const;

    std::optional<int> away_from_root_reg() const;

    virtual const tree_constants& get_tree_constants() const = 0;
    virtual const context_ref& get_const_context() const = 0;
    virtual       context_ref& get_context() = 0;

public:
    int n_nodes() const;
    int n_branches() const;
    int n_leaves() const;
    int n_leaf_branches() const;
    std::vector<int> leaf_branches() const;
    std::vector<int> internal_branches() const;
    std::vector<int> leaf_nodes() const;
    std::vector<int> internal_nodes() const;

    std::vector<int> branches() const;
    std::vector<int> directed_branches() const;
    std::vector<int> nodes() const;
    std::map<int,std::string> labels() const;
    std::optional<std::string> label(int n) const;

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

    bool has_root() const;
    int root() const;
    bool away_from_root(int b) const;
    bool toward_root(int b) const;
    std::optional<int> parent_branch_for_node(int n) const;
    std::optional<int> parent_of_node(int n) const;
    std::vector<int> children_of_node(int n) const;

    std::map<int,double> branch_lengths() const;
    bool has_branch_lengths() const;
    double branch_length(int b) const;
    bool can_set_branch_length(int b) const;
    void set_branch_length(int b, double l);

    bool has_node_times() const;
    double node_time(int n) const;
    bool can_set_node_time(int n) const;
    void set_node_time(int n, double t);

    void begin_modify_topology();
    void reconnect_branch(int s1, int t1, int t2);
    void end_modify_topology();

    virtual ~TreeInterface() = default;
};



class ParametersTreeInterface: public TreeInterface
{
    const tree_constants& get_tree_constants() const;
    const context_ref& get_const_context() const;
          context_ref& get_context();

public:
    const Parameters* P;

    constexpr ParametersTreeInterface(const Parameters* p):P(p) {};
};

class ModifiablesTreeInterface: public TreeInterface
{
    const tree_constants& get_tree_constants() const;
    const context_ref& get_const_context() const;
          context_ref& get_context();

    const context_ref& C;
    const tree_constants* TC;
public:

    ModifiablesTreeInterface(context_ref& c, int tree_head_);
    ~ModifiablesTreeInterface();
};

double tree_length(const TreeInterface& t);

std::vector<int> branches_from_leaves(const TreeInterface& t);

std::vector<boost::dynamic_bitset<>> get_partitions(const TreeInterface& t);

unsigned topology_distance(const TreeInterface& T1, const TreeInterface& T2);

/// Return a Newick string representation of the tree 'T' with names 'names' and branch lengths L.
std::string write(const TreeInterface& T, const std::map<int,double>& L, const std::vector<std::string>& names);
/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, std::map<int,std::string>, bool print_lengths=true);

void NNI(TreeInterface& T, int b1, int b2);
#endif
