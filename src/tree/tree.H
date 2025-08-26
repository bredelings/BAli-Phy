/*
  Copyright (C) 2004-2009 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

/**
 * @file tree.H
 *
 * @brief This file defines an unrooted Tree class and a (derived) rooted Tree class.
 *        This structure is based on the unrooted tree structure in Felsenstein's
 *        Phylogenetics book, but may not be exactly identical.
 */
#ifndef ETREE_H
#define ETREE_H

#include "util/assert.hh"
#include <vector>
#include <boost/dynamic_bitset.hpp>
#include "tree-branchnode.H"

// TODO.
// 1. Add node and branch attributes to tree objects.
//    - Each property will have an integer index, and (optionally) a string name, stored in the tree structure.
//      and not on the nodes.  This gives us to ability to quickly look up the property value, given that we can
//      quickly look up the node.
//    - The properties will also be associate with the nodes even when the node names (integer indices) change.
//    - It also allows us to add or remove nodes/branches -> new nodes have no properties, initially.
//    - If a node is missing a property, then the any value will be empty.
//    - We will create an extra node object, and an extra branch object to store the properties.
//      + Each BranchNode will point to its Node object, and its Branch object.
//    - When printing a tree, we can query each property (each value) about whether it is a string.
//    - We will only print string properties.
//    - We will select a specific (string) property to use as the leaf label.
//      + How do we avoid printing that property ALSO as a property?
// 2. Add the ability to read and write string attributes in NEWICK format.
//    - Extend the parser to handle a character at a time, instead of a word at a time.
//    - Allow recognizing [] comments, and ignoring them.
//    - In certain places, recognize and keep [&key=value] comments.
// 3. Remove class SequenceTree
//    - The leaf labels are just a special attribute
//    - Perhaps the branch lengths are ALSO just a special attribute?
// 4. Replace 

// Future goal: ability to save and reload trees exactly, for checkpoint-restore.
// Future goal: remove branch indices from tree structure, refer to branches via (node,node) pairs
//              instead of single integers?
// Future goal: stop forcing a particular order for nodes.
//              - instead allow accessors such as leaf_node(i), internal_node(i), leaf_branch(i), internal_branch(i), etc.

// Problem areas:
// - standardize: Names leaves, and assumes that rest of tree nodes get named based on leaf node names
//                Also assumes leaf nodes are named 0..n-1
// - branch_partition: Assumes leaf nodes are named 0..n-1
// - prune_leaves: assumes need to shift leaf nodes to keep them 0..n-1
// - add_leaf_node: assumed need to shift leaf nodes to keep them 0..n-1
// - extends_map( )
// - remap_T_leaf_indices( ): assumes leaf nodes numbered 0..n-1
// - alignment constraints are indexed by leaf index numbered 0...n-1

// Solution?
// - For now, just assume that the leaf nodes are labelled by their order in leaf_nodes.
//   + I should probably have a separate function to compute leaf-labelled branch partitions.
//
// - Well, when comparing two trees this is still problematic.
//   + We want to be able to say that two leaf-labelled trees are equivalent.
//   + Solution: Relabel the entire tree so that leaf labels are in 0..n-1.
//               We don't need to do this always, just when doing 'impose_leaf_labels( )'.
//               This would ensure that leaf_node(i) returns the ith node in the labelling.
//
//               We could optionally treat leaf labels as an extra attribute as well.
//               If so, we could make leaf_node(i) to be based on the leaf labels...
//     
//               When reading in trees with numeric labels, we could impose the leaf labels
//               after the entire tree is read in.
//
//               Hmm.. for the interface, we could just say that impose_leaf_labels( ) results in
//               leaf_node(i) returning the i-th labelled node.
//   + Basically, don't constantly require that leaf labels are 0..n-1, just arrange for that
//     to be true for the time interval where we are going to use it.

//---------------------------------- TreeView --------------------------//

/// This class wraps a BranchNode* and treats it as a reference to a tree.
class TreeView {
protected:
    /// A BranchNode in the tree
    BranchNode* root;

public:
    /// Deallocate the tree pointed too.
    static void destroy_tree(BranchNode*);
    /// Create a duplicate node ring with the same node numbers, branch numbers, and branch lengths.
    static BranchNode* copy_node(const BranchNode*);
    /// Create a duplicate tree.
    static BranchNode* copy_tree(const BranchNode*);

    /// Exchange the subtrees pointed to by b1 and b2
    static void exchange_subtrees(BranchNode* b1,BranchNode* b2);

    /// Remove subtree 'b', and point 'b' to it
    static BranchNode* create_node_on_branch(BranchNode* b,int new_branchname);

    /// Remove a node 'n' with exactly two neighbors, merging its two adjacent branches
    static int  remove_node_from_branch(BranchNode* n1, int branch_to_move = -1);

    /// Merge the node rings into a single node ring.
    static void merge_nodes(BranchNode* n1,BranchNode* n2);

    /// Cut the branch b and remove its BranchNode from the target of b.
    static BranchNode* unlink_subtree(BranchNode* b);

    /// Create a duplicate tree.
    TreeView copy() const;
    /// Deallocate the current tree.
    void destroy() {destroy_tree(root);}

    /// Access the BranchNode we are wrapping.
    operator BranchNode*() {return root;}

    /// Construct a TreeView wrapper from a BranchNode
    TreeView(BranchNode* b):root(b) {}
    ~TreeView() {}
};

//------------------------------------ Tree -----------------------------//

struct tree_dir_edge
{
    int node1;
    int node2;

    /// Check if the other branch is the same regardless of orientation
    bool operator==(const tree_dir_edge& b2) const = default;

    /// Order branches (for sorting)
    std::strong_ordering operator<=>(const tree_dir_edge& b2) const = default;

    tree_dir_edge reverse() const;

    tree_dir_edge():node1(-1),node2(-1) { }
    tree_dir_edge(int s, int t):node1(s), node2(t) { }
    tree_dir_edge(const BranchNode* BN):node1(BN->node_attributes->name), node2(BN->out->node_attributes->name) { }
    // The "explicit" is in order to avoid ambiguous conversions between 'const_branchview -> int' or 'const_branchview -> tree_dir_edge'
    explicit tree_dir_edge(const const_branchview& b):node1(b.source()), node2(b.target()) { }
};

inline tree_dir_edge tree_dir_edge::reverse() const
{
    return tree_dir_edge(node2, node1);
}

struct tree_edge
{
    int node1;
    int node2;

    /// Check if the other branch is the same and has the same orientation
    bool same_orientation(const tree_edge& b2) const
	{
	    if (node1 == b2.node1 and node2 == b2.node2) return true;
	    return false;
	}

    /// Check if the other branch is the same but has the opposite orientation
    bool opposite_orientation(const tree_edge& b2) const
	{
	    if (node1 == b2.node2 and node2 == b2.node1) return true;
	    return false;
	}

    /// Check if the other branch is the same regardless of orientation
    bool operator==(const tree_edge& b2) const
	{
	    if (same_orientation(b2)) return true;
	    if (opposite_orientation(b2)) return true;
	    return false;
	}

    /// Order branches (for sorting)
    bool operator<(const tree_edge& b2) const
	{
	    int b1n1 = std::min(node1,node2);
	    int b1n2 = std::max(node1,node2);

	    int b2n1 = std::min(b2.node1, b2.node2);
	    int b2n2 = std::max(b2.node1, b2.node2);

	    if (b1n1 < b2n1) return true;
	    if (b1n1 > b2n1) return false;
	    if (b1n2 < b2n2) return true;
	    if (b1n2 > b2n2) return false;

	    return false;
	}

    tree_edge& operator=(const tree_edge&) = default;
    
    tree_edge reverse() const;

    tree_edge():node1(-1),node2(-1) { }
    tree_edge(int s, int t):node1(s), node2(t) { }
    tree_edge(const BranchNode* BN):node1(BN->node_attributes->name), node2(BN->out->node_attributes->name) { }
    // The "explicit" is in order to avoid ambiguous conversions between 'const_branchview -> int' or 'const_branchview -> tree_edge'
    tree_edge(const tree_edge& e) = default;
    tree_edge(const tree_dir_edge& e):node1(e.node1),node2(e.node2) {}
    explicit tree_edge(const const_branchview& b):node1(b.source()), node2(b.target()) { }
};

inline tree_edge tree_edge::reverse() const
{
    return tree_edge(node2, node1);
}

inline bool operator<(const const_branchview& b1, const const_branchview& b2)
{
    tree_edge e1(b1);
    tree_edge e2(b2);
    return (e1<e2);
}

/**
 * @brief An unrooted tree class.
 *
 *        This structure is based on the unrooted tree structure in Felsenstein's
 *        Phylogenetics book, but may not be exactly identical.
 *
 *        Each node of degree d consists of a circular list (a node ring) of d 
 *        BranchNode's.  Each BranchNode in the ring corresponds to one of the
 *        outgoing branches, and has an extra pointer that points to a BranchNode
 *        in the node right of the target node.  Thus, each branch has two
 *        BranchNode's - one at each endpoint.  A BranchNode may therefore indicate
 *        either the node (ring) of which it is a part, or it may indiate
 *        a directed branch pointing from that node, to the node (ring) containing the
 *        BranchNode to which it points.
 */
class Tree 
{
    /// Are the cached_partitions valid?
    mutable bool caches_valid;

    /// Cached partitions.
    mutable std::vector< boost::dynamic_bitset<> > cached_partitions;

protected:
    /// The number of leaf nodes
    int n_leaves_;

    // Which index into the node attribute table is the node label
    std::optional<int> node_label_index;
    // Which index into the undirected branch attribute table is the branch length
    std::optional<int> branch_length_index;

public:
    /// The number of node attributes
    std::vector<std::string> node_attribute_names;
    /// The number of directed branch attributes
    std::vector<std::string> undirected_branch_attribute_names;

protected:
    /// The number of undirected attributes
    std::vector<std::string> directed_branch_attribute_names;

    friend class branchview;
    friend class const_branchview;
    friend std::vector<int> extends_map(const Tree&,const Tree&);

    /// an index to one BranchNode for each node
    std::vector<BranchNode*> nodes_;

    mutable std::optional<std::vector<int> > leaf_nodes_;

    mutable std::optional<std::vector<int> > internal_nodes_;

    /// an index to one BranchNode for each node
    std::vector<BranchNode*> branches_;

    mutable std::optional<std::vector<tree_edge> > leaf_branches_;

    mutable std::optional<std::vector<tree_edge> > internal_branches_;


    /// re-compute cached_partitions
    void compute_partitions() const;
  
    /// re-compute partitions if necessary
    void prepare_partitions() const {
	if (not caches_valid)
	    compute_partitions();
    }

    void set_equivalent_node_pointers(const std::vector<BranchNode*>&);

public:
    /// re-compute all caches
    virtual void recompute(BranchNode*,bool=true);

    void inc_node_pointers();

protected:
    /// check caches, linked lists, and naming conventions
    virtual void check_structure() const;

    /// Get the Branch* to the branch connecting node1 and node2
    BranchNode* find_branch_pointer(int node1,int node2) const {
	assert(0 <= node1 and node1 < nodes_.size());
	assert(0 <= node2 and node2 < nodes_.size());

	BranchNode* n1 = nodes_[node1];

	while(n1->out->node_attributes->name != node2) {
	    n1=n1->next;
	    if (n1 == nodes_[node1]) break;
	}

	if (n1->out->node_attributes->name == node2)
	    return n1;
	else
	    return NULL;
    }

public:

    /// re-compute all names and caches given leaf node names
    virtual void reanalyze(BranchNode*);

    /// Create a copy of the tree, and return a pointer to node n in the copy
    TreeView copy(int n) const {
	assert(0 <= n and n < nodes_.size());

	return TreeView(nodes_[n]).copy();
    }

    /// Create a copy of the tree, and return a pointer
    TreeView copy() const {
	if (nodes_.size()) 
	    return copy(0);
	else
	    return 0;
    }

    /// the number of nodes
    int n_nodes() const {return nodes_.size();}

    /// the number of leaf nodes
    int n_leaves() const {return n_leaves_;}
    const std::vector<int>& leaf_nodes() const;
    int leaf_node(int i) const {return leaf_nodes()[i];}

    const std::vector<int>& internal_nodes() const;
    int internal_node(int i) const {return internal_nodes()[i];}

    /// the number of branches
    int n_branches() const {return branches_.size()/2;}

    const std::vector<tree_edge>& leaf_branches() const;
    tree_edge leaf_branch(int i) const {return leaf_branches()[i];}

    /// the number of leaf branches
    int n_leafbranches() const;

    const std::vector<tree_edge>& internal_branches() const;
    tree_edge internal_branch(int i) const {return internal_branches()[i];}

    int n_node_attributes() const {return node_attribute_names.size();}

    void set_n_node_attributes(int);

    bool node_attribute_has_name(int i) const {return not node_attribute_names[i].empty();}

    void node_attribute_set_name(int i, const std::string& s) {node_attribute_names[i] = s;}

    std::optional<int> maybe_find_node_attribute_index_by_name(const std::string& s) const;

    int find_node_attribute_index_by_name(const std::string& s) const;

    int add_node_attribute();

    int add_node_attribute(const std::string&);

    const std::string& node_attribute_name(int i) const {return node_attribute_names[i];}

    int add_undirected_branch_attribute();

    int add_undirected_branch_attribute(const std::string&);

    int n_undirected_branch_attributes() const {return undirected_branch_attribute_names.size();}

    void set_n_undirected_branch_attributes(int);

    bool undirected_branch_attribute_has_name(int i) const {return not undirected_branch_attribute_names[i].empty();}

    void undirected_branch_attribute_set_name(int i, const std::string& s) {undirected_branch_attribute_names[i] = s;}

    std::optional<int> maybe_find_undirected_branch_attribute_index_by_name(const std::string& s) const;

    int find_undirected_branch_attribute_index_by_name(const std::string& s) const;

    const std::string& undirected_branch_attribute_name(int i) const {return undirected_branch_attribute_names[i];}


    int n_directed_branch_attributes() const {return directed_branch_attribute_names.size();}

    void set_n_directed_branch_attributes(int);

    bool directed_branch_attribute_has_name(int i) const {return not directed_branch_attribute_names[i].empty();}

    void directed_branch_attribute_set_name(int i, const std::string& s) {directed_branch_attribute_names[i] = s;}

    std::optional<int> maybe_find_directed_branch_attribute_index_by_name(const std::string& s) const;

    int find_directed_branch_attribute_index_by_name(const std::string& s) const;

    const std::string& directed_branch_attribute_name(int i) const {return directed_branch_attribute_names[i];}


    /// Get the branch with name 'b'
    const_branchview branch(int b) const {
	assert(0 <= b and b<n_branches());
	return branches_[b];
    }

    /// Get the branch with name 'b'
    branchview branch(int b) {
	assert(0 <= b and b<n_branches());
	return branches_[b];
    }

    /// Get the directed branch w/ name 'b'
    const_branchview directed_branch(int b) const {
	assert(0 <= b and b < 2*n_branches());
	return branches_[b];
    };

    /// Get the directed branch w/ name 'b'
    branchview directed_branch(int b) {
	assert(0 <= b and b < 2*n_branches());
	return branches_[b];
    };

    /// Get the directed branch from 'node1' -> 'node2'
    const_branchview directed_branch(int n1,int n2) const {
	return find_branch_pointer(n1,n2);
    }

    /// Get the directed branch from 'node1' -> 'node2'
    branchview directed_branch(int n1,int n2) {
	return find_branch_pointer(n1,n2);
    }

    /// Get the directed branch from 'node1' -> 'node2'
    const_branchview directed_branch(const tree_edge& e) const {
	return directed_branch(e.node1, e.node2);
    }

    /// Get the directed branch from 'node1' -> 'node2'
    branchview directed_branch(const tree_edge& e) {
	return directed_branch(e.node1, e.node2);
    }

    /// Get the branch connecting n1 <-> n2
    branchview branch(int n1,int n2) {
	branchview bv = find_branch_pointer(n1,n2);

	bv = bv.undirected();

	return bv;
    }
  
    /// Get the branch connecting n1 <-> n2
    const_branchview branch(int n1,int n2) const {
	const_branchview bv = find_branch_pointer(n1,n2);

	bv = bv.undirected();

	return bv;
    }
  
    const_branchview branch(const tree_edge& e) const {
	return branch(e.node1, e.node2);
    }

    branchview branch(const tree_edge& e) {
	return branch(e.node1, e.node2);
    }

    /// Get a reference to the 'i'-th node
    nodeview node(int i)       {return nodes_[i];}
    /// Get a reference to the 'i'-th node
    const_nodeview node(int i) const {return nodes_[i];}

    /// Precidate: are node1 and node2 connected by a branch?
    bool is_connected(int node1,int node2) const {
	return find_branch_pointer(node1,node2) != NULL;
    }

    /// The weighted distance between nodes i and j
    double distance(int i,int j) const;

    /// The unweighted distance between nodes i and j
    int edges_distance(int i,int j) const;

    /// A bitmask of nodes in front of directed branch b
    const boost::dynamic_bitset<>& partition(int b) const {
	prepare_partitions();
	return cached_partitions[b];
    }

    /// A bitmask of nodes in front of directed branch n1 -> n2
    const boost::dynamic_bitset<>& partition(int n1,int n2) const {
	return partition(directed_branch(n1,n2));
    }

    /// Construct standard names for leaves and branches
    virtual std::vector<int> standardize();

    /// Permute leaf nodes and then construct standard names for leaves and branches
    virtual std::vector<int> standardize(const std::vector<int>&);

    /// Is 'n' contained in the subtree delineated by 'b'?
    bool subtree_contains(int b,int n) const {return partition(b)[n];}

    /// Is 'b2' contained in the subtree delineated by 'b1'?
    bool subtree_contains_branch(int b1,int b2) const 
	{
	    const_branchview bv2 = directed_branch(b2);
	    return subtree_contains(b1,bv2.source()) and subtree_contains(b1,bv2.target());
	}


    /// Find and split a multifurcating node to create this partition
    int induce_partition(const boost::dynamic_bitset<>& partition);


    /// Add a node onto a branch 'b', splitting it into two sub-neighbors
    virtual nodeview create_node_on_branch(int b);

    /// Remove a node 'n' with exactly two neighbors, merging its two adjacent branches
    virtual void remove_node_from_branch(int n);

    /// remove leaves
    virtual std::vector<int> prune_leaf(int n);
    virtual std::vector<int> prune_leaves(const std::vector<int>&);

    /// merge node 'tn' from subtree 'T' to node 'n'
    virtual void merge_tree(int n,const Tree& T,int tn);

    /// create a node
    virtual void add_first_node();

    /// add a degree 0 , linking to node 'n'.  However, the index will not be a leaf index
    virtual nodeview add_leaf_node(int n);

    void reconnect_branch(int source, int target, int new_target);

    void reconnect_branch(const tree_edge& e, int new_target);

    /// Create an identical tree that does not share memory with the original
    Tree& operator=(const Tree& T); 

    int parse_(const std::string& s, std::function<void(BranchNode*)> assign_names);
    /// Parse and load the Newick format string 's', discovering and reporting leaf names in 'names'
    virtual int parse_and_discover_names(const std::string& s);
    /// Parse and load the Newick format string 's', where node names are given in 'names', or numerical starting at 1
    virtual int parse_with_names_or_numbers(const std::string& s, const std::vector<std::string>& names, bool allow_numbers=true);
    /// Parse and load the Newick format string 's', where node names are given in 'names'
    int parse_with_names(const std::string& s, const std::vector<std::string>& names);

    bool is_leaf_node(int n) const;
    int source(int b) const;
    int target(int b) const;
    int reverse(int b) const;
    std::vector<int> neighbors(int n) const;
    std::vector<int> all_branches_toward_node(int n) const;
    std::vector<int> branches_before(int b) const;
    std::vector<int> branches_after(int b) const;

    /// Create an empty tree
    Tree();
    /// Create a tree by taking over the tree structure containing BranchNode* n.
    explicit Tree(BranchNode* n);
    /// Create a new tree that is identical to T but does not share memory with it. 
    Tree(const Tree& T);

    virtual ~Tree();
};

/// Prune subtree 'b1' and regraft into branch 'b2'
int SPR(Tree& T,int b1,int b2, int branch_to_move = -1);
/// Swap the branches b1 and b2, along with the subtrees they point to.
void exchange_subtrees(Tree&,int b1,int b2);

//---------------------------- RootedTree -----------------------//

// In rooted_tree, we will assume that nodes_[i] gives the TOP node in a ring

/// A rooted tree class, consisting of an unrooted tree and a pointer to the root node.
class RootedTree: virtual public Tree 
{
protected:
    /// A pointer to the root node of the tree.
    BranchNode* root_;

    void recompute(BranchNode*,bool=true);

    void check_structure() const;
public:
    /// Return the root node
    const_nodeview root() const {return root_;}
    /// Return the root node
    nodeview root()       {return root_;}

    /// More the root from the current node to node 'n'
    void reroot(int n);

    /// The common ancestor of nodes i and j
    int common_ancestor(int i,int j) const;

    /// is n2 a descendant of n1?
    bool ancestor(int n1, int n2) const;

    /// Remove a node 'n' with exactly two neighbors, merging two branches
    void remove_node_from_branch(int n);

    /// remove leaves
    virtual std::vector<int> prune_leaf(int);
    virtual std::vector<int> prune_leaves(const std::vector<int>&);

    void add_first_node();

    RootedTree& operator=(const RootedTree&);

    /// load this tree from the string s
    virtual int parse_and_discover_names(const std::string& s);
    virtual int parse_with_names_or_numbers(const std::string& s, const std::vector<std::string>& names, bool allow_numbers=true);

    /// Create an empty tree with a NULL root
    RootedTree():root_(NULL) {}

    /// Create a tree by taking over the tree structure containing BranchNode* n, and using n as the root.
    explicit RootedTree(const BranchNode*);

    /// Construct a rooted tree from an unrooted tree by marking node 'r' as the root.
    RootedTree(const Tree& T,int r);
    /// Create a new rooted tree that is identical to T but does not share memory with it. 
    RootedTree(const RootedTree&);

    /// Create a new rooted tree by joining the root nodes of 't1' and 't2'
    RootedTree(const RootedTree& t1,const RootedTree& t2);

    ~RootedTree() {}
};

/// Create a copy of T with a new root node on branch b
RootedTree add_root(Tree T,int b);

/// Create a new rooted tree by joining the root nodes of 't1' and 't2'
inline RootedTree operator+(const RootedTree& t1,const RootedTree& t2) {
    RootedTree t3(t1,t2);

    return t3;
}

/// Return a Newick string representation of the rooted tree 'T' with names 'names', including branch lengths by default.
std::string write(const RootedTree& T, const std::vector<std::string>& names, bool print_lengths=true); 
/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const Tree& T, const std::vector<std::string>& names, bool print_lengths=true); 
/// Return a Newick string representation of the Rooted tree 'T', where node names are integers.
std::string write_no_names(const RootedTree& T, bool print_lengths=true); 
/// Return a Newick string representation of the tree 'T', where node names are integers.
std::string write_no_names(const Tree& T, bool print_lengths=true);

std::string escape_for_newick(const std::string& s);

/// Append branches before all of the branches passed in.
void get_branches_before(std::vector<const_branchview>& branch_list);
/// Append branches after all of the branches passed in.
void get_branches_after(std::vector<const_branchview>& branch_list);

/// list of all branches before b, with children before parents
std::vector<const_branchview> branches_before_inclusive(const Tree& T,int b);

/// list of all b and all branches after b, with children after parents
std::vector<const_branchview> branches_after_inclusive(const Tree& T,int b);

/// list of all branches after b, with children after parents
std::vector<const_branchview> branches_after(const Tree& T,int b);

/// list of all branches after n, with n as root and children after parents
std::vector<const_branchview> branches_from_node(const Tree& T,int n);

/// list of all branches before n, with n as root and children before parents
std::vector<const_branchview> branches_toward_node(const Tree& T,int n);

/// list of all branches, with children after parents
std::vector<const_branchview> branches_from_leaves(const Tree& T);

/// list of all outgoing branches, sorted by branch name
std::vector<const_branchview> sorted_neighbors(const_nodeview n);

/// list of all outgoing branches, sorted by branch name
std::vector<const_branchview> sorted_branches_after(const_branchview b);

std::vector<const_branchview> randomized_branches_after(const const_branchview& b);

std::vector<const_branchview> randomized_branches_out(const const_nodeview& b);

/// Construct a star tree with 'n' leaves.
Tree star_tree(int n);

/// Random resolve nodes on the tree.
void RandomTree(Tree&);

/// Return a bitmask of LEAF taxa (instead of all taxa) generated by branch b
boost::dynamic_bitset<> branch_partition(const Tree& T,int b);

/// Sum of the lengths of all branches
double tree_length(const Tree& T);

/// Sum of the lengths of all branches
double tree_diameter(const Tree& T);

/// Return the maximum distance from T.branch(b).target() to nodes after b.
int subtree_height(const Tree& T,int b);

/// What is the depth of the node in tree 'T'?
int node_depth(const Tree& T,int node);

/// Construct the list of leaves pointed to by each branch
std::vector<std::vector<int> > partition_sets(const Tree& T);

/// Predicate: return true if the Tree 't' has only nodes of degree 1 or 3.
bool is_Cayley(const Tree& T);

/// Predicate: return true if the Tree 't' has nodes of degree 2.
bool has_sub_branches(const Tree& T);

/// Predicate: return true if the Tree 't' has a polytomy
bool has_polytomy(const Tree& T);

/// Remove nodes of degree 2 and merge the resulting branches.
void remove_sub_branches(Tree& T);

bool same_topology_and_node_and_branch_numbers(const Tree& T1, const Tree& T2);

std::string escape_for_newick(const std::string& s);
std::string unescape_from_newick(const std::string& s);
#endif
