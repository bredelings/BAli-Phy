#ifndef ETREE_H
#define ETREE_H

#include <cassert>
#include <vector>
#include <valarray>
#include <list>
#include "tree-branchnode.H"

//---------------------------------- TreeView --------------------------//

class TreeView {
protected:
  BranchNode* root;

  static void destroy_tree(BranchNode*);
  static BranchNode* copy_node(BranchNode*);
  static BranchNode* copy_tree(BranchNode*);

public:
  /// Exchange the subtrees pointed to by b1 and b2
  static void exchange_subtrees(BranchNode* b1,BranchNode* b2);

  /// Remove subtree 'b', and point 'b' to it
  static BranchNode* create_node_on_branch(BranchNode* b,int new_branchname);
  static int  remove_node_from_branch(BranchNode* n1);

  static void merge_nodes(BranchNode* n1,BranchNode* n2);
  static BranchNode* unlink_subtree(BranchNode* b);

  TreeView copy() const;
  void destroy() {destroy_tree(root);}

  operator BranchNode*() {return root;}

  TreeView(BranchNode* b):root(b) {}
  ~TreeView() {}
};


//------------------------------------ Tree -----------------------------//

class Tree {
protected:
  int n_leaves_;

  std::vector< std::valarray<bool> > cached_partitions;
  std::vector< std::vector<int> > cached_partition_sets;

  friend class branchview;
  friend class const_branchview;

  std::vector<BranchNode*> nodes_;

  std::vector<BranchNode*> branches_;

  void compute_partitions();

  /// re-compute everything given leaf names
  virtual void reanalyze(BranchNode*);

  virtual void recompute(BranchNode*,bool=true);

  virtual void check_structure() const;

  /// Get the Branch* to the branch connecting node1 and node2
  BranchNode* find_branch_pointer(int node1,int node2) const {
    assert(0 <= node1 and node1 < nodes_.size());
    assert(0 <= node2 and node2 < nodes_.size());

    BranchNode* n1 = nodes_[node1];

    while(n1->out->node != node2) {
      n1=n1->next;
      if (n1 == nodes_[node1]) break;
    }

    if (n1->out->node == node2)
      return n1;
    else
      return NULL;
  }

public:

  TreeView copy(int n) const {
    assert(0 <= n and n < nodes_.size());

    return TreeView(nodes_[n]).copy();
  }

  TreeView copy() const {
    if (nodes_.size()) 
      return copy(0);
    else
      return 0;
  }

  int n_nodes() const {return nodes_.size();}
  int n_leaves() const {return n_leaves_;}
  int n_leafbranches() const {return (n_leaves()==2)?1:n_leaves();}
  int n_branches() const {return branches_.size()/2;}

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

  /// Get the directed branch from 'node1' -> 'node2'
  branchview directed_branch(int n1,int n2) const {
    return find_branch_pointer(n1,n2);
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
  
        nodeview operator[](int i)       {return nodes_[i];}
  const_nodeview operator[](int i) const {return nodes_[i];}

  bool is_connected(int node1,int node2) const {
    return find_branch_pointer(node1,node2) != NULL;
  }

  // walk path from i->j instead...
  /// The distance between nodes i and j
  double distance(int i,int j) const;
  int edges_distance(int i,int j) const;

  // n1 is an ancestor of n2, n2 is a descendant of n1
  std::valarray<bool> partition(int b) const {
    return cached_partitions[b];
  }

  std::valarray<bool> partition(int n1,int n2) const {
    return partition(directed_branch(n1,n2));
  }

  // n1 is an ancestor of n2, n2 is a descendant of n1
  std::vector<int> leaf_partition_set(int b) const {
    return cached_partition_sets[b];
  }

  std::vector<int> leaf_partition_set(int n1,int n2) const {
    return leaf_partition_set(directed_branch(n1,n2));
  }

  // FIXME - not really implemented yet
  virtual std::vector<int> standardize();
  virtual std::vector<int> standardize(const std::vector<int>&);

  /// is 'n' contained in the subtree delineated by 'b' 
  bool subtree_contains(int b,int n) const {return partition(b)[n];}

  /// switch subtrees 'b1' and 'b2'
  void exchange_subtrees(int b1,int b2);

  /// Prune subtree 'b1' and regraft into branch 'b2'
  void SPR(int b1,int b2);

  /// Add a node onto a branch 'b', splitting it into two sub-neighbors
  nodeview create_node_on_branch(int b);

  /// Remove a node 'n' with exactly two neighbors, merging two branches
  void remove_node_from_branch(int n);

  /// remove a subtree
  virtual nodeview prune_subtree(int b);

  /// merge node 'tn' from subtree 'T' to node 'n'
  virtual void merge_tree(int n,const Tree& T,int tn);

  /// create a node
  virtual void add_first_node();

  /// add a node, linking to node 'n'
  nodeview add_node(int n);

  Tree& operator=(const Tree& T); 

  Tree():n_leaves_(0) {}
  Tree(const Tree& T);

  virtual ~Tree();
};

//---------------------------- RootedTree -----------------------//

// In rooted_tree, we will assume that nodes_[i] gives the TOP node in a ring

class RootedTree: public Tree {
protected:
  BranchNode* root_;

  void recompute(BranchNode*,bool=true);

  void check_structure() const;
public:
  const_nodeview root() const {return root_;}
        nodeview root()       {return root_;}

  /// More the root from the current node to node 'n'
  void reroot(int n);

  std::vector<int> standardize() {return Tree::standardize();}
  std::vector<int> standardize(const std::vector<int>&);

  /// The common ancestor of nodes i and j
  int common_ancestor(int i,int j) const;

  /// n1 is an ancestor of n2, n2 is a descendant of n1
  bool ancestor(int n1, int n2) const;

  /// remove a subtree
  nodeview prune_subtree(int b);

  void add_first_node();

  RootedTree& operator=(const RootedTree&);

  RootedTree():root_(NULL) {}
  RootedTree(const Tree&,int r);
  RootedTree(const RootedTree&);

  RootedTree(const RootedTree& t1,const RootedTree& t2);

  ~RootedTree() {}
};


RootedTree add_root(Tree T,int b);

Tree remove_root(const RootedTree& RT);

inline RootedTree operator+(const RootedTree& t1,const RootedTree& t2) {
  RootedTree t3(t1,t2);

  return t3;
}

void get_branches_after(std::vector<const_branchview>& branch_list);
std::vector<const_branchview> branches_after(const Tree& T,int b);
std::vector<const_branchview> branches_from_node(const Tree& T,int n);
std::vector<const_branchview> branches_toward_node(const Tree& T,int n);


#endif
