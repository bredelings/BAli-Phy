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

public:
  static void destroy_tree(BranchNode*);
  static BranchNode* copy_node(const BranchNode*);
  static BranchNode* copy_tree(const BranchNode*);

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

BranchNode* get_first_node();
BranchNode* add_node(BranchNode*);
void insert_after(BranchNode*,BranchNode*);
void connect_nodes(BranchNode*,BranchNode*);

//------------------------------------ Tree -----------------------------//

class Tree 
{
  mutable bool caches_valid;

  mutable std::vector< std::valarray<bool> > cached_partitions;

protected:
  /// The number of leaf nodes
  int n_leaves_;

  friend class branchview;
  friend class const_branchview;
  friend std::vector<int> extends_map(const Tree&,const Tree&);

  /// an index to one BranchNode for each node
  std::vector<BranchNode*> nodes_;

  /// an index to one BranchNode for each node
  std::vector<BranchNode*> branches_;

  /// re-compute cached_partitions
  void compute_partitions() const;

  /// re-compute partitions if necessary
  void prepare_partitions() const {
    if (not caches_valid)
      compute_partitions();
  }

public:
  /// re-compute all caches
  virtual void recompute(BranchNode*,bool=true);

protected:
  /// check caches, linked lists, and naming conventions
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

  /// the number of branches
  int n_branches() const {return branches_.size()/2;}

  /// the number of leaf branches
  int n_leafbranches() const {return (n_branches()==1)?1:n_leaves();}

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

  /// are node1 and node2 connected by a branch?
  bool is_connected(int node1,int node2) const {
    return find_branch_pointer(node1,node2) != NULL;
  }

  /// The weighted distance between nodes i and j
  double distance(int i,int j) const;

  /// The unweighted distance between nodes i and j
  int edges_distance(int i,int j) const;

  /// A bitmask of nodes in front of directed branch b
  const std::valarray<bool>& partition(int b) const {
    prepare_partitions();
    return cached_partitions[b];
  }

  /// A bitmask of nodes in front of directed branch n1 -> n2
  const std::valarray<bool>& partition(int n1,int n2) const {
    return partition(directed_branch(n1,n2));
  }

  /// construct standard names for leaves and branches
  virtual std::vector<int> standardize();

  /// permute leaf nodes and then construct standard names for leaves and branches
  virtual std::vector<int> standardize(const std::vector<int>&);

  /// is 'n' contained in the subtree delineated by 'b' 
  bool subtree_contains(int b,int n) const {return partition(b)[n];}

  /// switch subtrees 'b1' and 'b2'
  void exchange_subtrees(int b1,int b2);

  /// Prune subtree 'b1' and regraft into branch 'b2'
  void SPR(int b1,int b2);

  /// Find and split a multifurcating node to create this partition
  void induce_partition(const std::valarray<bool>& partition);


  /// Add a node onto a branch 'b', splitting it into two sub-neighbors
  nodeview create_node_on_branch(int b);

  /// Remove a node 'n' with exactly two neighbors, merging two branches
  virtual void remove_node_from_branch(int n);

  /// remove a subtree
  virtual nodeview prune_subtree(int b);

  /// remove leaves
  virtual std::vector<int> prune_leaves(const std::vector<int>&);

  /// merge node 'tn' from subtree 'T' to node 'n'
  virtual void merge_tree(int n,const Tree& T,int tn);

  /// create a node
  virtual void add_first_node();

  /// add a node, linking to node 'n'
  nodeview add_node(int n);

  Tree& operator=(const Tree& T); 

  Tree():n_leaves_(0) {}
  explicit Tree(const BranchNode*);
  Tree(const Tree& T);

  virtual ~Tree();
};

//---------------------------- RootedTree -----------------------//

// In rooted_tree, we will assume that nodes_[i] gives the TOP node in a ring

class RootedTree: virtual public Tree {
protected:
  BranchNode* root_;

  void recompute(BranchNode*,bool=true);

  void check_structure() const;
public:
  const_nodeview root() const {return root_;}
        nodeview root()       {return root_;}

  /// More the root from the current node to node 'n'
  void reroot(int n);

  /// The common ancestor of nodes i and j
  int common_ancestor(int i,int j) const;

  /// is n2 a descendant of n1?
  bool ancestor(int n1, int n2) const;

  /// Remove a node 'n' with exactly two neighbors, merging two branches
  void remove_node_from_branch(int n);

  /// remove a subtree
  nodeview prune_subtree(int b);

  /// remove leaves
  virtual std::vector<int> prune_leaves(const std::vector<int>&);

  void add_first_node();

  RootedTree& operator=(const RootedTree&);

  RootedTree():root_(NULL) {}
  explicit RootedTree(const BranchNode*);
  RootedTree(const Tree&,int r);
  RootedTree(const RootedTree&);

  RootedTree(const RootedTree& t1,const RootedTree& t2);

  ~RootedTree() {}
};

/// Create a copy of T with a new root node on branch b
RootedTree add_root(Tree T,int b);

inline RootedTree operator+(const RootedTree& t1,const RootedTree& t2) {
  RootedTree t3(t1,t2);

  return t3;
}

void get_branches_before(std::vector<const_branchview>& branch_list);
void get_branches_after(std::vector<const_branchview>& branch_list);

/// list of all branches before b, with children before parents
std::vector<const_branchview> branches_before(const Tree& T,int b);

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

/// Does the bitmask have any bits set?
bool empty(const std::valarray<bool>& v);

/// Are two bitmasks equal to each other?
bool equal(const std::valarray<bool>& v1,const std::valarray<bool>& v2);

/// Do the two bitmasks intersect?
bool intersect(const std::valarray<bool>& v1,const std::valarray<bool>& v2);

/// Does bitmask v1 imply bitmask v2?
bool is_subset(const std::valarray<bool>& v1,const std::valarray<bool>& v2);

/// How many of v's bits are true?
int n_elements(const std::valarray<bool>& v);

/// How many of v's bits are true?
int n_elements(const std::vector<bool>& v);

Tree star_tree(int n);

/// Construct a random tree with n leaf nodes
Tree RandomTree(int n,double=1.0);

/// Return a bitmask of LEAF taxa (instead of all taxa) generated by branch b
std::valarray<bool> branch_partition(const Tree& T,int b);

/// Sum of the lengths of all branches
double length(const Tree& T);

int subtree_height(const Tree& T,int b);

int node_depth(const Tree& T,int node);

std::vector<std::vector<int> > partition_sets(const Tree& T);

bool is_Cayley(const Tree& T);

bool has_sub_branches(const Tree& T);

bool has_polytomy(const Tree& T);

void remove_sub_branches(Tree& T);

#endif
