#ifndef ETREE_H
#define ETREE_H

#include <cassert>
#include <vector>
#include <valarray>
/***************** struct tree ********************/

struct Branch;

//FIXME - I should include the name of the branch and its length ...
//          - I could still keep the node-view and branch-view classes

//FIXME - I have the coordinate system - should I write the iterator classes?
//        Can I force a root somewhere, now?
//        I need to do bottom up, as well as top down... 

struct node {
  node* parent;
  node* left;
  node* right;

  Branch* parent_branch;

  int name;
  int order;

  node():parent(0),left(0),right(0),parent_branch(0),name(-1),order(-1) {}
};

struct Branch {
  node* parent;
  node* child;
  double length;

  int name;

  Branch(): parent(0),child(0),length(0.0),name(-1) {}
};

// Split these out into
//  1. Just a Tree class (tree.H)
//  2. A tree class w/ Branches
//  3. A tree class w/ Branches and named leaves


// All internal nodes should be number higher than all leaves

/* These trees are going to be rooted.  
 *   o But we'll ignore the root I guess.
 *   o The root should have 2 neighbors
 * How do you store this?  Its a binary tree, but with labelled edges...
 */


// shouldn't we be able to have a 'copy(const tree&)' non-member function?

// TV1 = TV2 means TV1.root = TV2.root

class TreeView {
protected:
  node* root;

  static void destroy_tree(node*);
  static void copy_tree(node*);
public:
  void destroy() {destroy_tree(root);}
  static void exchange_cousins(node*,node*);

  TreeView copy() const;

  operator node*() {return root;}

  TreeView(node* n):root(n) {}
  ~TreeView() {}
};

class const_branchview;

class nodeview {
  const node* n;
public:
  bool leaf() const {return not n->left and not n->right;}

  bool has_left() const {return bool(n->left);}
  bool has_right() const {return bool(n->right);}
  
  int name() const {return n->name;}
  int order() const {return n->order;}

  nodeview left() const {assert(has_left());return n->left;}
  nodeview right() const {assert(has_right());return n->right;}
  nodeview parent() const {
    node* p = n->parent;
    if (not p->parent) {
      if (p->left != n)
	p = p->left;
      else
	p = p->right;
    }
    return nodeview(p);
  }

  operator int() {return name();}
  nodeview(const node* n_):n(n_) {}
};

class branchview {
  Branch* b;
public:
  nodeview parent() const {
    if (b->parent->parent)
      return b->parent;
    else
      return b->parent->right;
  }
  nodeview child() const { return b->child;}

  int& name() {return b->name;}
  const int& name() const {return b->name;}

  double& length() {return b->length;}
  const double& length() const {return b->length;}

  operator int() {return name();}
  branchview(Branch* b_):b(b_) {}
};

class const_branchview {
  const Branch* b;
public:
  nodeview parent() const {
    if (b->parent->parent)
      return b->parent;
    else
      return b->parent->right;
  }
  nodeview child() const {return b->child;}

  int name() const {return b->name;}
  double length() const {return b->length;}

  operator int() {return name();}
  const_branchview(const Branch* b_):b(b_) {}
};

class tree {
  int n_leaves_;

  std::vector<node*> order;

  std::vector<Branch*> branches_;
protected:
  std::vector<node*> names;
  node* root;

  void add_left(node*,const tree&);
  void add_right(node*,const tree&);
  virtual void remove_subtree(node*);

  std::vector< std::valarray<bool> > ancestors;

  void compute_ancestors();

  void check_structure() const;

  //auxiliary
  void do_swap(node*,node*);
  //re-compute everything from the root, put tree in sane state
  void reanalyze();
  //re-calculate the order of the nodes
  void reorder();

  void add_root(); 

  double distance2(int,int) const;
public:
  void unroot() {
    if (root->right)
      root->right->parent_branch->length=0.0;
  }

  void remove_subtree(int i);

  branchview branch(int i) {
    assert(i<branches_.size()-1);
    assert(branches_[i]->parent == branches_[i]->child->parent);
    return branches_[i];
  }

  const_branchview branch(int i) const {
    assert(i<branches_.size()-1);
    assert(branches_[i]->parent == branches_[i]->child->parent);
    return branches_[i];
  }
  
  const nodeview operator[](int i) const { return nodeview(names[i]);}
  //  node& operator[](int i) { return *names[i];}

  int get_nth(int i) const {return order[i]->name;}

  std::vector<int> neighbors(int node) const;
  bool connected(int node1,int node2) const;
  int find_branch(int node1,int node2) const;

  int leaves() const {return n_leaves_;}
  int branches() const {return branches_.size()-1;}
  int leafbranches() const {return (n_leaves_==2)?1:n_leaves_;}
  int num_nodes() const {return order.size();}

  int n_nodes() const {return num_nodes();}
  int n_leaves() const {return leaves();}
  int n_leafbranches() const {return leafbranches();}
  int n_branches() const {return branches();}

  bool leaf_node(int n) const {return n < n_leaves();}
  bool internal_node(int n) const {return not leaf_node(n);}

  branchview branch_up(int);
  const_branchview branch_up(int) const;

  // These change don't change the tree itself, how it is stored/referenced
  void swap_children(int n);
  std::vector<int> standardize(bool=true);
  std::vector<int> standardize(const std::vector<int>&,bool=true);

  /// The common ancestor of nodes i and j
  int common_ancestor(int i,int j) const;
  /// The distance between nodes i and j
  double distance(int i,int j) const;


  // n1 is an ancestor of n2, n2 is a descendant of n1
  bool ancestor(int n1, int n2) const {return ancestors[n1][n2];}
  std::valarray<bool> partition(int n1,int n2) const;

  TreeView copy() const {return TreeView(root).copy();}
  void exchange(int,int);
  void SPR(int,int,int);
  void reroot(int);
  
  tree& operator=(const tree&);

  tree():root(0) {n_leaves_=0;}
  tree(const tree& t1):root(0) {operator=(t1);}

  tree(const tree& t1,const tree& t2);
  tree(const tree& t1,double b1,const tree& t2,double b2);
  virtual ~tree() {TreeView(root).destroy();}
};

inline bool leaf(const node& n) {
  return (not n.left and not n.right);
}

inline tree operator+(const tree& t1,const tree& t2) {
  tree t3(t1,t2);

  return t3;
}

/// Are nodes node1 and node2 connected? (Unrooted topology)
inline bool tree::connected(int node1,int node2) const {
  assert(0 <= node1 and node1 < names.size()-1);
  assert(0 <= node2 and node2 < names.size()-1);

  node* n1 = names[node1];
  node* n2 = names[node2];

  if (n1->parent == n2)
    return true;
  if (n2->parent == n1)
    return true;
    
  if (n1->parent == root and n2->parent == root)
    return true;

  return false;
}

#endif
