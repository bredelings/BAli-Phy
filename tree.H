#ifndef ETREE_H
#define ETREE_H

#include "mytypes.H"
#include "sequence.H"
#include <istream>
#include <vector>
#include <string>
/***************** struct tree ********************/

struct Branch;

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
  int n_leaves;

  vector<node*> names;
  vector<node*> order;

  vector<Branch*> branches_;

protected:
  node* root;

  void add_left(node*,const tree&);
  void add_right(node*,const tree&);

  vector< std::valarray<bool> > ancestors;

  void compute_ancestors();

  //auxiliary
  void do_swap(node*,node*);
  //re-compute everything from the root, put tree in sane state
  void reanalyze();
  //re-calculate the order of the nodes
  void reorder();

  void add_root(); 

public:
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

  int leaves() const {return n_leaves;}
  int branches() const {return branches_.size()-1;}
  int num_nodes() const {return order.size();}
  branchview branch_up(int);
  const_branchview branch_up(int) const;

  // These change don't change the tree itself, how it is stored/referenced
  void swap_children(int n);
  void standardize(const vector<int>&);

  bool ancestor(int n1, int n2) const {return ancestors[n1][n2];}
  std::valarray<bool> partition(int n1,int n2) const;

  TreeView copy() const {return TreeView(root).copy();}
  void exchange(int,int);
  
  tree& operator=(const tree&);

  tree():root(0) {n_leaves=0;}
  tree(const tree& t1):root(0) {operator=(t1);}

  tree(const tree& t1,const tree& t2);
  tree(const tree& t1,double b1,const tree& t2,double b2);
  ~tree() {TreeView(root).destroy();}
};

inline bool leaf(const node& n) {
  return (not n.left and not n.right);
}

inline tree operator+(const tree& t1,const tree& t2) {
  tree t3(t1,t2);

  return t3;
}


/****************** class TreeFunc *******************/

template<class RangeType>
class TreeFunc {
  const tree& t;

  vector<RangeType> values;
public:
  RangeType& operator()(int i) {return values[i];} 
  const RangeType& operator()(int i) const {return values[i];} 

  RangeType& operator()(const node& n) {return values[n.name];} 
  const RangeType& operator()(const node& n) const {return values[n.name];} 

  TreeFunc(const tree& t1): t(t1),values(t1.num_nodes()) {}
  TreeFunc(const tree& t1,RangeType value): t(t1),values(t1.num_nodes(),value) {}
};

TreeFunc<int> mark_tree(const vector<int>&,const tree&);


class SequenceTree: public tree {
  vector<string> sequences;
  string write(int n,bool) const;

public:
  const string& seq(int i) const {return sequences[i];}
  const vector<string>& get_sequences() const {return sequences;}

  void standardize(const vector<int>&);

  int index(const string&) const;

  string write(bool=true) const;

  void read(const string&);
  void read(std::istream&);
  void parse(const string&);

  SequenceTree() {}
  SequenceTree(const string& s);
  SequenceTree(const SequenceTree& T1, const SequenceTree& T2);
  SequenceTree(const SequenceTree& T1, double b1,const SequenceTree& T2,double b2);

  SequenceTree(std::istream&);   //load a tree from a file
};

inline SequenceTree operator+(const SequenceTree& t1,const SequenceTree& t2) {
  SequenceTree t3(t1,t2);

  return t3;
}

inline std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  return o<<T.write();
}

bool same_topology(const SequenceTree&,const SequenceTree&);

SequenceTree RandomTree(const vector<string>&,double=1.0);
/***************** class SequenceTree ****************/

#endif
