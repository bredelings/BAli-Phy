#ifndef ETREE_H
#define ETREE_H

#include "mytypes.H"
#include "sequence.H"
#include <istream>
/***************** struct tree ********************/

struct node {
  node* parent;
  node* left;
  node* right;

  int name;
  int order;

  node():parent(0),left(0),right(0),name(-1),order(-1) {}
};

class nodeview {
  const node* n;
public:
  bool leaf() const {return not n->left and not n->right;}

  bool has_left() const {return bool(n->left);}
  bool has_right() const {return bool(n->right);}
  
  int name() const {return n->name;}
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

vector<int> get_neighbors(const node& n);

struct Branch {
  int node1;
  int node2;
  double length;
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

  void destroy(node**);
public:
  void destroy() {if (root) destroy(&root);}

  TreeView copy() const;

  operator node*() {return root;}

  TreeView(node* n):root(n) {}
  ~TreeView() {}
};

class tree {
  int n_leaves;
  node* root;

  vector<node*> names;
  vector<node*> order;

  // cache results of which branch is which way
  vector<int> left_branches;
  vector<int> right_branches;
  vector<int> up_branches;

  void add_left(node&,node&);
  void add_right(node&,node&);
  void add_left(node&,const tree&);
  void add_right(node&,const tree&);

  vector< std::valarray<bool> > ancestors;

  void compute_ancestors();

  //re-calculate the order of the nodes
  void reorder();

  vector<Branch> branches_;
  void  exchange_cousins(int,int);
protected:
  void add_root(); 

public:
  Branch& branch(int i) {
    assert(branches_[i].node1 == parent(branches_[i].node2));
    return branches_[i];
  }
  
  const Branch& branch(int i) const {
    assert(branches_[i].node1 == parent(branches_[i].node2));
    return branches_[i];
  }
  
  const nodeview operator[](int i) const { return nodeview(names[i]);}
  //  node& operator[](int i) { return *names[i];}

  int get_nth(int i) {return order[i]->name;}

  int leaves() const {return n_leaves;}
  int branches() const {return num_nodes()-2;}
  int num_nodes() const {return order.size();}
  vector<int> path(int,int) const;
  int branch_up(int) const;

  bool ancestor(int n1, int n2) const {return ancestors[n1][n2];}
  std::valarray<bool> partition(int n1,int n2) const;

  TreeView copy() const;
  void exchange(int,int);
  
  inline int parent(int) const;

  tree& operator=(const tree&);

  tree():root(0) {n_leaves=0;}
  tree(const tree& t1):root(0) {operator=(t1);}

  tree(const tree& t1,const tree& t2);
  tree(const tree& t1,double d1,const tree& t2,double d2);
 ~tree() {TreeView(root).destroy();}
};

inline bool leaf(const node& n) {
  return (not n.left and not n.right);
}

inline int tree::parent(int n) const {
  assert(names[n]->parent);
  int p = names[n]->parent->name;
  if (not names[p]->parent) {
    if (names[p]->left->name != n)
      p = names[p]->left->name;
    else
      p = names[p]->right->name;
  }
  return p;
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
  void write(std::ostream&,int n) const;

public:
  const string& seq(int i) const {return sequences[i];}
  const vector<string>& get_sequences() const {return sequences;}

  int index(const string&) const;

  void write(std::ostream&) const;

  explicit SequenceTree(const string&);
  SequenceTree(const sequence& s);
  SequenceTree(const SequenceTree& T1, const SequenceTree& T2);
  SequenceTree(std::istream&);   //load a tree from a file
};

inline SequenceTree operator+(const SequenceTree& t1,const SequenceTree& t2) {
  SequenceTree t3(t1,t2);

  return t3;
}

inline std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  T.write(o);
  return o;
}

/***************** class SequenceTree ****************/

#endif
