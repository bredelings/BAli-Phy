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

  node():parent(0),left(0),right(0),name(-1) {}
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
  void destroy() {destroy(&root);}

  TreeView copy() const;

  operator node*() {return root;}

  TreeView(node* n):root(n) {}
  ~TreeView() {}
};

class tree {
  int n_leaves;
  node* root;

  vector<node*> lookup;

  void add_left(node&,node&);
  void add_right(node&,node&);

  vector< std::valarray<bool> > ancestors;

  void compute_ancestors();

  //renumber and return mapping of old->new node names
  vector<int> renumber();

  vector<Branch> branches_;
public:
  enum direction { left , right, up };

  Branch& branch(int i) {return branches_[i];}
  const Branch& branch(int i) const {return branches_[i];}
  
  const node& operator[](int i) const { return *lookup[i];}
  node& operator[](int i) { return *lookup[i];}

  int leaves() const {return n_leaves;}
  int branches() const {return num_nodes()-2;}
  int num_nodes() const {return lookup.size();}
  vector<int> path(int,int) const;

  bool ancestor(int n1, int n2) const {return ancestors[n1][n2];}
  std::valarray<bool> partition(int n1,int n2) const;

  TreeView copy() const;
  void add_left(node&,const tree&);
  void add_right(node&,const tree&);
  void add_root();
  void exchange(int,int);
  
  inline int parent(int) const;

  tree& operator=(const tree&);

  tree():root(0) {n_leaves=0;}
  tree(const tree& t1) {operator=(t1);}

  tree(const tree& t1,const tree& t2);
  tree(const tree& t1,double d1,const tree& t2,double d2);
 ~tree() {TreeView(root).destroy();}
};

inline bool leaf(const node& n) {
  return (!n.left && !n.right);
}

inline int tree::parent(int n) const {
  const tree& T = *this;
  assert(T[n].parent);
  int p = T[n].parent->name;
  if (!T[p].parent) {
    if (T[p].left->name != n)
      p = T[p].left->name;
    else
      p = T[p].right->name;
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
