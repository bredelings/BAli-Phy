#ifndef ETREE_H
#define ETREE_H

#include "mytypes.H"
#include "sequence.H"

/***************** struct tree ********************/

struct node {
  node* parent;
  node* left;
  node* right;

  int name;

  node():parent(0),left(0),right(0),name(-1) {}
};

vector<int> get_neighbors(const node& n);

struct branch {
  int node1;
  int node2;
};

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
  void renumber();

public:
  enum direction { left , right, up };

  vector<branch> branches;
  vector<double> lengths;
  
  const node& operator[](int i) const { return *lookup[i];}
  node& operator[](int i) { return *lookup[i];}

  int leaves() const {return n_leaves;}
  int num_nodes() const {return lookup.size();}
  vector<int> path(int,int) const;

  bool ancestor(int n1, int n2) const {return ancestors[n1][n2];}
  std::valarray<bool> partition(int n1,int n2) const;

  TreeView copy() const;
  void add_left(node&,const tree&);
  void add_right(node&,const tree&);
  void add_root();
  
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

#endif
