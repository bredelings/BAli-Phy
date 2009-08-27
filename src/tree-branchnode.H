/**
 * @file tree-branchnode.H
 *
 * @brief This file defines the struct BranchNode, which is used in creating
 *        an unrooted tree class defined in tree.H.  It also defines 
 *        several iterator classes for walking the tree in various ways.
 *        This structure is based on the unrooted tree structure in Felsensteins
 *        Phylogenetics book, but may not be exactly identical.
 */

#ifndef BRANCHNODE
#define BRANCHNODE


/// A structure that uniquely identifies both a node and a directed branch
struct BranchNode 
{
  /// The (integer) name of this directed branch
  int branch;
  /// The (integer) name of this node
  int node;

  /// Pointer to prev BranchNode in this node (forms a circular list)
  BranchNode *prev;
  /// Pointer to next BranchNode in this node (forms a circular list)
  BranchNode *next;
  /// Point to BranchNode on the other end of this branch
  BranchNode *out;

  /// The length of the this branch
  double length;

  /// Construct a NULL BranchNode
  BranchNode():branch(-1),node(-1),prev(NULL),next(NULL),out(NULL),length(-1) {}
  /// Construct a BranchNode for node n, and for branch b with length l
  BranchNode(int b,int n,double l):branch(b),node(n),prev(NULL),next(NULL),out(NULL),length(l) {}
};

/// Predicate: return true if this node is a leaf node.
inline bool is_leaf_node(const BranchNode* n) {
  return (n->prev == n);
}

/// Predicate: return true if this node is an internal node.
inline bool is_internal_node(const BranchNode* n) {
  return not is_leaf_node(n);
}

/// Predicate: return true if this branch is a leaf branch.
inline bool is_leaf_branch(const BranchNode* b) {
  return (is_leaf_node(b) or is_leaf_node(b->out));
}

/// Predicate: return true if this branch is an internal branch.
inline bool is_internal_branch(const BranchNode* b) {
  return not is_leaf_branch(b);
}

//------------------------------ BN_iterator ---------------------------//

/// FORWARD circular iterator for going over every BranchNode in a tree
class BN_iterator 
{
  /// The place we started iterating
  BranchNode const* start;
  /// The current position
  BranchNode* here;
  /// The number of times we have gone around the tree
  unsigned circuits_;
public:
  /// Dereference: return the current location
  BranchNode* operator*() const {return here;}

  /// Increment: step forward
  BN_iterator& operator++() {
    assert(here);
    assert(here->out);
    here = here->out->next;
    if (here == start) circuits_++;
    return *this;
  }

  /// Increment: step forward
  BN_iterator operator++(int) {
    BN_iterator pre(*this);
    operator++();
    return pre;
  }

  /// Decrement: step backward
  BN_iterator& operator--() {
    if (here == start) circuits_--;
    here = here->out->prev;
    return *this;
  }

  /// Decrement: step backward
  BN_iterator operator--(int) {
    BN_iterator pre(*this);
    operator--();
    return pre;
  }

  /// Access the number of circuits
  int circuits() const {return circuits_;}
  
  /// Have we finished a circuit yet?
  operator bool() const {return circuits_==0;}

  /// Construct the iterator from a starting location on the tree
  BN_iterator(BranchNode* b):start(b),here(b),circuits_(0) { }
};


/// A template function that puts BranchNode*'s into a list using an iterator.
template<class iterator_t,typename T>
inline void append(const iterator_t& start,T& v) {
  for(iterator_t i(start);i;i++)
    v.push_back(*i);
}


//------------------------ node_edges_iterator (base) ---------------------//

/// This class iterators over the BranchNodes in a node, and equivalently over the outgoing edges from a node
template <class node_t,class view_t>
class node_edges_iterator 
{
protected:
  /// The place we started iterating
  node_t const* start;

  /// The current position
  node_t* here;

public:

  /// Dereference: return the current location
  virtual view_t operator*() const =0;

  /// Increment: step forward
  void operator++() {
    here = here->next;
    if (here == start) here = NULL;
  }

  /// Increment: step forward
  void operator++(int) {
    operator++();
  }

  /// Have we finished yet?
  operator bool() const {return here;}

  /// How many outgoing edges do we have?
  unsigned size() const {
    node_t* temp = here;
    unsigned s = 0;
    while (temp) {
      temp = temp->next;
      if (temp == start) temp = NULL;
      s++;
    }
    return s;
  }

  /// Construct the iterator from a starting location on the node
  node_edges_iterator(node_t *b):start(b),here(b) {
    if (here->out == here) here=NULL;
  }

  virtual ~node_edges_iterator() {}
};

class nodeview;
class const_nodeview;

class branchview;
class const_branchview;

/// A const iterator which iterates over neighboring nodes
class const_neighbors_iterator:public node_edges_iterator<const BranchNode,const_nodeview> {
public:
  inline const_nodeview operator*() const;

  const_neighbors_iterator(const BranchNode* b):node_edges_iterator<const BranchNode,const_nodeview>(b) {}
};

/// An iterator which iterates over neighboring nodes
class neighbors_iterator:public node_edges_iterator<BranchNode,nodeview> {
public:
  inline nodeview operator*() const;

  neighbors_iterator(BranchNode* b):node_edges_iterator<BranchNode,nodeview>(b) {}
};

/// A const iterator which iterates over incoming edges
class const_in_edges_iterator:public node_edges_iterator<const BranchNode,const_branchview> {
public:
  inline const_branchview operator*() const;

  const_in_edges_iterator(const BranchNode* b):node_edges_iterator<const BranchNode,const_branchview>(b) {}
};

/// An iterator which iterates over incoming edges
class in_edges_iterator:public node_edges_iterator<BranchNode,branchview> {
public:
  inline branchview operator*() const;

  in_edges_iterator(BranchNode* b):node_edges_iterator<BranchNode,branchview>(b) {}
};

/// A const iterator which iterates over outgoing edges
class const_out_edges_iterator:public node_edges_iterator<const BranchNode,const_branchview> {
public:
  inline const_branchview operator*() const;

  const_out_edges_iterator(const BranchNode* b):node_edges_iterator<const BranchNode,const_branchview>(b) {}
};

/// An iterator which iterates over outgoing edges
class out_edges_iterator:public node_edges_iterator<BranchNode,branchview> {
public:
  inline branchview operator*() const;

  out_edges_iterator(BranchNode* b):node_edges_iterator<BranchNode,branchview>(b) {}
};

//-------------------------------- nodeview ---------------------------------//

/// This class wraps a BranchNode* to use it as a reference to a node
class nodeview 
{
protected:
  /// A BranchNode in the node that we are referring to.
  BranchNode* n;

  friend class const_nodeview;
public:
  /// Predicate: return true if this node is a leaf node.
  bool is_leaf_node() const {return ::is_leaf_node(n);}
  /// Predicate: return true if this node is an internal node.
  bool is_internal_node() const {return ::is_internal_node(n);}

  /// The (integer) name of this node
  int name() const {return n->node;}

  /// Return an iterator over the neighbors of this node.
  neighbors_iterator neighbors() const {return n;}
  /// Return an iterator over this node's incoming branches.
  in_edges_iterator  branches_in() const {return n;}
  /// Return an iterator over this node's outgoing branches.
  out_edges_iterator branches_out() const {return n;}
  
  /// Compute the degree of this node
  unsigned degree() const {return neighbors().size();}

  /// The (integer) name of this node
  operator int() const {return name();}

  /// Access the BranchNode* used to reference the node.
  operator BranchNode*() const {return n;}

  /// Is this nodeview non-NULL?
  bool valid() const {return n;}

  /// Construct the nodeview from a BranchNode*.
  nodeview(BranchNode* bn):n(bn) {}
};

/// This class wraps a BranchNode* to use it as a non-constant reference to a node
class const_nodeview {
protected:
  /// A BranchNode in the node that we are referring to.
  const BranchNode* n;
public:
  /// Predicate: return true if this node is a leaf node.
  bool is_leaf_node() const {return ::is_leaf_node(n);}
  /// Predicate: return true if this node is an internal node.
  bool is_internal_node() const {return ::is_internal_node(n);}

  /// The (integer) name of this node
  int name() const {return n->node;}

  /// Return an iterator over the neighbors of this node.
  const_neighbors_iterator neighbors()    const {return n;}
  /// Return an iterator over this node's incoming branches.
  const_in_edges_iterator  branches_in()  const {return n;}
  /// Return an iterator over this node's outgoing branches.
  const_out_edges_iterator branches_out() const {return n;}
  
  /// Compute the degree of this node
  unsigned degree() const {return neighbors().size();}

  /// The (integer) name of this node
  operator int() const {return name();}
  
  /// The (integer) name of this node
  bool valid() const {return n;}

  /// Construct the const_nodeview from a BranchNode*.
  const_nodeview(const BranchNode* bn):n(bn) {}
  /// Construct the const_nodeview from a nodeview
  const_nodeview(const nodeview& nv):n(nv.n) {}
};

inline nodeview neighbors_iterator::operator*() const {
  return here->out;
}

inline const_nodeview const_neighbors_iterator::operator*() const {
  return here->out;
}

//-------------------------------- branchview ---------------------------------//

/// An iterator which iterates over incoming edges
class edges_before_iterator: public in_edges_iterator {
public:
  edges_before_iterator(BranchNode* b)
    :in_edges_iterator(b)
  {
    (*this)++;
  }
};


/// A const iterator which iterates over incoming edges
class const_edges_before_iterator: public const_in_edges_iterator {
public:
  const_edges_before_iterator(const BranchNode* b)
    :const_in_edges_iterator(b)
  {
    (*this)++;
  }
};


/// An iterator which iterates over outgoing edges
class edges_after_iterator: public out_edges_iterator {
public:
  edges_after_iterator(BranchNode* b)
    :out_edges_iterator(b->out)
  {
    (*this)++;
  }
};


/// A const iterator which iterates over outgoing edges
class const_edges_after_iterator: public const_out_edges_iterator {
public:
  const_edges_after_iterator(const BranchNode* b)
    :const_out_edges_iterator(b->out)
  {
    (*this)++;
  }
};


/// This class wraps a BranchNode* to use it as a non-constant reference to a directed branch
class branchview {
protected:
  /// A BranchNode that corresponds to the directed branch.
  BranchNode* b;

  friend class const_branchview;
public:
  /// The node that this branch is from.
  nodeview source() const { return b;}
  /// The node that this branch is to.
  nodeview target() const { return b->out;}

  /// The (integer) name of this directed branch.
  int name() const {return b->branch;}
  /// The (integer) name of this branch, as an undirected branch.
  int undirected_name() const { return std::min(b->branch,b->out->branch); }

  /// Get a reference to an undirected branch, which is the direction with the smaller name
  branchview undirected() const {
    BranchNode* BN = b;

    assert(BN->out->branch != BN->branch);
    if (BN->out->branch < BN->branch)
      BN = BN->out;

    return BN;
  }

  /// Predicate: return true if this branch is a leaf branch.
  bool is_leaf_branch() const {return ::is_leaf_branch(b);}
  /// Predicate: return true if this branch is an internal branch.
  bool is_internal_branch() const {return ::is_internal_branch(b);}

  /// Compute the that is identical to this one, but in the reverse direction
  branchview reverse() const {return b->out;}

  /// Return an iterator over the branches adjacent to this branch, and pointing to its source
  edges_before_iterator branches_before() const  {return b;}

  /// Return an iterator over the branches adjacent to this branch, and pointing away from its target
  edges_after_iterator branches_after() const {return b;}

  /// Return this branch's length
  double length() const {return b->length;}

  /// Alter this branch's length
  void set_length(double d) const {b->out->length = b->length = d;}

  /// The (integer) name of this directed branch
  operator int() const {return name();}
  /// Access the BranchNode* used to reference the directed branch.
  operator BranchNode*() const {return b;}

  /// Is this branchview non-NULL?
  bool valid() const {return b;}
  /// Construct the branchview from a BranchNode*.
  branchview(BranchNode* bn):b(bn) { }
};

/// This class wraps a BranchNode* to use it as a constant reference to a directed branch
class const_branchview {
protected:
  /// A BranchNode that corresponds to the directed branch.
  const BranchNode* b;
public:
  /// The node that this branch is from.
  const_nodeview source() const { return b;}
  /// The node that this branch is to.
  const_nodeview target() const { return b->out;}

  /// The (integer) name of this directed branch.
  int name() const {return b->branch;}
  /// The (integer) name of this branch, as an undirected branch.
  int undirected_name() const { return std::min(b->branch,b->out->branch); }

  /// Get a reference to an undirected branch, which is the direction with the smaller name
  const_branchview undirected() const {
    const BranchNode* BN = b;

    assert(BN->out->branch != BN->branch);
    if (BN->out->branch < BN->branch)
      BN = BN->out;

    return BN;
  }

  /// Predicate: return true if this branch is a leaf branch.
  bool is_leaf_branch() const {return ::is_leaf_branch(b);}
  /// Predicate: return true if this branch is an internal branch.
  bool is_internal_branch() const {return ::is_internal_branch(b);}

  /// Compute the that is identical to this one, but in the reverse direction
  const_branchview reverse() const {return b->out;}

  /// Return an iterator over the branches adjacent to this branch, and pointing to its source
  const_edges_before_iterator branches_before() const  {return b;}

  /// Return an iterator over the branches adjacent to this branch, and pointing away from its target
  const_edges_after_iterator branches_after() const {return b;}

  /// Return this branch's length
  double length() const {return b->length;}

  /// The (integer) name of this directed branch
  operator int() const {return name();}
  /// Is this branchview non-NULL?
  bool valid() const {return b;}
  /// Construct the const_branchview from a BranchNode*.
  const_branchview(const BranchNode* bn):b(bn)   {}
  /// Construct the const_branchview from a branchview
  const_branchview(const branchview& bv):b(bv.b) {}
};

inline const_branchview const_in_edges_iterator::operator*() const {
  return here->out;
}

inline branchview in_edges_iterator::operator*() const {
  return here->out;
}

inline const_branchview const_out_edges_iterator::operator*() const {
  return here;
}

inline branchview out_edges_iterator::operator*() const {
  return here;
}

#endif
