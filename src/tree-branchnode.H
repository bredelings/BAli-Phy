/*
   Copyright (C) 2004-2007,2009 Benjamin Redelings

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
 * @file tree-branchnode.H
 *
 * @brief This file defines the struct BranchNode, which is used in creating
 *        an unrooted tree class defined in tree.H.  It also defines 
 *        several iterator classes for walking the tree in various ways.
 *        This structure is based on the unrooted tree structure in Felsenstein's
 *        Phylogenetics book, but may not be exactly identical.
 *
 *        FIXME - Rewrite circular iterators to fit into the standard framework by
 *                + having a circular list class with ..
 *                + begin() and end() functions?
 *                Nontrivial, but could be done.
 *                Then, the container class, not the iterators, could convert to vector<>.
 */


/*
 * Add node and branch pointers to attribute structures.
 *
 * - Use copy-on-write pointers so that when we copy a tree w/ a lot of data, we don't have to copy the data
 *   that we don't modify.
 *
 * - The attribute structure will be a vector of boost::any.  The user of an attribute must know what its type is.
 *
 * - Attributes will be indexed, but unnamed in attribute list structure.  However, class Tree will store 
 *   names for attributes.  Empty names correspond to unnamed attributes.
 *
 * - Possibly reserve node attribute 0 for node names and branch attribute 0 for branch lengths.
 *
 * - Do I want to try to put node names (indices) into the attribute structure?
 *   + How would I handle directed branch names?
 */

#ifndef BRANCHNODE
#define BRANCHNODE

#include <vector>

#include <boost/any.hpp>
#include <boost/intrusive_ptr.hpp>

struct tree_attributes: public std::vector< boost::any >
{
  int count;
  int name;

  tree_attributes* unused_copy() const
  {
    tree_attributes* temp = new tree_attributes(*this);
    temp->count = 0;
    return temp;
  }

  tree_attributes():count(0),name(-1) { }
  tree_attributes(int n):std::vector< boost::any >(n), count(0),name(-1) { }
};


inline void intrusive_ptr_add_ref(tree_attributes* a)
{
  ++a->count;
}

inline void intrusive_ptr_release(tree_attributes* a)
{
  if (--(a->count) == 0)
    delete a;
}

// Built-in attributes:
// undirected_branch[0] = double branch length


/// A structure that uniquely identifies both a node and a directed branch
struct BranchNode 
{
  /// Attributes for this (undirected) branch
  boost::intrusive_ptr<tree_attributes> undirected_branch_attributes;
  /// Attributes for this (directed) branch
  boost::intrusive_ptr<tree_attributes> directed_branch_attributes;
  /// Attributes for this node
  boost::intrusive_ptr<tree_attributes> node_attributes;

  /// Pointer to prev BranchNode in this node (forms a circular list)
  BranchNode *prev;
  /// Pointer to next BranchNode in this node (forms a circular list)
  BranchNode *next;
  /// Point to BranchNode on the other end of this branch
  BranchNode *out;

  /// Construct a NULL BranchNode
  BranchNode()
    :prev(NULL),
     next(NULL),
     out(NULL)
  {}
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
inline bool is_branch(const BranchNode* b) 
{
  return (b->out);
}

/// Predicate: return true if this branch is a leaf branch.
inline bool is_leaf_branch(const BranchNode* b) {
  return (is_branch(b) and (is_leaf_node(b) or is_leaf_node(b->out)));
}

/// Predicate: return true if this branch is an internal branch.
inline bool is_internal_branch(const BranchNode* b) {
  return (is_branch(b) and not is_leaf_branch(b));
}

inline int degree(const BranchNode* b)
{
  int d = 1;
  const BranchNode* b2 = b->next;
  while(b2 != b) {
    b2 = b2->next;
    d++;
  }
  return d;
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
  virtual view_t operator*() const {return here;}

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

  operator std::vector<view_t> () const
  {
    std::vector<view_t> v;
    append(*this,v);
    return v;
  }

  template <template<typename,typename> class V,class A> 
  V<view_t,A> as() const
  {
    V<view_t,A> v;
    append(*this,v);
    return v;
  }

  template <template<typename,typename> class V> 
  V<view_t,std::allocator<view_t> > as() const
  {
    V<view_t,std::allocator<view_t> > v;
    append(*this,v);
    return v;
  }

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
  int name() const {return n->node_attributes->name;}

  /// Return an iterator over the neighbors of this node.
  neighbors_iterator neighbors() const {return n;}
  /// Return an iterator over this node's incoming branches.
  in_edges_iterator  branches_in() const {return n;}
  /// Return an iterator over this node's outgoing branches.
  out_edges_iterator branches_out() const {return n;}
  
  /// Compute the degree of this node
  unsigned degree() const {return ::degree(n);}

  /// The (integer) name of this node
  operator int() const {return name();}

  /// Access the BranchNode* used to reference the node.
  operator BranchNode*() const {return n;}

  /// Is this nodeview non-NULL?
  bool valid() const {return n;}

        tree_attributes& attributes()       {return *n->node_attributes;}
  const tree_attributes& attributes() const {return *n->node_attributes;}

        boost::any& attribute(int i)       {return attributes()[i];}
  const boost::any& attribute(int i) const {return attributes()[i];}

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
  int name() const {return n->node_attributes->name;}

  /// Return an iterator over the neighbors of this node.
  const_neighbors_iterator neighbors()    const {return n;}
  /// Return an iterator over this node's incoming branches.
  const_in_edges_iterator  branches_in()  const {return n;}
  /// Return an iterator over this node's outgoing branches.
  const_out_edges_iterator branches_out() const {return n;}
  
  /// Compute the degree of this node
  unsigned degree() const {return ::degree(n);}

  /// The (integer) name of this node
  operator int() const {return name();}
  
  /// The (integer) name of this node
  bool valid() const {return n;}

  const tree_attributes& attributes() const {return *n->node_attributes;}
  const boost::any& attribute(int i) const {return attributes()[i];}

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
  int name() const {return b->directed_branch_attributes->name;}
  /// The (integer) name of this branch, as an undirected branch.
  int undirected_name() const { return b->undirected_branch_attributes->name; }

  /// Get a reference to an undirected branch, which is the direction with the smaller name
  branchview undirected() const 
  {
    BranchNode* BN = b;

    assert(name() != reverse().name());
    if (name() != undirected_name())
    {
      BN = BN->out;
    }

    assert(BN->undirected_branch_attributes->name == undirected_name());

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

  bool has_length() const {return not (*b->undirected_branch_attributes)[0].empty(); }

  /// Return this branch's length
  double length() const { return *boost::unsafe_any_cast<double>(&(*b->undirected_branch_attributes)[0]); }

  /// Alter this branch's length
  void set_length(double d) const {(*b->undirected_branch_attributes)[0] = d;}

  tree_attributes& undirected_attributes() {return *b->undirected_branch_attributes;}
  const tree_attributes& undirected_attributes() const {return *b->undirected_branch_attributes;}

        boost::any& undirected_attribute(int i)       {return undirected_attributes()[i];}
  const boost::any& undirected_attribute(int i) const {return undirected_attributes()[i];}

  tree_attributes& directed_attributes() {return *b->directed_branch_attributes;}
  const tree_attributes& directed_attributes() const {return *b->directed_branch_attributes;}

        boost::any& directed_attribute(int i)       {return directed_attributes()[i];}
  const boost::any& directed_attribute(int i) const {return directed_attributes()[i];}

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
  int name() const {return b->directed_branch_attributes->name;}
  /// The (integer) name of this branch, as an undirected branch.
  int undirected_name() const { return b->undirected_branch_attributes->name;}

  /// Get a reference to an undirected branch, which is the direction with the smaller name
  const_branchview undirected() const 
  {
    const BranchNode* BN = b;

    assert(name() != reverse().name());
    if (name() != undirected_name())
    {
      BN = BN->out;
    }

    assert(BN->undirected_branch_attributes->name == undirected_name());
    
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

  bool has_length() const {return not (*b->undirected_branch_attributes)[0].empty(); }

  /// Return this branch's length
  double length() const { return boost::any_cast<double>((*b->undirected_branch_attributes)[0]); }

  const tree_attributes& undirected_attributes() const {return *b->undirected_branch_attributes;}
  const boost::any& undirected_attribute(int i) const {return undirected_attributes()[i];}
  const tree_attributes& directed_attributes() const {return *b->directed_branch_attributes;}
  const boost::any& directed_attribute(int i) const {return directed_attributes()[i];}

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

std::vector<int> directed_names(const std::vector<branchview>& v);
std::vector<int> directed_names(const std::vector<const_branchview>& v);

#endif
