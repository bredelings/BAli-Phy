#ifndef BRANCHNODE
#define BRANCHNODE


//------------------------------ BranchNode ---------------------------//

struct BranchNode {
  int branch;
  int node;

  BranchNode *prev;
  BranchNode *next;
  BranchNode *out;

  class Tree* tree;

  BranchNode():branch(-1),node(-1),prev(NULL),next(NULL),out(NULL),tree(NULL) {}
  BranchNode(int b,int n):branch(b),node(n),prev(NULL),next(NULL),out(NULL),tree(NULL) {}
};

inline bool is_leaf_node(const BranchNode* n) {
  return (n->prev == n);
}

inline bool is_internal_node(const BranchNode* n) {
  return not is_leaf_node(n);
}

inline bool is_leaf_branch(const BranchNode* b) {
  return (is_leaf_node(b) or is_leaf_node(b->out));
}

inline bool is_internal_branch(const BranchNode* b) {
  return not is_leaf_branch(b);
}

//------------------------------ BN_iterator ---------------------------//

/// FORWARD circular iterator for going over every BranchNode in a tree
class BN_iterator {
  BranchNode* start;
  BranchNode* here;
  unsigned circuits_;
public:
  BranchNode* operator*() const {return here;}

  BN_iterator& operator++() {
    here = here->out->next;
    if (here == start) circuits_++;
    return *this;
  }

  BN_iterator operator++(int) {
    BN_iterator pre(*this);
    operator++();
    return pre;
  }

  BN_iterator& operator--() {
    if (here == start) circuits_--;
    here = here->out->prev;
    return *this;
  }

  BN_iterator operator--(int) {
    BN_iterator pre(*this);
    operator--();
    return pre;
  }

  int circuits() const {return circuits_;}
  
  operator bool() const {return circuits_==0;}

  BN_iterator(BranchNode* b):start(b),here(b),circuits_(0) { }
};


template<class iterator_t,typename T>
void append(const iterator_t& start,std::vector<T>& v) {
  for(iterator_t i(start);i;i++)
    v.push_back(*i);
}


//------------------------ node_edges_iterator (base) ---------------------//
template <class T>
class node_edges_iterator 
{
protected:
  const BranchNode* start;
  const BranchNode* here;
  unsigned circuits_;

public:
  virtual T operator*() const =0;

  void operator++() {
    here = here->next;
    if (here == start) here = NULL;
  }

  void operator++(int) {
    operator++();
  }

  operator bool() const {return here;}

  unsigned size() const {
    BranchNode* temp = start->next;
    unsigned s = 1;
    while(temp != start) {
      temp = temp->next;
      s++;
    }
    return s;
  }

  node_edges_iterator(const BranchNode *b):start(b),here(b) {}
};

class nodeview;
class const_nodeview;

class branchview;
class const_branchview;

// Neighbors
class neighbors_iterator:public node_edges_iterator<nodeview> {
public:
  nodeview operator*() const;

  neighbors_iterator(const BranchNode* b):node_edges_iterator<nodeview>(b) {}
};

class const_neighbors_iterator:public node_edges_iterator<const_nodeview> {
public:
  const_nodeview operator*() const;

  const_neighbors_iterator(const BranchNode* b):node_edges_iterator<const_nodeview>(b) {}
};

// in_edges
class in_edges_iterator:public node_edges_iterator<branchview> {
public:
  branchview operator*() const;

  in_edges_iterator(const BranchNode* b):node_edges_iterator<branchview>(b) {}
};

class const_in_edges_iterator:public node_edges_iterator<const_branchview> {
public:
  const_branchview operator*() const;

  const_in_edges_iterator(const BranchNode* b):node_edges_iterator<const_branchview>(b) {}
};

// out_edges
class out_edges_iterator:public node_edges_iterator<branchview> {
public:
  branchview operator*() const;

  out_edges_iterator(const BranchNode* b):node_edges_iterator<branchview>(b) {}
};

class const_out_edges_iterator:public node_edges_iterator<const_branchview> {
public:
  const_branchview operator*() const;

  const_out_edges_iterator(const BranchNode* b):node_edges_iterator<const_branchview>(b) {}
};

//-------------------------------- nodeview ---------------------------------//

class const_nodeview {
protected:
  const BranchNode* n;
public:
  bool is_leaf_node() const {return ::is_leaf_node(n);}
  bool is_internal_node() const {return ::is_internal_node(n);}

  int name() const {return n->node;}

  const_neighbors_iterator neighbors()    const {return n;}
  const_in_edges_iterator  branches_in()  const {return n;}
  const_out_edges_iterator branches_out() const {return n;}
  
  operator int() const {return name();}

  const_nodeview(const BranchNode* bn):n(bn) {}
};

class nodeview: public const_nodeview {
protected:
  nodeview(const const_nodeview& nv):const_nodeview(nv) {}
public:
  const neighbors_iterator neighbors() const {return n;}
  const in_edges_iterator  branches_in() const {return n;}
  const out_edges_iterator branches_out() const {return n;}
  
  nodeview(const BranchNode* bn):const_nodeview(bn) {}
};

inline nodeview neighbors_iterator::operator*() const {
  return here->out;
}

inline const_nodeview const_neighbors_iterator::operator*() const {
  return here->out;
}

//-------------------------------- branchview ---------------------------------//

class edges_before_iterator: public in_edges_iterator {
public:
  edges_before_iterator(const BranchNode* b)
    :in_edges_iterator(b)
  {
    (*this)++;
  }
};


class const_edges_before_iterator: public const_in_edges_iterator {
public:
  const_edges_before_iterator(const BranchNode* b)
    :const_in_edges_iterator(b)
  {
    (*this)++;
  }
};


class edges_after_iterator: public out_edges_iterator {
public:
  edges_after_iterator(const BranchNode* b)
    :out_edges_iterator(b->out)
  {
    (*this)++;
  }
};


class const_edges_after_iterator: public const_out_edges_iterator {
public:
  const_edges_after_iterator(const BranchNode* b)
    :const_out_edges_iterator(b->out)
  {
    (*this)++;
  }
};


class const_branchview {
protected:
  const BranchNode* b;
public:
  const_nodeview source() const { return b;}
  const_nodeview target() const { return b->out;}

  int name() const {return b->branch;}
  int undirected_name() const { return std::min(b->branch,b->out->branch); }

  const_branchview undirected() const {
    const BranchNode* BN = b;
    if (BN->out->branch < BN->branch)
      BN = BN->out;
    return BN;
  }

  bool is_leaf_branch() const {return ::is_leaf_branch(b);}
  bool is_internal_branch() const {return ::is_internal_branch(b);}

  const_branchview reverse() const {return b->out;}

  const_edges_before_iterator branches_before() const  {return b;}

  const_edges_after_iterator branches_after() const {return b;}

  double length() const;

  operator int() const {return name();}
  const_branchview(const BranchNode* b_):b(b_) {}
};

class branchview: public const_branchview 
{
protected:
  branchview(const const_branchview& bv):const_branchview(bv) {}
public:
  branchview undirected() const {return branchview::undirected();}

  branchview reverse() const {return branchview::reverse();}

  edges_before_iterator branches_before() const  {return b;}

  edges_after_iterator branches_after() const {return b;}

  double& length() const;

  operator int() const {return name();}
  branchview(const BranchNode* bn):const_branchview(bn) {}
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
