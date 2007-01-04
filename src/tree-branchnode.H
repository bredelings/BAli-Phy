#ifndef BRANCHNODE
#define BRANCHNODE


//------------------------------ BranchNode ---------------------------//

struct BranchNode {
  int branch;
  int node;

  BranchNode *prev;
  BranchNode *next;
  BranchNode *out;

  double length;

  BranchNode():branch(-1),node(-1),prev(NULL),next(NULL),out(NULL),length(-1) {}
  BranchNode(int b,int n,double l):branch(b),node(n),prev(NULL),next(NULL),out(NULL),length(l) {}
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
  BranchNode const* start;
  BranchNode* here;
  unsigned circuits_;
public:
  BranchNode* operator*() const {return here;}

  BN_iterator& operator++() {
    assert(here);
    assert(here->out);
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
inline void append(const iterator_t& start,T& v) {
  for(iterator_t i(start);i;i++)
    v.push_back(*i);
}


//------------------------ node_edges_iterator (base) ---------------------//
template <class node_t,class view_t>
class node_edges_iterator 
{
protected:
  node_t const* start;
  node_t* here;

public:
  virtual view_t operator*() const =0;

  void operator++() {
    here = here->next;
    if (here == start) here = NULL;
  }

  void operator++(int) {
    operator++();
  }

  operator bool() const {return here;}

  unsigned size() const {
    node_t* temp = start->next;
    unsigned s = 1;
    while(temp != start) {
      temp = temp->next;
      s++;
    }
    return s;
  }

  node_edges_iterator(node_t *b):start(b),here(b) {
    if (here->out == here) here=NULL;
  }

  virtual ~node_edges_iterator() {}
};

class nodeview;
class const_nodeview;

class branchview;
class const_branchview;

// Neighbors
class const_neighbors_iterator:public node_edges_iterator<const BranchNode,const_nodeview> {
public:
  inline const_nodeview operator*() const;

  const_neighbors_iterator(const BranchNode* b):node_edges_iterator<const BranchNode,const_nodeview>(b) {}
};

class neighbors_iterator:public node_edges_iterator<BranchNode,nodeview> {
public:
  inline nodeview operator*() const;

  neighbors_iterator(BranchNode* b):node_edges_iterator<BranchNode,nodeview>(b) {}
};

// in_edges
class const_in_edges_iterator:public node_edges_iterator<const BranchNode,const_branchview> {
public:
  inline const_branchview operator*() const;

  const_in_edges_iterator(const BranchNode* b):node_edges_iterator<const BranchNode,const_branchview>(b) {}
};

class in_edges_iterator:public node_edges_iterator<BranchNode,branchview> {
public:
  inline branchview operator*() const;

  in_edges_iterator(BranchNode* b):node_edges_iterator<BranchNode,branchview>(b) {}
};

// out_edges
class const_out_edges_iterator:public node_edges_iterator<const BranchNode,const_branchview> {
public:
  inline const_branchview operator*() const;

  const_out_edges_iterator(const BranchNode* b):node_edges_iterator<const BranchNode,const_branchview>(b) {}
};

class out_edges_iterator:public node_edges_iterator<BranchNode,branchview> {
public:
  inline branchview operator*() const;

  out_edges_iterator(BranchNode* b):node_edges_iterator<BranchNode,branchview>(b) {}
};

//-------------------------------- nodeview ---------------------------------//

class nodeview 
{
protected:
  BranchNode* n;

  friend class const_nodeview;
public:
  bool is_leaf_node() const {return ::is_leaf_node(n);}
  bool is_internal_node() const {return ::is_internal_node(n);}

  int name() const {return n->node;}

  neighbors_iterator neighbors() const {return n;}
  in_edges_iterator  branches_in() const {return n;}
  out_edges_iterator branches_out() const {return n;}
  
  unsigned degree() const {return neighbors().size();}

  operator int() const {return name();}

  operator BranchNode*() const {return n;}

  bool valid() const {return n;}

  nodeview(BranchNode* bn):n(bn) {}
};

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
  
  unsigned degree() const {return neighbors().size();}

  operator int() const {return name();}
 
  bool valid() const {return n;}

  const_nodeview(const BranchNode* bn):n(bn) {}
  const_nodeview(const nodeview& nv):n(nv.n) {}
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
  edges_before_iterator(BranchNode* b)
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
  edges_after_iterator(BranchNode* b)
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


class branchview {
protected:
  BranchNode* b;

  friend class const_branchview;
public:
  nodeview source() const { return b;}
  nodeview target() const { return b->out;}

  int name() const {return b->branch;}
  int undirected_name() const { return std::min(b->branch,b->out->branch); }

  branchview undirected() const {
    BranchNode* BN = b;

    assert(BN->out->branch != BN->branch);
    if (BN->out->branch < BN->branch)
      BN = BN->out;

    return BN;
  }

  bool is_leaf_branch() const {return ::is_leaf_branch(b);}
  bool is_internal_branch() const {return ::is_internal_branch(b);}

  branchview reverse() const {return b->out;}

  edges_before_iterator branches_before() const  {return b;}

  edges_after_iterator branches_after() const {return b;}

  double length() const {return b->length;}

  void set_length(double d) const {b->out->length = b->length = d;}

  operator int() const {return name();}
  operator BranchNode*() const {return b;}

  bool valid() const {return b;}
  branchview(BranchNode* bn):b(bn) { }
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

    assert(BN->out->branch != BN->branch);
    if (BN->out->branch < BN->branch)
      BN = BN->out;

    return BN;
  }

  bool is_leaf_branch() const {return ::is_leaf_branch(b);}
  bool is_internal_branch() const {return ::is_internal_branch(b);}

  const_branchview reverse() const {return b->out;}

  const_edges_before_iterator branches_before() const  {return b;}

  const_edges_after_iterator branches_after() const {return b;}

  double length() const {return b->length;}

  operator int() const {return name();}
  bool valid() const {return b;}
  const_branchview(const BranchNode* bn):b(bn)   {}
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
