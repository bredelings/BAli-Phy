#include "tree.H"
#include "exponential.H"
#include <algorithm>
#include <sstream>

using std::vector;
using std::valarray;

void TreeView::destroy_tree(node* n) {
  if (not n) return;

  if (n->parent_branch) {
    delete n->parent_branch;
    n->parent_branch = 0;
  }

  destroy_tree(n->left);
  n->left = 0;

  destroy_tree(n->right);
  n->right = 0;

  delete n;
}

void TreeView::copy_tree(node* n) {

  if (n->parent) {
    assert(n->parent_branch);

    n->parent_branch = new Branch( *(n->parent_branch) );
    n->parent_branch->parent = n->parent;
    n->parent_branch->child = n;
  }

  if (n->left) {
    n->left = new node( *(n->left) );
    n->left->parent = n;
    copy_tree(n->left);
  }

  if (n->right) {
    n->right = new node( *(n->right) );
    n->right->parent = n;
    copy_tree(n->right);
  }
}

TreeView TreeView::copy() const {
  if (not root)
    return TreeView(0);

  node* top = new node(*root);

  copy_tree(top);

  return TreeView(top);
}

void TreeView::exchange_cousins(node* node1, node* node2) {
  node* p1 = node1->parent;
  node* p2 = node2->parent;

  // The nodes are distinct
  assert(node1 != node2);

  // The nodes don't have the same parent
  assert(p1 != p2);

  // We assume that either node is an ancestor of the other

  if (p1->left == node1)
    p1->left = node2;
  else
    p1->right = node2;

  if (p2->left == node2)
    p2->left = node1;
  else
    p2->right = node1;

  node1->parent = p2;
  node2->parent = p1;

  node1->parent_branch->parent = node1->parent;
  node2->parent_branch->parent = node2->parent;
}

vector<node*> find_nodes(node* root) {
  assert(root);

  vector<node*> order;

  order.push_back(root);
  
  int start=0;
  do {
    int end = order.size();
    for(int i=start;i<end;i++) {
      node* left = order[i]->left;
      node* right = order[i]->right;
      if (left) order.push_back(left);
      if (right) order.push_back(right);
    }
    start = end;
  }  while(start<order.size());
  return order;
}


vector<node*> find_leaves(node* root) {
  vector<node*> nodes = find_nodes(root);
  vector<node*> leaves;

  for(int i=0;i<nodes.size();i++)
    if (leaf(*nodes[i]))
      leaves.push_back(nodes[i]);

  return leaves;
}

/**************** Begin 'tree' implementation ***************/
// Until stated otherwise, the tree is considered rooted

void add_left(node* parent,node* child) {
  assert(not parent->left);

  parent->left = child;
  child->parent = parent;

  child->parent_branch = new Branch;
  child->parent_branch->child = child;
  child->parent_branch->parent = parent;
}

void add_right(node* parent,node* child) {
  assert(not parent->right);

  parent->right = child;
  child->parent = parent;

  child->parent_branch = new Branch;
  child->parent_branch->child = child;
  child->parent_branch->parent = parent;
}


//NOTE - if we remove root->right->parent_branch, we must patch this up
void remove_subtree(node* n,node* &root) {
  // don't remove the root!
  assert(n->parent);

  // get parent and sibling
  node* parent = n->parent;
  node* sibling = n->parent->right;
  if (n == sibling) sibling = n->parent->left;

  // this node must have a sibling
  assert(sibling);

  // connect sibling to parent->parent (as its parent)
  sibling->parent = parent->parent;
  //FIXME - we have unneeded pointers here...
  if (parent->parent)
    sibling->parent_branch->parent = sibling->parent;
  else {
    delete sibling->parent_branch;
    sibling->parent_branch=0;
  }

  // set grandparent pointer to point to sibling (as its child)
  if (not parent->parent)
    root = sibling;
  else {
    // connect parent->parent to sibling (as its child)
    if (parent->parent->right == parent)
      parent->parent->right = sibling;
    else
      parent->parent->left = sibling;

    // Merge the two branch lengths
    sibling->parent_branch->length += parent->parent_branch->length;
    parent->parent_branch->length = 0.0;
  }

}

// Do I need to export an interface which maps each node name
// on the two old trees into a node name in the current tree?
// I could put a field 'old_name' in the 'struct node' for this...

void insert_subtree(node* n,node* new_sibling) {
  
  node* parent = n->parent;
  node* g_parent = new_sibling->parent;

  // Set up new parent node and branch
  parent->left = n;
  parent->right = new_sibling;
  parent->parent = g_parent;
  parent->parent_branch->parent = g_parent;

  // attatch new_sibling to parent
  new_sibling->parent = n->parent;
  new_sibling->parent_branch->parent = n->parent;

  // attatch g_parent to parent
  if (g_parent->left == new_sibling)
    g_parent->left = parent;
  else
    g_parent->right = parent;
}



// subtree(root,tree::left) = T
void tree::add_left(node* parent,const tree& T) {
  node* child = T.copy();
  ::add_left(parent,child);

  // re-name the leaves in the newly added subtree
  vector<node*> leaves = find_leaves(child);
  for(int i=0;i<leaves.size();i++)
    leaves[i]->name += n_leaves_;

  // compute order and internal names from leaf names
  reanalyze();
}

// subtree(root,tree::right) = T
void tree::add_right(node* parent,const tree& T) {
  node* child = T.copy();
  ::add_right(parent,child);

  // re-name the leaves in the newly added subtree
  vector<node*> leaves = find_leaves(child);
  for(int i=0;i<leaves.size();i++)
    leaves[i]->name += n_leaves_;

  // compute order and internal names from leaf names
  reanalyze();
}

/// Remove the subtree with root node n
void tree::remove_subtree(node* n) {
  // Take the subtree @ n out of the tree
  ::remove_subtree(n,root);

  // Then free up the removed subtree
  TreeView(n).destroy();
  delete n->parent->parent_branch;
  delete n->parent;
  
  // Fix-up branch lengths so that branch right of root doesn't count
  if (root->right) {
    root->left->parent_branch->length += root->right->parent_branch->length;
    root->right->parent_branch->length = 0;
  }

  // re-compute the leaf names - can't preserve...
  // I could present some, decrease others by one, if helpful...
  vector<node*> leaves = find_leaves(root);
  for(int i=0;i<leaves.size();i++)
    leaves[i]->name = i;

  // Reconstruct everything
  reanalyze();
}


void tree::add_root() {
  assert(not root);
  root = new node;
  order.push_back(root);
  names.push_back(root);
  // No branches
  n_leaves_ = 1;
  root->name = 0;
  root->order = 0;
}

void tree::reroot(int b) {
  assert(root->right->parent_branch->length == 0.0);
  assert(0 <= b and b< n_branches());

  vector<Branch*> intermediate_branches;
  node* n = branches_[b]->child;

  while(n->parent) {
    intermediate_branches.push_back(n->parent_branch);
    n=n->parent;
  }
  intermediate_branches.pop_back();
  
  while(intermediate_branches.size()) {
    Branch* b2 = intermediate_branches.back();
    intermediate_branches.pop_back();
    if (b2->parent == root->left) 
      swap_children(root->name);
    assert(b2->parent == root->right);

    TreeView::exchange_cousins(root->left,b2->child);
  }
  assert(branches_[b]->parent == root);

  reorder();
  compute_ancestors();
  assert(root->right->parent_branch->length == 0.0);
}

// This routine (re)computes everything, starting with
//   o the tree structure at @root
//   o the names of the leaves

void tree::reanalyze() {

  // find the leaves, and count them
  vector<node*> leaves = find_leaves(root);
  n_leaves_ = leaves.size();

  // compute the name hash for just the leaves
  names = vector<node*>(n_leaves_);
  for(int i=0;i<leaves.size();i++)
    names[leaves[i]->name] = leaves[i];

  // compute the order hash
  order = find_nodes(root);
  reorder();


  // compute the names from the order
  names = order;
  for(int i=0;i<names.size();i++)
    names[i]->name = i;

  if (n_nodes()>1) {
    // compute the branches_ hash
    branches_ = vector<Branch*>(names.size()-1,0);
    for(int i=0;i<branches_.size();i++) {
      branches_[i] = names[i]->parent_branch;
      branches_[i]-> name = i;
    }

    // force the highest number branch to the right of root
    if (root->right and
	branches_[branches_.size()-1]->child != root->right) {
      int i = branches_.size()-1;
      int j = root->right->parent_branch->name;
      std::swap(branches_[i]->name,branches_[j]->name);
      std::swap(branches_[i],branches_[j]);
    }
  }

  /***** Check that our lookup tables are right *****/
  assert(names.size() == order.size());
  for(int i=0;i<names.size();i++) {
    assert(names[i]->name == i);
    assert(order[i]->order == i);
    if (i<n_branches())
      assert(branches_[i]->name == i);
  }

  /******* for leaves, branch names and node names should correspond *******/
  if (this->n_leaves()>2)
    for(int i=0;i<this->n_leaves();i++)
      assert(branch_up(i) == i);


  /****** Re-compute ancestor relation *******/
  compute_ancestors();
}

// order shouldn't depend on left/right stuff

/// Compute the order hash into the nodes, and mark nodes w/ their order

/// This routine depends on sanity introduced by previous routine(s)
///   o order contains all the nodes
///   o names hash for leaves is right
void tree::reorder() {

  int old_size = order.size();
  
  // Mark all nodes as non-visted
  for(int i=0;i<order.size();i++) 
    order[i]->order = -1;

  // Get the leaf order from the names
  order.clear();
  for(int i=0;i<n_leaves_;i++) {
    order.push_back(names[i]);
    order[i]->order = i;
  }

  /* Add nodes in order of distance from leaves */
  int start=0;
  do {
    int end = order.size();
    for(int i=start;i<end;i++) {
      node* parent = order[i]->parent;

      // the root doesn't have a parent
      if (not parent) continue;

      // skip if parent already added
      if (parent->order != -1) continue;
	
      // skip if left child not ready
      if (parent->left and parent->left->order == -1)
	continue;

      // skip if right child not ready
      if (parent->right and parent->right->order == -1)
	continue;

      parent->order = order.size();
      order.push_back(parent);
    }
    start = end;
  } while (start < order.size());

  assert(old_size == order.size());
  for(int i=0;i<order.size();i++)
    assert(order[i]->order == i);

}

// names need to be already set up for this
void tree::compute_ancestors() {

  ancestors.clear();
  ancestors.insert(ancestors.begin(),n_nodes(),
		   valarray<bool>(false,n_nodes()));
  for(int i=0;i<n_nodes();i++) {
    int n = get_nth(i);
    ancestors[n][n] = true;

    node* N = names[n];
    assert(N->name == n);

    if (N->parent) 
      ancestors[N->parent->name] |= ancestors[n];
  }

  for(int i=0;i<n_nodes()-1;i++) {
    int c = i;
    int p = names[c]->parent->name;
    assert(ancestor(p,c));
  }
}

//Unrooted Tree
valarray<bool> tree::partition(int node1,int node2) const {
  node* n1 = names[node1];
  node* n2 = names[node2];

  valarray<bool> mask(n_nodes());;

  if ((n1==n2->parent) or
      (not n1->parent->parent and not n2->parent->parent))
    mask = ancestors[node2];
  else if (n2 == n1->parent)
    mask = !ancestors[node1];
  else // the two nodes are connected!
    std::abort();
    
  return mask;
}

branchview tree::branch_up(int node1) {
  node* n1 = names[node1];
  if (not n1->parent->parent)
    return n1->parent->left->parent_branch;
  else
    return n1->parent_branch;
}

const_branchview tree::branch_up(int node1) const {
  node* n1 = names[node1];
  if (not n1->parent->parent)
    return n1->parent->left->parent_branch;
  else
    return n1->parent_branch;
}


// ***** "exchange_cousins" and "exchange": *****
// We exchange the subtrees specified by node1 and node2
// o The subtrees are specified by only one node - the direction
//    is away from the other node
// o Assert that the subtrees are disjoint:
//    they must have at least 1 node between them
//
//    - if the neither node is an ancestor of the other, then they have a 
//      common ancestor between them, so that is OK.
//    - otherwise, assert that they do.
//    - if they have only one node between them, do nothing.
// o Branches move along with the subtree - so we move branch lengths
//    - p1->n1:l1, p1->n2:l2  becomes p1->n2:l2,p2->n1:l1
//    - this is easy in exchange_cousins, but takes works after reorder


// if one of the subtrees contains the 
// branch connect the children of the root then
// we have to take special action in fixing up branch lengths

// if parent(node1) == parent(node2), which can happen at the top
// of the tree when p1 != p2, then 

void tree::do_swap(node* c1,node* c2) {
  node* n1 = c1->parent;
  node* n2 = c2->parent;

  assert(n2->order > n1->order);

  Branch* b1 = n1->parent_branch;
  Branch* b2 = n2->left->parent_branch;
  if (c2 == n2->left)
    b2 = n2->right->parent_branch;

  // swap the names for n1 and n2
  std::swap(names[n1->name],names[n2->name]);
  std::swap(n1->name,n2->name);

  // swap the branch names and lengths for b1 and b2
  std::swap(branches_[b1->name],branches_[b2->name]);
  std::swap(b1->name,b2->name);
  std::swap(b1->length,b2->length);

  // swap the subtrees c1 and c2
  TreeView::exchange_cousins(c1,c2);
}


void tree::exchange(int node1,int node2) {
  if (ancestor(node1,node2))
    std::swap(node1,node2);

  node* n1 = names[node1];
  node* n2 = names[node2];

  // Always want to deal with LEFT branch under root
  if (n1 == root->right or n2 == root->right) 
    swap_children(root->name);

  if (n1->parent == n2->parent) 
    std::abort();                     // this is like swap_children
  else if (n1->parent == root and n2->parent->parent == root)
    std::abort();                     // this is a topology change
  else if (n2->parent == root and n1->parent->parent == root)
    std::abort();                     // this is a topology change
  else if (ancestor(node2,node1)) {

    vector<node*> intermediate;
    node* here = n1->parent;

    /****** Find the intermediate children *******/
    assert(here != n2);
    do {
      node* there = here->left;
      if (ancestor(there->name,node1))
	  there = here->right;
      intermediate.push_back(there);
      here = here->parent;
    }
    while(here != n2);

    /******  Invert the order of the intermediate nodes ****/
    for(int i=0;i<intermediate.size()/2;i++) {
      int j = intermediate.size()-1-i;
      node* nodei = intermediate[i];
      node* nodej = intermediate[j];
      do_swap(nodei,nodej);
    }
  }
  else     
    TreeView::exchange_cousins(n1,n2);

  // doesn't mess with the names
  reorder();

  /***** Check that our lookup tables are right *****/
  assert(names.size() == order.size());
  for(int i=0;i<names.size();i++) {
    assert(names[i]->name == i);
    assert(order[i]->order == i);
    if (i<n_branches())
      assert(branches_[i]->name == i);
  }

  /******* for leaves, branch names and node names should correspond *******/
  if (this->n_leaves()>2)
    for(int i=0;i<this->n_leaves();i++)
      assert(branch_up(i) == i);


  compute_ancestors();

}

void tree::SPR(int n1, int n2, int b) {
  assert(root->right->parent_branch->length == 0.0);

  // don't regraft to the sub-branches we are being pruned from
  assert(branch(b).parent() != n1 and branch(b).child() != n1);

  // reroot so that n1->n2 points down
  if (not ancestor(n1,n2)) {
    valarray<bool> subtree_nodes = partition(n1,n2);
    int chosen_branch = -1;
    for(int i=0;i<n_branches();i++)
      if (not subtree_nodes[ branch(i).parent() ] or
	  not subtree_nodes[ branch(i).child() ] ) {
	chosen_branch = i;
	break;
      }
	
    assert(chosen_branch >= 0);
    reroot(b);
  }
  assert(ancestor(n1,n2));
    
  // cut out n2, fix up the length
  ::remove_subtree(names[n2],root);

  // fix up lengths of hacked branch
  assert(root->left and root->right);
  if (root->right->parent_branch->length != 0) {
    root->left->parent_branch->length += root->right->parent_branch->length;
    root->right->parent_branch->length = 0;
  }

  // fix up naming so that root->right->parent_branch->name == n_branches()
  if (root->right->parent_branch->name != n_branches()) {
    int b1 = root->right->parent_branch->name;
    int b2 = n_branches();
    std::swap(branches_[b1],branches_[b2]);
    branches_[b1]->name = b1;
    branches_[b2]->name = b2;
  }


  // re-insert n1->n2
  insert_subtree(names[n2],names[ branch(b).child() ] );

  // recompute ordering, and ancestors
  reorder();
  compute_ancestors();

  assert(root->right->parent_branch->length == 0.0);
}


tree& tree::operator=(const tree& t1) {
  TreeView(root).destroy();

  n_leaves_ = t1.n_leaves();
  root = t1.copy();
  vector<node*> nodes;
  if (root)
    nodes = find_nodes(root);
  order = nodes;
  names = nodes;
  
  // Reconstruct the 'order' and 'names' hashes
  if (nodes.size() > 1) {
    for(int i=0;i<nodes.size();i++) {
      int o = nodes[i]->order;
      int n = nodes[i]->name;
      assert(0 <= n and n < nodes.size());
      assert(0 <= o and o < nodes.size());

      names[n] = nodes[i];
      order[o] = nodes[i];
    }

    // index numbers shouldn't change across copies
    branches_ = vector<Branch*>(names.size()-1,0);
    for(int i=0;i<branches_.size();i++) {
      int name = names[i]->parent_branch->name;
      branches_[name] = names[i]->parent_branch;
    }
  }

  compute_ancestors();

  return *this;
}


tree::tree(const tree& t1, const tree& t2) {
  root = new node;            // *here = new node;
  root->parent = 0;
  n_leaves_ = 0;

  add_left(root,t1);
  add_right(root,t2);

  root->left->parent_branch->length = 0.2;
  root->right->parent_branch->length = 0.2;
}

tree::tree(const tree& t1, double b1, const tree& t2, double b2) {
  root = new node;            // *here = new node;
  root->parent = 0;
  n_leaves_ = 0;

  add_left(root,t1);
  add_right(root,t2);
  
  root->left->parent_branch->length = b1;
  root->right->parent_branch->length = b2;
}


TreeFunc<int> mark_tree(const vector<int>& present_leaf,const tree& T) {
  //Step 0: Set all nodes as not-considered
  TreeFunc<int> present(T,-1);

  //Step 1: Load leaf information
  for(int i=0;i<present_leaf.size();i++) 
    present(i) = present_leaf[i];

  //Step 2: Connect the cluster of 'present' nodes
  int top = -1;
  for(int i=0;i<present_leaf.size();i++) {
    if (present(i) != 1) continue;

    if (top == -1)
      top = i;
    else {
      int here=i;
      while(not T.ancestor(here,top)) {
	here = T[here].parent();
	present(here) = 1;
      }
      int parent = here;
      
      here = top;
      while(here != parent) {
	here = T[here].parent();
	present(here) = 1;
      }
      
      if (parent>top) top=parent;
    }
  }
  
  assert(top != -1); //at least one node must be present

  // Step 3: connect the 'missing' nodes to the cluster of 'present' nodes
  for(int i=0;i<present_leaf.size();i++) {
    if (present_leaf[i] != 0) continue;

    int here=i;
    int parent;
    while(not T.ancestor(here,top)) {
      here = T[here].parent();
      if (present(here) != -1)
	goto done;
      present(here) = 0;
    }
    parent = here;
    
    here = top;
    while(here != parent) {
      here = T[here].parent();
      present(here) = 0;
    }
  done: continue;
  }

  return present;
}



void tree::swap_children(int n) {
  node* p = names[n];

  // make sure that the left branch stays left
  if (n == n_nodes()-1) {
    std::swap(p->left->parent_branch->child,p->right->parent_branch->child);
    std::swap(p->left->parent_branch,p->right->parent_branch);
  }

  std::swap(p->left,p->right);
} 


// recompute everything from new leaf names

vector<int> tree::standardize(bool do_reroot) {
  vector<int> lnames(n_leaves());
  for(int i=0;i<lnames.size();i++)
    lnames[i] = i;
  return standardize(lnames,do_reroot);
}

  
vector<int> tree::standardize(const vector<int>& lnames,bool do_reroot) {

  /********** Set the leaf names ***********/
  assert(lnames.size() == n_leaves());

  vector<node*> old_names = names;

  /********** Change the topology **********/
  int newroot=-1;
  
  for(int i=0;i<n_leaves();i++)
    if (lnames[i] == 0) {
      newroot = i;
      break;
    }

  // newroot is actually a branch...
  if (n_leaves() == 2)
    newroot=0;
  if (do_reroot)
    reroot(newroot);

  /********** Input new leaf order *************/
  for(int i=0;i<n_leaves();i++)
    names[i]->name = lnames[i];

  /********* recompute everything ************/
  reanalyze();

  /******** Set the left/right order ********/
  for(int i=n_leaves();i<n_nodes();i++) {
    if (names[i]->left->order > names[i]->right->order)
      swap_children(i);
  }

  vector<int> mapping(old_names.size());
  for(int i=0;i<mapping.size();i++)
    mapping[i] = old_names[i]->name;

  return mapping;
}

int tree::common_ancestor(int i,int j) const {
  assert(0 <= i and i < n_nodes()); 
  assert(0 <= j and j < n_nodes()); 

  node* ni = names[i];
  while (not ancestors[ni->name][j])
    ni = ni->parent;

  return ni->name;
}


double tree::distance2(int i,int j) const {
  assert(ancestors[j][i]);

  double d=0;
  node* ni = names[i];
  node* nj = names[j];
  while(ni != nj) {
    d += ni->parent_branch->length;
    ni = ni->parent;
  }

  return d;
}

void tree::remove_subtree(int i) {
  assert(0 <= i and i < names.size());
  remove_subtree(names[i]);
}

double tree::distance(int i,int j) const {
  int ancestor = common_ancestor(i,j);

  return distance2(i,ancestor)+distance2(j,ancestor);
}

int tree::find_branch(int node1,int node2) const {
  for(int b=0;b<n_branches();b++) {
    if (branch(b).child() == node1 and branch(b).parent() == node2) return b;
    if (branch(b).child() == node2 and branch(b).parent() == node1) return b;
  }
  std::abort();
}

bool same(const valarray<bool>& vb1,const valarray<bool>& vb2) {
  assert(vb1.size() == vb2.size());
  for(int i=0;i<vb1.size();i++)
    if (vb1[i] != vb2[i])
      return false;
  return true;
}

int find_partition(const valarray<bool>& p1, const vector<valarray<bool> >& pv) {
  valarray<bool> np1 = not p1;
  for(int i=0;i<pv.size();i++) {
    if (same(pv[i],p1) or same(pv[i],np1))
      return i;
  }
  return -1;
}

double distance1(const tree& T1, const tree& T2) {
  assert(T1.n_branches() == T2.n_branches());

  vector<double> d1(T1.n_branches());
  vector< valarray<bool> > part1(T1.n_branches(),valarray<bool>(false,T1.n_nodes()));

  vector<double> d2(T2.n_branches());
  vector< valarray<bool> > part2(T2.n_branches(),valarray<bool>(false,T2.n_nodes()));

  // get partitions and lengths for T1
  for(int b=0;b<T1.n_branches();b++) {
    d1[b] = T1.branch(b).length();
    int parent = T1.branch(b).parent();
    int child = T1.branch(b).child();
    part1[b] = T1.partition(parent,child);
  }

  // get partitions and lengths for T2
  for(int b=0;b<T2.n_branches();b++) {
    d2[b] = T2.branch(b).length();
    int parent = T2.branch(b).parent();
    int child = T2.branch(b).child();
    part2[b] = T2.partition(parent,child);
  }

  // Accumulate distances for T1 partitions
  double total=0;
  for(int i=0;i<part1.size();i++) {
    int found = find_partition(part1[i],part2);
    if (found == -1)
      total += std::abs(d1[i]);
    else {
      total += std::abs(d1[i] - d2[found]);
    }
  }

  // Accumulate distances for T2 partitions
  for(int i=0;i<part1.size();i++) {
    int found = find_partition(part1[i],part2);
    if (found == -1)
      total += std::abs(d1[i]);
    else
      ; // this is already counted in the previous loop
  }
  return total;
}
