#include "tree.H"
#include "exponential.H"
#include <algorithm>
#include <sstream>
#include "myexception.H"

using std::vector;
using std::valarray;

void TreeView::destroy_tree(BranchNode* start) {
  assert(start);

  vector<BranchNode*> nodes;
  nodes.reserve(20);

  for(BN_iterator BN(start);BN;BN++) 
    nodes.push_back(*BN);

  for(int i=0;i<nodes.size();i++)
    delete nodes[i];
}

BranchNode* TreeView::copy_node(BranchNode* start) {

  BranchNode* n1 = start;

  BranchNode* start2 = new BranchNode(n1->branch,n1->node);
  BranchNode* n2 = start2;

  do {

    n1 = n1->next;

    if (n1 == start)
      n2->next = start2;
    else
      n2->next = new BranchNode(n1->branch,n1->node);

    n2->next->prev = n2;

    n2 = n2->next;
  } while(n1 != start);

  return n2;
}


BranchNode* TreeView::copy_tree(BranchNode* start) {
  BranchNode* here1 = start;
  BranchNode* start2 = copy_node(start);
  BranchNode* here2 = start2;
  
  BranchNode* prev = NULL;

  
  for(;here1!=start or prev==NULL;) {
    
    // If we jump out to another node, then create it if its not there
    if (prev == here1->prev or here1->next == here1) {
      prev  = here1;
      here1 = here1->out;

      if (not here2->out) {
	here2->out = copy_node(here1);
	here2->out->out = here2;
      }
      
      here2 = here2->out;
    }

    // Otherwise just move around the current node together
    else {
      prev  = here1;
      here1 = here1->next;
      here2 = here2->next;
    }
  }

  return start2;
}

TreeView TreeView::copy() const {
  if (not root)
    return TreeView(0);

  return copy_tree(root);
}

void TreeView::exchange_subtrees(BranchNode* node1, BranchNode* node2) {
  // I should assert that the subtrees are disjoint, somehow...

  // Neither branches comes from a leaf node
  assert(node1->prev != node1);
  assert(node2->prev != node2);

  // The nodes are distinct
  assert(node1 != node2);

  // Switch the nodes that we point to
  std::swap(node1->prev,node2->prev);
  std::swap(node1->next,node2->next);

  // Switch the nodes that point to us
  node1->prev->next = node1;
  node1->next->prev = node1;

  node2->prev->next = node2;
  node2->next->prev = node2;

  // Switch the node name that we are part of
  std::swap(node1->node,node2->node);
}

void TreeView::merge_nodes(BranchNode* n1,BranchNode* n2) {
  std::swap(n1->next,n2->next);
  n1->next->prev = n1;
  n2->next->prev = n2;
}

// this should preserve sub-branch directions

BranchNode* TreeView::create_node_on_branch(BranchNode* b, int  new_branchname) {
  // Create a ring of size 2
  BranchNode* n = new BranchNode;
  n->next = n->prev = new BranchNode;
  n->next->prev = n->prev->next = n;

  // Link from ring to branch endpoints - duplicate branch names
  n->out = b;
  n->branch = b->out->branch;

  n->next->out = b->out;
  n->next->branch = b->branch;

  // Link from branch endpoints to the ring
  b->out->out = n->next;
  b->out = n;

  int delta = std::abs(b->branch - b->out->branch);

  // choose sub-branch to give the new name to
  if (b->node < n->next->out->node)
    b = n->next->out;

  // determine sub-branch direction;
  if (b->branch > b->out->branch)
    b = b->out;

  b->branch = new_branchname;
  b->out->branch = new_branchname + delta;

  return n;
}

int TreeView::remove_node_from_branch(BranchNode* n) {
  if (n->next->next != n)
    std::abort();

  //----- project down branch names, get delta -----//
  BranchNode* b1 = n->out;
  BranchNode* b2 = n->next->out;

  int delta = std::abs(b1->branch - b1->out->branch);

  b1->branch = std::min(b1->branch,b1->out->branch);
  b2->branch = std::min(b2->branch,b2->out->branch);

  //---------- choose the branch to keep -----------//
  if (b1->branch > b2->branch)
    std::swap(b1,b2);

  int dead_branch = b2->branch;
  b2->branch = b1->branch + delta;

  //-------- unlink the node and delete it ---------//
  b1->out = b2;
  b2->out = b1;

  delete n->next;
  delete n;

  return dead_branch;
}

BranchNode* TreeView::unlink_subtree(BranchNode* b) {
  BranchNode* prev = b->prev;

  // disconnect from tree
  b->prev->next = b->next;
  b->next->prev = b->prev;

  // re-link this node as a leaf node
  b->prev = b->next = b;

  if (prev == b)
    return NULL;
  else
    return prev;
}


//NOTE - this leaves branch names for the new branches in an inconsistent state
//void TreeView::graft_subtree(BranchNode* b1,BranchNode* b2) {
//  create_node_on_branch(b2);
//  merge_nodes(b2->out,b1);
//}

vector<BranchNode*> find_leaves(BranchNode* start) {
  vector<BranchNode*> leaves;

  for(BN_iterator BN(start);BN;BN++) 
    if (is_leaf_node(*BN))
	leaves.push_back(*BN);

  return leaves;
}

//------------------------ Begin definition of Tree::* routines ------------------------//

void name_node(BranchNode* start,int i) {
  BranchNode* n = start;

  do {
    n->node = i;
    n = n->next;
  } while (n != start);
}

vector<int> Tree::standardize() {
  vector<int> lnames(n_leaves());
  for(int i=0;i<lnames.size();i++)
    lnames[i] = i;
  return standardize(lnames);
}

vector<int> Tree::standardize(const vector<int>& lnames) {

  //----------- Set the leaf names ------------//
  assert(lnames.size() == n_leaves());

  vector<BranchNode*> old_nodes = nodes_;

  //---------- Input new leaf order -----------//
  for(int i=0;i<n_leaves();i++)
    nodes_[i]->node = lnames[i];

  //---------- recompute everything -----------//

  //FIXME - write some code to determine the order of prev/next


  reanalyze(nodes_[0]);
  

  //------------- compute mapping -------------//
  vector<int> mapping(old_nodes.size());
  for(int i=0;i<mapping.size();i++)
    mapping[i] = old_nodes[i]->node;

  return mapping;
}

vector<BranchNode*> index_branches(BranchNode* start,int N) {
  vector<BranchNode*> branches(N,(BranchNode*)NULL);
  
  for(BN_iterator BN(start);BN;BN++) {
    if ((*BN)->branch < branches.size())
      branches[ (*BN)->branch ] = (*BN);
  }
  return branches;
}

void Tree::restore_branch_lengths(const vector<BranchNode*>& branches,const vector<double>& old_lengths) {
  for(int old_name=0;old_name<branches.size();old_name++) {
    BranchNode* b = branches_[old_name];
    if (not b) continue;

    int new_name = std::min(b->branch,b->out->branch);

    lengths[new_name] = old_lengths[old_name];
  }
}

/// Give leaf nodes their new names
void shift_leaves(BranchNode* start,int first,int n) {
  for(BN_iterator BN(start);BN;BN++) {
    if (not is_leaf_node(*BN))
      continue;

    if ((*BN)->node >= n)
      ((*BN)->node)--;

    (*BN)->node += first;
  }
}

/// Remove the subtree with root node n
nodeview Tree::prune_subtree(int br) {
  BranchNode* b = branches_[br];
  
  // compute mapping between lengths and surviving branches;
  vector<double> old_lengths = lengths;
  vector<BranchNode*> b_index = index_branches(b,n_branches());
  for(int i=0;i<b_index.size();i++) {
    int node_name = b_index[i]->node;
    if (cached_partitions[br][node_name])
      b_index[i] = NULL;
  }
  b_index[br] = NULL;

  // shift remaining leaf names down so that they remain contiguous
  for(int i=0;i<n_leaves();i++) {

    // skip leaf nodes to be deleted
    if (cached_partitions[br][i]) continue;

    // calculate new name 
    int name = i;
    for(int j=0;j<=i;j++)
      if (cached_partitions[br][j]) name--;

    // assign new name
    name_node(nodes_[i],name);
  }

  // Remove and destroy subtree @ b
  BranchNode* node_remainder = TreeView::unlink_subtree(b);
  TreeView(b).destroy();
  
  /// Reconstruct everything from node names
  reanalyze(node_remainder);

  restore_branch_lengths(b_index,old_lengths);

  return node_remainder;
}


/* Guarantees for names: 
     1. Surviving leaf names will be in same order in each tree.
     2. All the leaf names in tree 1 will be before all the leaf names in tree 2.
     3. The merged node will NOT be a leaf node in the new tree.
   Requirements:
     1. Both trees have at least 1 edge: 
        This ensures that the nodes retain definition.
	If either tree has only one node and no edges then that node simply goes away,
         and guarantee #2 is lost.
    
*/

void Tree::merge_tree(int node, const Tree& T, int tnode) {
  //--- Make new tree structure, w/ correct leaf node names ---//
  BranchNode* n  = nodes_[node];
  BranchNode* tn = T.copy(tnode);

  if (not n->out)
    throw myexception()<<"Trying to merge a tree into a tree w/ only one node: not allowed.";
  if (not tn->out)
    throw myexception()<<"Trying to merge a tree w/ only one node: not allowed.";

  vector<double> old_lengths = lengths;
  vector<BranchNode*> branches_1 = index_branches(n ,   n_branches());
  vector<BranchNode*> branches_2 = index_branches(tn, T.n_branches());

  int nl1 = n_leaves();   if (is_leaf_node(n )) nl1--;

  shift_leaves( n, 0  ,  n->node);
  shift_leaves(tn, nl1, tn->node);

  TreeView::merge_nodes(n,tn);

  //------------------------- Setup ---------------------------//
  assert(not is_leaf_node(n));
  assert(not is_leaf_node(tn));

  reanalyze(nodes_[0]);

  assert(n->node == tn->node);

  //---------------- Restore branch lengths -------------------//
  restore_branch_lengths(branches_1,old_lengths);
  restore_branch_lengths(branches_1,T.branch_lengths());
}

double Tree::distance(int i,int j) const {
  double d=0;

  BranchNode* b = nodes_[i];

  while (b->node != j) {
    if (subtree_contains(b->branch,j)) {
      d += lengths[std::min(b->branch,b->out->branch)];
      b = b->out;
    }
    else 
      b = b->next;
  }
  return d;
}

void Tree::add_first_node() {
  if (nodes_.size())
    throw myexception()<<"Trying to add first node to tree which is not empty";

  BranchNode* BN = new BranchNode(0,0);
  BN->prev = BN->next = BN;

  nodes_.push_back(BN);
  branches_.push_back(BN);

  n_leaves_ = 1;
}

nodeview Tree::add_node(int node) {
  assert(0 <= node and node < nodes_.size());

  // Add a node to the ring at node 'node'.
  BranchNode* n = nodes_[node];

  // The spot to which to link the new node.
  BranchNode* n_link = NULL;
  if (n->out) {
    n_link = new BranchNode(-1,n->node);
    n_link->prev = n; 
    n_link->next = n->next;

    n_link->prev->next = n_link;
    n_link->next->prev = n_link;
  }
  else
    n_link = n;

  // Add a new leaf node, and an edge to node 'node'
  BranchNode* n_leaf = new BranchNode(-1,n_leaves_);
  n_leaf->prev = n_leaf->next = n_leaf;
  n_leaf->out = n_link;
  n_link->out = n_leaf;

  reanalyze(n_leaf);

  return n_leaf;
}


vector<const_branchview> nodes_away(const Tree& T,nodeview n) {

  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T[n].branches_out(),branch_list);

  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_after(),branch_list);

  std::reverse(branch_list.begin(),branch_list.end());
  return branch_list;
}  

vector<const_branchview> nodes_toward(const Tree& T,nodeview n) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T[n].branches_in(),branch_list);

  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_before(), branch_list);

  std::reverse(branch_list.begin(),branch_list.end());
  return branch_list;
}  

void Tree::compute_partitions() {
  vector<const_branchview> branch_list = nodes_away(*this,nodes_[0]);
  vector<const_branchview> temp; temp.reserve(3);

  cached_partitions = vector< valarray<bool> >(branches_.size(),valarray<bool>(false,n_nodes()));

  for(int i=0;i<branch_list.size();i++) {
    const_branchview b = branch_list[i];

    cached_partitions[b][b.target()] = true;

    if (b.target().is_internal_node()) {
      temp.clear();
      append(b.branches_after(),temp);
      for(int i=0;i<temp.size();i++)
	cached_partitions[b] |= cached_partitions[temp[i]];
    }

    cached_partitions[b.reverse()] = not cached_partitions[b];
  }
}


void Tree::exchange_subtrees(int br1,int br2) {
  BranchNode* b1 = branches_[br1];
  BranchNode* b2 = branches_[br2];

  // FIXME - add more clash conditions
  if (subtree_contains(br1,b2->out->node))
    std::abort();

  if (subtree_contains(br2,b1->out->node))
    std::abort();


  TreeView::exchange_subtrees(b1,b2);

  // doesn't mess with the names
  recompute(nodes_[0]);
}

nodeview Tree::create_node_on_branch(int br) {
  BranchNode* b = branches_[br];

  BranchNode* n = TreeView::create_node_on_branch(b,-1);

  FIXME - reanalyze destroys branch names, and so destroys length information...
  lets put branch information in the tree structure!
  std::abort();

  reanalyze(nodes_[0]);

  return n;
}


void Tree::remove_node_from_branch(int node) {
  BranchNode* n = nodes_[node];

  TreeView::remove_node_from_branch(n);

  reanalyze(nodes_[0]);
}


/// SPR: move the subtree b1 into branch b2
void Tree::SPR(int br1,int br2) {

  BranchNode* b1 = branches_[br1];
  BranchNode* b2 = branches_[br2];

  assert(n_leaves() > 2);

  // don't regraft to the sub-branches we are being pruned from
  assert(b2 != b1->prev and b2 != b1->next);

  //------------ Prune the subtree -----------------//
  BranchNode* newbranch = TreeView::unlink_subtree(b1)->out;
  int dead_branch = TreeView::remove_node_from_branch(newbranch->out);

  // give the length from the dead branch to the surviving branch;
  lengths[newbranch->branch] += lengths[dead_branch];
  lengths[dead_branch] = 0;
  
  //----------- Regraft the subtree ---------------//
  TreeView::create_node_on_branch(b2,dead_branch);
  TreeView::merge_nodes(b1,b2->out);
  name_node(b1,b1->node);

  recompute(b1);
}

//NOTE: both of these routines assume that prev,next, and old pointers are correct

/// This routine assumes only that leaf nodes have proper names and are indexed in nodes_
void Tree::reanalyze(BranchNode* start) {

  nodes_.clear();
  branches_.clear();
  n_leaves_ = 0;
  int total_branch_nodes = 0;

  //------- Initialize leaf nodes && clear other nodes --------//
  for(BN_iterator BN(start);BN;BN++) {
    
    if (is_leaf_node(*BN)) {
      n_leaves_++;
      (*BN)->branch      = (*BN)->node;
    }
    else {
      (*BN)->node = -1;
      (*BN)->branch = -1;
    }
    
    total_branch_nodes++;
  }

  branches_.resize(total_branch_nodes);
  lengths.resize(branches_.size()/2);

  int total_nodes = n_leaves_;
  int total_branches = n_leaves_;

  //---------- Compute node and branch names -----------------//
  for(BN_iterator BN(start);BN;BN++) {
    // name the node 
    if ((*BN)->node == -1) {
      assert(not(is_leaf_node(*BN)));
      name_node(*BN,total_nodes);
      total_nodes++;
    }

    if ((*BN)->branch == -1) {
      if ((*BN)->out->branch == -1) {
	(*BN)->branch = total_branches++;
	(*BN)->out->branch = (*BN)->branch + n_branches();
      }
      else
	(*BN)->branch = (*BN)->out->branch + n_branches();
    }
  }
  
  nodes_.resize(total_nodes);
  
  // give names to nodes and branches
  recompute(start);
}

/// Computes nodes_[] and branch_[] indices, and cached_partitions[]
void Tree::recompute(BranchNode* start,bool recompute_partitions) {

  n_leaves_ = 0;
  for(BN_iterator BN(start);BN;BN++) {

    // each leaf node has only one BranchNode, so this works
    if (is_leaf_node(*BN)) n_leaves_++;

    // point the tree structure to this tree object
    (*BN)->tree = this;

    //construct the nodes_ index
    nodes_[(*BN)->node] = *BN;

    //construct the branches_ index
    branches_[(*BN)->branch] = *BN;
  }
  
  check_structure();

  if (recompute_partitions)
    compute_partitions();
}

void Tree::check_structure() const {
#ifndef NDEBUG

  //----- Check that our lookup tables are right ------//
  for(int i=0;i<nodes_.size();i++) {
    BranchNode* BN = nodes_[i];

    //the cache lookup tables for node names must be correct
    assert(BN->node == i);

    //leaf nodes come before internal nodes
    if (i<n_leaves())
      assert(is_leaf_node(BN));
    else
      assert(is_internal_node(BN));

    //each BranchNode in a node must have the same node name
    for(BranchNode* n = BN->next;n != BN;n=n->next)
      assert(n->node == BN->node);

    //this node and its prev and next nodes must link to each other consistently
    assert(BN->prev->next == BN);
    assert(BN->next->prev == BN);
  }

  for(int i=0;i<n_leafbranches();i++) {

    BranchNode* BN = branches_[i];

    //the cached lookup tables must correct refer to the lower-named branch
    if (i < n_branches()) {
      assert(BN->branch == i);
      assert(BN->out->branch == i+n_branches());
      //leaf branches must have the same branch name and node name
      if (is_leaf_branch(BN))
	assert(BN->node == i);
    }
    else {
      assert(BN->out->branch == i);
      assert(BN->branch == i + n_branches());
    }

    //this branch and its reversal must link to each other consistently
    assert(BN->out->out == BN);

    //reversed branch must have a different name
    assert(BN->branch != BN->out->branch);
  }

#endif
}

Tree& Tree::operator=(const Tree& T) {
    assert(&T != this);

    // bail if we are copying the same thing only ourselves
    if (&T == this)
      return *this;

    // destroy old tree structure
    if (nodes_.size()) TreeView(nodes_[0]).destroy();

    n_leaves_ = T.n_leaves_;
    cached_partitions = T.cached_partitions;
    nodes_ = std::vector<BranchNode*>(T.nodes_.size(),(BranchNode*)NULL);
    branches_ = std::vector<BranchNode*>(T.branches_.size(),(BranchNode*)NULL);
    lengths = T.lengths;

    // recalculate pointer indices
    BranchNode* start = T.copy();
    recompute(start,false);

    return *this;
  }

Tree::Tree(const Tree& T) 
    :n_leaves_(T.n_leaves_),
     cached_partitions(T.cached_partitions),
     nodes_(T.nodes_.size(),(BranchNode*)NULL),
     branches_(T.branches_.size(),(BranchNode*)NULL),
     lengths(T.lengths)
{
    // recalculate pointer indices
    BranchNode* start = T.copy();
    recompute(start);

    //operator=(T); 
}

Tree::~Tree() 
{
  for(int i=0;i<branches_.size();i++)
    delete branches_[i];
}

void add_left_right(BranchNode*& top, BranchNode* left,BranchNode* right) {
  BranchNode* n = new BranchNode;

  n->prev = left;
  n->prev->next = n;

  n->next = right;
  n->next->prev = n;

  n->prev->prev = n->next;
  n->next->next = n->prev;

  top->out = n;
  n->out = top;
}


// IDEA - always include root and root->out, but if we don't want a stem
// just make the ring which WOULD contain root->out skip it.  We still have
// pointers INTO the ring from root->out->{prev,next}.

void swap_children(BranchNode* b) {
  // this assumes BINARY trees, where every node has exactly two children
  assert(b->next->next->next == b);

  std::swap(b->prev,b->next);

  b->prev->prev = b->next;
  b->prev->next = b;

  b->next->next = b->prev;
  b->next->prev = b;
} 

vector<int> RootedTree::standardize(const vector<int>& lnames) {
  vector<int> mapping = Tree::standardize(lnames);
  
  // FIXME - move the root node?  Perhaps only unrooted trees need that...

  //------- Set the left/right order -------//
  for(int i=n_leaves();i<n_nodes();i++) {
    if (nodes_[i]->prev->node > nodes_[i]->next->node)
      swap_children(nodes_[i]);
  }

  return mapping;
}

void RootedTree::recompute(BranchNode* start,bool recompute_partitions) {
  Tree::recompute(start,recompute_partitions);

  //  asdf;
  //   std::abort();
  // now make sure that nodes_[i] points to the TOP node in the ring
}

void RootedTree::check_structure() const {
#ifndef NDEBUG
  Tree::check_structure();
  /*
  bool found = false;
  for(int i=0;i<branches_.size() and not found;i++)
    if (root_ == branches_[i])
      found = true;

  if (not nodes_.size())
    assert(root_ == (BranchNode*)NULL);
  else if (not found) {
    throw myexception()<<"RootedTree: root node is none of our nodes!";
    assert(found);
  }
  */
#endif
}


nodeview RootedTree::prune_subtree(int br) {
  if (cached_partitions[br][root_->node])
    throw myexception()<<"Can't deleted a subtree containing the root node!";

  return Tree::prune_subtree(br);
}


BranchNode* gen_root() {
  BranchNode* root = new BranchNode;
  root->prev = root->next = root;

  root->out = new BranchNode;
  root->out->prev = root->out->next = root->out;
  root->out->out = root;
  
  return root;
}

void RootedTree::reroot(int n) {
  assert(0 <= n and n < n_nodes());
  root_ = nodes_[n];
}

int RootedTree::common_ancestor(int i,int j) const {
  assert(0 <= i and i < n_nodes()); 
  assert(0 <= j and j < n_nodes()); 

  BranchNode* BN = root_;
  
  do {
    BN = BN->out;
    while(not subtree_contains(BN->branch,i))
      BN = BN->next;

    assert(subtree_contains(BN->branch,j));
  } while(subtree_contains(BN->branch,j));

  return BN->node;
}

void RootedTree::add_first_node() {
  Tree::add_first_node();
  root_ = nodes_[0];
}

RootedTree& RootedTree::operator=(const RootedTree& RT) {

  Tree::operator=(RT);

  root_ = nodes_[RT.root_->node];

  return *this;
}

RootedTree::RootedTree(const Tree& T,int r)
  :Tree(T),root_(nodes_[r])
{ }

RootedTree::RootedTree(const RootedTree& RT)
  :Tree(RT),root_(nodes_[RT.root_->node])
{ }

RootedTree::RootedTree(const RootedTree& t1, const RootedTree& t2) 
  :Tree(t1),root_(nodes_[t1.root_->node])
{
  merge_tree(root(), t2, t2.root());
}

RootedTree add_root(Tree T,int b) {
  T.create_node_on_branch(b);
  return RootedTree(T,T.n_nodes()-1);
}

Tree remove_root(const RootedTree& RT) {
  Tree T = RT;
  T.remove_node_from_branch(RT.root());
  return T;
}

