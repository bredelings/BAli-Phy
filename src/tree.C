#include "tree.H"
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

BranchNode* TreeView::copy_node(const BranchNode* start) {

  const BranchNode* n1 = start;

  BranchNode* start2 = new BranchNode(n1->branch,n1->node,n1->length);
  BranchNode* n2 = start2;

  if (start->out == start)
    start2->out = start2;

  do {

    n1 = n1->next;

    if (n1 == start)
      n2->next = start2;
    else
      n2->next = new BranchNode(n1->branch,n1->node,n1->length);

    n2->next->prev = n2;

    n2 = n2->next;
  } while(n1 != start);

  return n2;
}


BranchNode* TreeView::copy_tree(const BranchNode* start) {

  const BranchNode* here1 = start;
  BranchNode* start2 = copy_node(start);
  BranchNode* here2 = start2;
  
  do {
    
    // If we jump out to another node, then create it if its not there
    if (!here2->out) {
      here2->out = copy_node(here1->out);
      here2->out->out = here2;
    }

    here1 = here1->out->next;
    here2 = here2->out->next;
    
  } while(here1 != start);

  assert(here1 == start);
  assert(here2 == start2);

  return start2;
}

TreeView TreeView::copy() const {
  if (not root)
    return TreeView(0);

  return copy_tree(root);
}

void TreeView::exchange_subtrees(BranchNode* n1, BranchNode* n2) {
  // I should assert that the subtrees are disjoint, somehow...

  // Neither branches comes from a leaf node
  assert(not is_leaf_node(n1));
  assert(not is_leaf_node(n2));

  // The nodes are distinct
  assert(n1 != n2);

  // Switch the nodes that we point to
  std::swap(n1->prev,n2->prev);
  std::swap(n1->next,n2->next);

  // Switch the nodes that point to us
  n1->prev->next = n1;
  n1->next->prev = n1;

  n2->prev->next = n2;
  n2->next->prev = n2;

  // Switch the node name that we are part of
  std::swap(n1->node,n2->node);
}

void TreeView::merge_nodes(BranchNode* n1,BranchNode* n2) {
  std::swap(n1->next,n2->next);
  n1->next->prev = n1;
  n2->next->prev = n2;
}

// this preserves sub-branch directions
// the node with the smaller name maintains the name of its attatched branch

BranchNode* TreeView::create_node_on_branch(BranchNode* b1, int  new_branchname) {

  BranchNode* b2 = b1->out;

  // Create a ring of size 2 - duplicate branch names and lengths
  BranchNode* n1 = new BranchNode(b2->branch,-1,b2->length);
  BranchNode* n2 = new BranchNode(b1->branch,-1,b1->length);
  n1->next = n1->prev = n2;
  n2->next = n2->prev = n1;

  // Link from ring to branch endpoints
  n1->out = b1;
  b1->out = n1;

  n2->out = b2;
  b2->out = n2;

  int delta = std::abs(b2->branch - b1->branch);

  // choose sub-branch to give the new name to
  if (b1->node > b2->node)
    std::swap(b1,b2);

  // determine sub-branch direction;
  if (b2->branch > b2->out->branch)
    b2 = b2->out;

  // set new branch name and set length to 0
  b2->branch = new_branchname;
  b2->out->branch = new_branchname + delta;
  b2->length = b2->out->length = 0;

  return n1;
}

/// Merge sub-branches, adding their lengths, and reporting which branch name didn't survive.
int TreeView::remove_node_from_branch(BranchNode* n1) {
  BranchNode* n2 = n1->next;
  assert(n2->next == n1);

  BranchNode* b1 = n1->out;
  BranchNode* b2 = n2->out;

  if (b1->node > b2->node) {
    std::swap(n1,n2);
    std::swap(b1,b2);
  }

  //---------- get delta - and check it ------------//
  assert(std::abs(b1->branch - b1->out->branch) == std::abs(b2->branch - b2->out->branch));
  int dead_branch_name = std::min(b2->branch,b2->out->branch);

  //-- Connect branches, merge lengths, use new name --//
  b1->out = b2;
  b2->out = b1;

  b2->branch = n1->branch;
  b1->length += b2->length;
  b2->length = b1->length;

  //-------- Remove the node, and reconnect --------//
  delete n1;
  delete n2;

  return dead_branch_name;
}

BranchNode* TreeView::unlink_subtree(BranchNode* b) {
  if (not is_leaf_node(b)) {
    BranchNode* prev = b->prev;

    // disconnect from tree
    b->prev->next = b->next;
    b->next->prev = b->prev;

    // re-link this node as a leaf node
    b->prev = b->next = b;

    return prev;
  }
  else {
    BranchNode* copy = new BranchNode;
    copy->prev = copy->next = copy->out = copy;
    copy->node = b->node;
    copy->branch = b->branch;

    return copy;
  }
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

  vector<BranchNode*> old_nodes = nodes_;

  //----------- Set the leaf names ------------//
  assert(lnames.size() == n_leaves());
  for(int i=0;i<n_leaves();i++)
    nodes_[i]->node = lnames[i];

  //---------- recompute everything -----------//
  reanalyze(nodes_[0]);

  //------------- compute mapping -------------//
  vector<int> mapping(old_nodes.size());
  for(int i=0;i<mapping.size();i++)
    mapping[i] = old_nodes[i]->node;

  return mapping;
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
  
  prepare_partitions();

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

  return node_remainder;
}

/// Remove the subtree with root node n
vector<int> Tree::prune_leaves(const vector<int>& remove) 
{
  // get pointers to the current leaves
  vector<BranchNode*> leaves(n_leaves());
  for(int i=0;i<leaves.size();i++)
    leaves[i] = nodes_[i];

  // get mask of leaves to remove
  vector<int> do_remove(n_leaves(),false);
  for(int i=0;i<remove.size();i++)
    do_remove[remove[i]] = true;

  // remove some leaves and shift others down
  int shift = 0;
  BranchNode* node_remainder = NULL;
  for(int i=0;i<leaves.size();i++)
  {
    if (do_remove[i]) {
      shift++;

      BranchNode* leaf = leaves[i];
      while(is_leaf_node(leaf) and leaf->out and leaf->out != leaf) {
	BranchNode* parent = TreeView::unlink_subtree(leaf->out);
	TreeView(leaf).destroy();
	leaf = parent;
      }

      if (leaf->next != leaf and leaf->next->next == leaf)
	TreeView::remove_node_from_branch(leaf);
    }
    else {
      name_node(leaves[i],i-shift);
      node_remainder = leaves[i];
    }
  }
  
  /// Reconstruct everything from node names
  reanalyze(node_remainder);

  vector<int> mapping(leaves.size() - shift);
  for(int i=0;i<leaves.size();i++)
    if (not do_remove[i])
      mapping[leaves[i]->node] = i;

  return mapping;
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

  int nl1 = n_leaves();   if (is_leaf_node(n )) nl1--;

  shift_leaves( n, 0  ,  n->node);
  shift_leaves(tn, nl1, tn->node);

  TreeView::merge_nodes(n,tn);

  //------------------------- Setup ---------------------------//
  assert(not is_leaf_node(n));
  assert(not is_leaf_node(tn));

  reanalyze(nodes_[0]);

  assert(n->node == tn->node);
}

double Tree::distance(int i,int j) const {
  double d=0;

  BranchNode* b = nodes_[i];

  while (b->node != j) {
    if (subtree_contains(b->branch,j)) {
      d += b->length;
      b = b->out;
    }
    else 
      b = b->next;
  }
  return d;
}

int Tree::edges_distance(int i,int j) const {
  int d=0;

  BranchNode* b = nodes_[i];

  while (b->node != j) {
    if (subtree_contains(b->branch,j)) {
      d++;
      b = b->out;
    }
    else 
      b = b->next;
  }
  return d;
}

BranchNode* get_first_node() {
  BranchNode* BN = new BranchNode(0,0,-1);
  BN->prev = BN->next = BN;
  BN->out = BN;

  return BN;
}

void Tree::add_first_node() {
  if (nodes_.size())
    throw myexception()<<"Trying to add first node to tree which is not empty";

  BranchNode* BN = get_first_node();

  nodes_.push_back(BN);
  branches_.push_back(BN);

  n_leaves_ = 1;
}

BranchNode* add_node(BranchNode* n) 
{
  // The spot to which to link the new node.
  BranchNode* n_link = NULL;
  if (n->out == n)
    n_link = n;
  else {
    n_link = new BranchNode(-1,n->node,-1);
    n_link->prev = n; 
    n_link->next = n->next;

    n_link->prev->next = n_link;
    n_link->next->prev = n_link;
  }

  // Add a new leaf node, and an edge to node 'node'
  BranchNode* n_leaf = new BranchNode;
  n_leaf->prev = n_leaf->next = n_leaf;
  n_leaf->out = n_link;
  n_link->out = n_leaf;

  return n_leaf;
}


nodeview Tree::add_node(int node) {
  assert(0 <= node and node < nodes_.size());

  BranchNode* n_leaf = ::add_node(nodes_[node]);
  n_leaf->node = n_leaves_;

  reanalyze(n_leaf);

  return n_leaf;
}


void get_branches_before(vector<const_branchview>& branch_list) {
  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_before(),branch_list);
}


void get_branches_after(vector<const_branchview>& branch_list) {
  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_after(),branch_list);
}

vector<const_branchview> branches_before(const Tree& T,int b) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  branch_list.push_back(T.directed_branch(b));
  get_branches_before(branch_list);

  return branch_list;
}


vector<const_branchview> branches_after(const Tree& T,int b) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  branch_list.push_back(T.directed_branch(b));
  get_branches_after(branch_list);

  return branch_list;
}

vector<const_branchview> branches_from_node(const Tree& T,int n) {

  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T[n].branches_out(),branch_list);

  get_branches_after(branch_list);

  std::reverse(branch_list.begin(),branch_list.end());
  return branch_list;
}  

vector<const_branchview> branches_toward_node(const Tree& T,int n) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T[n].branches_in(),branch_list);

  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_before(), branch_list);

  std::reverse(branch_list.begin(),branch_list.end());
  return branch_list;
}  

vector<const_branchview> branches_from_leaves(const Tree& T) 
{
  vector<const_branchview> branch_list;
  branch_list.reserve(2*T.n_branches());
  vector<bool> visited(2*T.n_branches(),false);

  for(int i=0;i<T.n_leaves();i++) {
    branch_list.push_back(T.branch(i));
    visited[i] = true;
  }

  for(int i=0;i<branch_list.size();i++) 
  {
    // because we are on the list, we are 'visited'
    assert(visited[branch_list[i]]);

    // check branches-after to see if any are ready
    for(const_edges_after_iterator j = branch_list[i].branches_after();j;j++) 
    {
      // if we are already valid, then ignore
      if (visited[*j]) continue;

      // check if all branches-before are valid
      bool ready = true;
      for(const_edges_before_iterator k = (*j).branches_before();k;k++)
	if (not visited[*k]) ready = false;

      // if so, then 
      if (ready) {
	branch_list.push_back(*j);
	visited[*j] = true;
      }
    }
  }
  assert(branch_list.size() == 2*T.n_branches());

  return branch_list;
}

void Tree::compute_partitions() const {
  vector<const_branchview> branch_list = branches_from_node(*this,nodes_[0]->node);

  // set up cached partition masks
  cached_partitions.resize(2*n_branches());
  for(int i=0;i<cached_partitions.size();i++)
    if (cached_partitions[i].size() != n_nodes())
      cached_partitions[i].resize(n_nodes());

  // compute partition masks
  for(int i=0;i<branch_list.size();i++) {
    const_branchview b = branch_list[i];

    if (b.target().is_leaf_node())
      cached_partitions[b] = false;
    else {
      const_edges_after_iterator j = b.branches_after();

      cached_partitions[b] = cached_partitions[*j];j++;
      for(;j;j++)
	cached_partitions[b] |= cached_partitions[*j];
    }

    cached_partitions[b][b.target()] = true;

    cached_partitions[b.reverse()] = not cached_partitions[b]; 
  }

  caches_valid = true;
}

void exchange_subtrees(Tree& T, int br1, int br2) 
{
  BranchNode* n0 = (BranchNode*)T[0];

  BranchNode* b1 = (BranchNode*)T.branch(br1);
  BranchNode* b2 = (BranchNode*)T.branch(br2);

  assert(not T.subtree_contains(br1,b2->out->node));
  assert(not T.subtree_contains(br2,b1->out->node));

  TreeView::exchange_subtrees(b1,b2);

  // don't mess with the names
  T.recompute(n0);
}

nodeview Tree::create_node_on_branch(int br) 
{
  BranchNode* b = branches_[br];

  BranchNode* n = TreeView::create_node_on_branch(b,-1);

  //FIXME - reanalyze destroys branch names, and so destroys length information...
  //  lets put branch information in the tree structure!
  //  std::abort();

  reanalyze(nodes_[0]);

  return n;
}


void remove_sub_branches(Tree& T)
{
  while(true) 
  {
    // find first node of degree 2
    int n=0;
    while(n<T.n_nodes() and T[n].degree() != 2)
      n++;

    // if no nodes of degree 2, we are done
    if (n == T.n_nodes())
      return;

    // remove the first node of degree 2
    T.remove_node_from_branch(n);
  };
}

void Tree::remove_node_from_branch(int node) 
{
  BranchNode* n = nodes_[node];

  if (nodeview(n).degree() == 2) {
    TreeView::remove_node_from_branch(n);

    reanalyze(nodes_[0]);
  }
}

/// SPR: move the subtree b1 into branch b2
void SPR(Tree& T, int br1,int br2) 
{
  //  T.SPR(br1,br2);
  //  return;

  BranchNode* b1 = (BranchNode*)T.branch(br1);
  BranchNode* b2 = (BranchNode*)T.branch(br2);

  assert(T.n_leaves() > 2);

  // don't regraft to the sub-branches we are being pruned from
  assert(b2 != b1->prev and b2 != b1->next);
  assert(T.partition(b1->out->branch)[b2->node]);
  assert(T.partition(b1->out->branch)[b2->out->node]);

  //------------ Prune the subtree -----------------//
  BranchNode* newbranch = TreeView::unlink_subtree(b1)->out;
  int dead_branch = TreeView::remove_node_from_branch(newbranch->out);
  
  //----------- Regraft the subtree ---------------//
  TreeView::create_node_on_branch(b2,dead_branch);
  TreeView::merge_nodes(b1,b2->out);
  name_node(b1,b1->node);

  T.recompute(b1);
}

/// Return a pointer to the BN in @start which points towards the root
BranchNode* get_parent(BranchNode* start) {
  BranchNode* BN = start;
  BranchNode* first = NULL;

  assert(start->node == -1);

  // find the first non-visited neighbor
  do {
    if (BN->out->node == -1) {
      if (first)
	return NULL;  // >1 unvisited neighbors - let them do it

      first = BN;
    }
    BN = BN->next;
  } while (BN != start);

  if (first)      // point to the unnamed neighbor
    return first; 
  else            // no un-visited neighbors - somebody is already doing this one.
    return NULL;
}

//NOTE: both of these routines assume that prev,next, and old pointers are correct

/// This routine assumes only that leaf nodes have proper names
void Tree::reanalyze(BranchNode* start) {

  nodes_.clear();
  branches_.clear();

  //--------------- Count nodes ---------------//
  n_leaves_ = 0;
  int total_branch_nodes = 0;
  for(BN_iterator BN(start);BN;BN++) {
    total_branch_nodes++;
    if (is_leaf_node(*BN))
      n_leaves_++;
  }
  nodes_.resize(1+total_branch_nodes/2);
  branches_.resize(total_branch_nodes);

  if (n_leaves_ == 1) {
    recompute(start);
    return;
  }

  //----------- Set the leaf names ------------//
  vector<BranchNode*> work(n_leaves());
  for(BN_iterator BN(start);BN;BN++)
    if (is_leaf_node(*BN))
      work[(*BN)->node] = (*BN);

  //------------- Clear all names -------------//
  for(BN_iterator BN(start);BN;BN++) {
    (*BN)->node = -1;
    (*BN)->branch = -1;
  }

  //----------- recompute all names -----------//
  int n=0;
  int b=0;
  const int B = branches_.size()/2;

  while(work.size()) 
  {
    vector<BranchNode*> temp = work;
    work.clear();

    for(int i=0;i<temp.size();i++) {
      // name the node
      name_node(temp[i],n++);

      if (temp[i]->branch == -1) {
	// name the branch out of the node
	temp[i]->branch = b++;
	temp[i]->out->branch = temp[i]->branch + B;

	// add the parent to the list if we are its last child
	BranchNode* next = get_parent(temp[i]->out);
	if (next) work.push_back(next);
      }
    }
  } 
  assert(b == B);
  assert(n == nodes_.size());

  // give names to nodes and branches
  recompute(start);
}

/// Computes nodes_[] and branch_[] indices, and cached_partitions[]
void Tree::recompute(BranchNode* start,bool recompute_partitions) 
{
  n_leaves_ = 0;
  for(BN_iterator BN(start);BN;BN++) {

    // each leaf node has only one BranchNode, so this works
    if (is_leaf_node(*BN)) n_leaves_++;

    //construct the nodes_ index
    nodes_[(*BN)->node] = *BN;

    //construct the branches_ index
    branches_[(*BN)->branch] = *BN;
  }
  
  check_structure();

  if (recompute_partitions)
    caches_valid = false;
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

  for(int i=0;i<branches_.size();i++) {

    BranchNode* BN = branches_[i];

    //the cached lookup tables must correct refer to the lower-named branch
    assert(BN->branch == i);

    if (not n_branches()) continue; 

    //leaf branches must have the same branch name and node name
    if (i < n_leafbranches())
      assert(BN->node == i);

    //reversed branch must have a different name
    assert(std::abs(BN->branch - BN->out->branch) == n_branches());

    //this branch and its reversal must link to each other consistently
    assert(BN->out->out == BN);

    assert(BN->length == BN->out->length);
  }

#endif
}

void knit_node_together(const vector<BranchNode*>& nodes) {
  nodes[0]->prev = nodes.back();
  nodes.back()->next = nodes[0];

  for(int i=0;i<nodes.size()-1;i++) {
    nodes[i]->next = nodes[i+1];
    nodes[i+1]->prev = nodes[i];
  }
  
}

void insert_after(BranchNode* n1,BranchNode* n2)
{
  n2->node = n1->node;

  n2->prev = n1;
  n2->next = n1->next;

  n1->next = n2;
  n2->next->prev = n2;
}

BranchNode* connect_nodes(BranchNode* n1, BranchNode* n2)
{
  BranchNode* c1 = new BranchNode;
  insert_after(n1,c1);

  BranchNode* c2 = new BranchNode;
  insert_after(n2,c2);

  c1->length = c2->length = -1.0;

  c1->out = c2;
  c2->out = c1;

  return c1;
}


BranchNode* split_node(vector<BranchNode*> group1,vector<BranchNode*> group2)
{
  // groups are already split!
  if (group1.size() == 1 or group2.size() == 1) return NULL;

  // separate the two nodes
  knit_node_together(group1);
  knit_node_together(group2);
  
  // connect the two nodes
  return connect_nodes(group1.back(),group2.back());
}

int Tree::induce_partition(const std::valarray<bool>& partition) 
{
  assert(partition.size() == n_leaves());
  
  prepare_partitions();

  valarray<bool> partition1(false,n_nodes());
  valarray<bool> partition2(false,n_nodes());
  for(int i=0;i<partition.size();i++) {
    partition1[i] = partition[i];
    partition2[i] = not partition[i];
  }

  for(int i=n_leaves();i<n_nodes();i++) {
    vector<BranchNode*> group1;
    vector<BranchNode*> group2;

    // divide the branches out into two groups
    BranchNode * BN = nodes_[i];
    do {
      if (empty(partition1 and cached_partitions[BN->branch]))
	group2.push_back(BN);
      else if (empty(partition2 and cached_partitions[BN->branch]))
	group1.push_back(BN);
      else {
	group1.clear();
	group2.clear();
	break;
      }
      
      BN = BN->next;
    } while (BN != nodes_[i]);

    // this node can't separate the groups
    if (not group1.size() and not group2.size()) continue;

    BranchNode* bn = NULL;

    // groups are already split!
    if (group1.size() == 1)
      bn = group1[0];
    // groups are already split!
    else if (group2.size() == 1)
      bn = group2[0];
    // split the node and note the name of the newly added branch
    else {
      bn = split_node(group1,group2);
      reanalyze(nodes_[0]);
    }

    if (not bn)
      return -1;
    else
      return std::min(bn->branch,bn->out->branch);
  }
  throw myexception()<<"induce_partition: partition conflicts with tree!";
}

Tree& Tree::operator=(const Tree& T) 
{
    assert(&T != this);

    // bail if we are copying the same thing only ourselves
    if (&T == this)
      return *this;

    // destroy old tree structure
    if (nodes_.size()) TreeView(nodes_[0]).destroy();

    n_leaves_ = T.n_leaves_;
    caches_valid = T.caches_valid;
    cached_partitions.clear();
    if (caches_valid)
      cached_partitions = T.cached_partitions;
    nodes_ = std::vector<BranchNode*>(T.nodes_.size(),(BranchNode*)NULL);
    branches_ = std::vector<BranchNode*>(T.branches_.size(),(BranchNode*)NULL);

    // recalculate pointer indices
    BranchNode* start = T.copy();
    recompute(start,false);

    return *this;
  }

Tree::Tree(const BranchNode* BN) 
  :caches_valid(false)
{
  reanalyze(TreeView::copy_tree(BN));
}

Tree::Tree(const Tree& T) 
    :caches_valid(T.caches_valid),
     cached_partitions(T.cached_partitions),
     n_leaves_(T.n_leaves_),
     nodes_(T.nodes_.size(),(BranchNode*)NULL),
     branches_(T.branches_.size(),(BranchNode*)NULL)
{
    // recalculate pointer indices
    BranchNode* start = T.copy();
    recompute(start,false);
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


void RootedTree::remove_node_from_branch(int node) 
{
  if (root_->node == node)
    root_ = nodes_[0];

  Tree::remove_node_from_branch(node);
}

nodeview RootedTree::prune_subtree(int br) {
  if (partition(br)[root_->node])
    throw myexception()<<"Can't delete a subtree containing the root node!";

  if (root_ == branches_[br]) 
    root_ = root_->next;

  return Tree::prune_subtree(br);
}

vector<int> RootedTree::prune_leaves(const vector<int>& remove) 
{
  root_ = NULL;

  // if we need to do this, virtualize unlink_subtree to complain if the subtree
  // contains the root.

  return Tree::prune_leaves(remove);
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

RootedTree::RootedTree(const BranchNode* BN)
  :root_(TreeView::copy_tree(BN)) 
{
  reanalyze(root_);
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

bool branch_order(const const_branchview& b1,const const_branchview& b2) {
    return b1.target().name() < b2.target().name();
}

vector<const_branchview> sorted_neighbors(const_nodeview n) {
  vector<const_branchview> branches;
  append(n.branches_out(),branches);

  std::sort(branches.begin(),branches.end(),branch_order);

  return branches;
}


vector<const_branchview> sorted_branches_after(const_branchview b) {
  vector<const_branchview> branches;
  append(b.branches_after(),branches);

  std::sort(branches.begin(),branches.end(),branch_order);

  return branches;
}

int n_elements(const valarray<bool>& v) {
  int count = 0;
  for(int i=0;i<v.size() ;i++)  
    if (v[i]) count++;
  return count;
}

int n_elements(const vector<bool>& v) {
  int count = 0;
  for(int i=0;i<v.size() ;i++)  
    if (v[i]) count++;
  return count;
}


bool empty(const valarray<bool>& v) {
  for(int i=0;i<v.size() ;i++)  
    if (v[i]) return false;
  return true;
}

bool equal(const valarray<bool>& v1,const valarray<bool>& v2) {
  assert(v1.size() == v2.size());
  for(int i=0;i<v1.size();i++)
    if (v1[i] != v2[i]) 
      return false;
  return true;
}

bool intersect(const valarray<bool>& v1,const valarray<bool>& v2) 
{
  assert(v1.size() == v2.size());
  return not empty(v1&v2);
}

bool is_subset(const valarray<bool>& v1,const valarray<bool>& v2) {
  assert(v1.size() == v2.size());
  for(int i=0;i<v1.size();i++)
    if (v1[i] and not v2[i])
      return false;
  return true;
}

Tree star_tree(int n) 
{
  BranchNode* center = get_first_node();
  for(int i=0;i<n;i++)
    add_node(center)->node = i;

  return Tree(center);
}

valarray<bool> branch_partition(const Tree& T,int b) 
{
  valarray<bool> temp = T.partition(b);
  valarray<bool> p(T.n_leaves());
  for(int i=0;i<p.size();i++)
    p[i] = temp[i];

  return p;
}


double length(const Tree& T) {
  double total = 0;
  for(int i=0;i<T.n_branches();i++)
    total += T.branch(i).length();
  return total;
}

#include <iostream>

int subtree_height(const Tree& T,int b) 
{
  int node = T.directed_branch(b).target();

  int depth = 0;
  for(int i=0;i<T.n_leaves();i++) 
    if (T.partition(b)[i])
      depth = std::max(depth, T.edges_distance(node,i) );

  return depth;
}

int node_depth(const Tree& T,int node) 
{
  int depth = 0;
  for(int i=0;i<T.n_leaves();i++) 
    depth = std::min(depth, T.edges_distance(node,i) );

  return depth;
}


vector< vector<int> > partition_sets(const Tree& T)
{
  // set up cached partition sets
  vector< vector<int> > sets(2*T.n_branches());

  for(int b=0;b<2*T.n_branches();b++)
    for(int i=0;i<T.n_leaves();i++) 
      if (T.partition(b)[i]) 
	sets[b].push_back(i);

  return sets;
}


bool is_Cayley(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    int d = T[i].degree();
    if (d != 1 and d != 3)
      return false;
  }
  return true;
}

bool has_sub_branches(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    if (T[i].degree() == 2)
      return true;
  }
  return false;
}

bool has_polytomy(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    if (T[i].degree() > 3)
      return true;
  }
  return false;
}

