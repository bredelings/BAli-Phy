#include "tree.H"
#include "myexception.H"
#include "exponential.H"
#include <algorithm>

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
  if (!root)
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

  // Neither node is a child of the root
  assert(p1->parent and p2->parent);

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


void find_leaves(node* n,vector<node*>& leaves) {
  if (!n)
    return;

  if (leaf(*n)) {
    leaves.push_back(n);
    return;
  }

  find_leaves(n->left,leaves);
  find_leaves(n->right,leaves);
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

// Do I need to export an interface which maps each node name
// on the two old trees into a node name in the current tree?
// I could put a field 'old_name' in the 'struct node' for this...

// subtree(root,tree:left) = T
void tree::add_left(node* parent,const tree& T) {
  node* child = T.copy();
  ::add_left(parent,child);
  reorder();
}

// subtree(root,tree:right) = T
void tree::add_right(node* parent,const tree& T) {
  node* child = T.copy();
  ::add_right(parent,child);
  reorder();
}

void tree::add_root() {
  assert(!root);
  root = new node;
  order.push_back(root);
  names.push_back(root);
  // No branches
  n_leaves = 1;
  root->name = 0;
}

/* This routine computes 'Layer 2' from 'Layer 1'.
 * Layer 1: pointer structure, in memory
 * Layer 2: node names, node orders
 *
 * If the number of nodes remains unchanged, then 
 * the node and branch names remain the same.
 */

void tree::reorder() {
  int old_size = order.size();

  order = find_nodes(root);
  int new_size = order.size();

  // Mark all nodes as non-visted
  for(int i=0;i<order.size();i++)
    order[i]->order = -1;

  // Fill order with the leaves, and order them
  order.clear();
  find_leaves(root,order);
  for(int i=0;i<order.size();i++)
    order[i]->order = i;

  n_leaves = order.size();

  /* Add nodes in order of distance from leaves */
  int start=0;
  do {
    int end = order.size();
    for(int i=start;i<end;i++) {
      node* parent = order[i]->parent;
      if (!parent) continue; // actually this would be a good exit condition

      if (parent->left and parent->right) {  // if there are 2 children
	if (parent->left->order == -1) continue;  // both children 
	if (parent->right->order == -1) continue; // must go on first
	if (order[i]->order < parent->left->order or 
	    order[i]->order < parent->right->order)
	  continue; // and only the second child puts the parent on
      }
      parent->order = order.size();
      order.push_back(parent);
    }
    start = end;
  } while (start < order.size());

  assert(new_size == order.size());
  for(int i=0;i<order.size();i++)
    assert(order[i]->order == i);

  /***** re-assign names if the tree has changed *****/
  if (not names.size() or old_size != new_size) {
    names = order;
    for(int i=0;i<names.size();i++)
      names[i]->name = i;

    branches_ = vector<Branch*>(names.size()-1,0);
    for(int i=0;i<branches_.size();i++) {
      branches_[i] = names[i]->parent_branch;
      branches_[i]-> name = i;
    }
  }

  compute_ancestors();
}

// Names need to be set up already for this
void tree::compute_ancestors() {

  ancestors.clear();
  ancestors.insert(ancestors.begin(),num_nodes(),
		   std::valarray<bool>(false,num_nodes()));
  for(int i=0;i<num_nodes();i++) {
    int n = get_nth(i);
    ancestors[n][n] = true;

    if (names[n]->left) {
      int n_left = order[i]->left->name;
      ancestors[n] |= ancestors[ n_left ];
    }
    
    if (names[n]->right) {
      int n_right = order[i]->right->name;
      ancestors[n] |= ancestors[ n_right ];
    }
  }
}

//Unrooted Tree
std::valarray<bool> tree::partition(int n1,int n2) const {
  std::valarray<bool> mask(num_nodes());;

  if (n1 > n2 or
      (!names[n1]->parent->parent and !names[n2]->parent->parent))
    mask = ancestors[n2];
  else 
    mask = !ancestors[n1];
    
  return mask;
}

int tree::branch_up(int node1) const {
  return up_branches[node1];
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

void tree::exchange(int node1, int node2) {
  if (ancestor(node1,node2))
    std::swap(node1,node2);

  if (parent(node1) == parent(node2))
    return;
  else if (ancestor(node2,node1) or not names[node2]->parent->parent) {  

    vector<int> intermediate;
    int here = parent(node1);

    /****** Find the intermediate children *******/
    assert(here != node2);
    do {
      intermediate.push_back(here);
      here = parent(here);
    }
    while(here != node2);

    //FIXME - implement new algorithm!
    assert(0);

    /******  Invert the order of the intermediate nodes ****/
    for(int i=0;i<intermediate.size()/2;i++) {
      int j = intermediate.size()-1-i;

      // exchange their names 
      int pi = parent(intermediate[i]);
      int pj = parent(intermediate[j]);
      std::swap(names[pi],names[pj]);
      names[pi]->name = pi;
      names[pj]->name = pj;

      // exchange their subtrees
      TreeView::exchange_cousins(intermediate[i],intermediate[j]);
    }

    //***** Get the intermediate nodes *********/
    for(int i=0;i<intermediate.size();i++)
      intermediate[i] = parent(intermediate[i]);

    assert(parent(intermediate[0]) == node2);
    assert(parent(node1) == intermediate[intermediate.size()-1]);
  }
  else     
    TreeView::exchange_cousins(node1,node2);

  reorder();
}


tree& tree::operator=(const tree& t1) {
  TreeView(root).destroy();

  n_leaves = t1.leaves();
  root = t1.copy();
  vector<node*> nodes = find_nodes(root);
  order = nodes;
  names = nodes;
  
  // Reconstruct the 'order' and 'names' hashes
  if (nodes.size() > 1)
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

  compute_ancestors();

  return *this;
}


tree::tree(const tree& t1, const tree& t2) {
  root = new node;            // *here = new node;
  root->parent = 0;

  add_left(root,t1);
  add_right(root,t2);

  root->left->parent_branch->length = 0.2;
  root->right->parent_branch->length = 0.2;
}

tree::tree(const tree& t1, double d1, const tree& t2, double d2) {
  root = new node;            // *here = new node;
  root->parent = 0;

  add_left(root,t1);
  add_right(root,t2);
  
  root->left->parent_branch->length = d1;
  root->right->parent_branch->length = d2;
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
      while(!T.ancestor(here,top)) {
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
    while(!T.ancestor(here,top)) {
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

/************************** SequenceTree methods *****************************/

void SequenceTree::write(std::ostream& o,int n) const {
  const std::ios_base::fmtflags old_flags = o.flags(o.flags() | std::ios::fixed);
  assert(0 <= n and n < num_nodes());
  if (n<leaves()) {
    o<<seq(n);
  }
  else {
    const tree& T = *this;
    int left = T[n].left();
    int right = T[n].right();
    o<<"(";
    write(o,left);
    o<<":"<<T.branch(left).length<<",";
    write(o,right);

    if (right>= branches())
      o<<":"<<0<<")";
    else
      o<<":"<<T.branch(right).length<<")";
  }
  o.flags(old_flags);
}


void SequenceTree::write(std::ostream& o) const {
  write(o,num_nodes()-1);
  o<<";\n";
}


int SequenceTree::index(const string& s) const {
  for(int i=0;i<sequences.size();i++) {
    if (sequences[i] == s) return i;
  }
  return -1;
}


SequenceTree::SequenceTree(const string& s)
{
  add_root();
  sequences.push_back(s);
}

SequenceTree::SequenceTree(const sequence& s)
{
  add_root();
  sequences.push_back(s.name);
}

SequenceTree::SequenceTree(const SequenceTree& T1, const SequenceTree& T2):tree(T1,T2) {
  assert(0); //associations between leaves and sequence names
  // can get screwed up - CHECK this - it could be OK.
  for(int i=0;i<T1.leaves();i++) 
    sequences.push_back(T1.seq(i));
  for(int i=0;i<T2.leaves();i++) 
    sequences.push_back(T2.seq(i));
}


// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
SequenceTree::SequenceTree(std::istream& file) {
  assert(file);

  int depth = 0;
  int pos=0;
  //how to turn pos into a string?

  vector<SequenceTree> tree_stack;

  string name;
  do {
    char c;
    file>>c;
    if (c == '(') {
      depth++;
      if (name.length()!=0) 
	throw myexception(string("In tree file, found '(' in the middle of name \"") +name
			  +string("\""));
    }
    else if (c== ')') {
      if (name.length() != 0) {
	tree_stack.push_back(SequenceTree(name));
	name = "";
      }
      assert(tree_stack.size() >= 2);
      SequenceTree T2 = tree_stack.back();tree_stack.pop_back();
      SequenceTree T1 = tree_stack.back();tree_stack.pop_back();
      SequenceTree Join = SequenceTree(T1,T2);
      tree_stack.push_back(Join);
      //      std::cerr<<"    leaves: "<<T1.leaves()<<" + "<<T2.leaves()<<" = "<<Join.leaves()<<endl;
      depth--;
      if (depth < 0) 
	throw myexception(string("In tree file, too many end parenthesis."));
    }
    else if (c== ',') {
      if (name.length() != 0) {
	tree_stack.push_back(SequenceTree(name));
	name = "";
      }
    }
    else if (c== ' ' or c=='\n' or c == 9) 
      ;
    else 
      name += c;
    //    std::cerr<<"char = "<<c<<"    depth = "<<depth<<"   stack size = "<<tree_stack.size()<<"    name = "<<name<<endl;

    pos++;
  } while (file and depth > 0);
  assert(tree_stack.size() == 1);
  (*this) = tree_stack[0];
}

