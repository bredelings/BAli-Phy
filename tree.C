#include "tree.H"
#include "myexception.H"
#include "exponential.H"
#include <algorithm>

void TreeView::destroy(node** n) {
  assert(n);
  if (!*n) return;
  destroy(&((*n)->left));
  destroy(&((*n)->right));
  delete *n;
  *n=0;
}

TreeView TreeView::copy() const {
  node* top=0;
  if (!root)
    return TreeView(top);

  top = new node;
  top->name = root->name;
  top->parent = 0;

  top->left = (node*)TreeView(root->left).copy();
  if (top->left) top->left->parent = top;

  top->right = (node*)TreeView(root->right).copy();
  if (top->right) top->right->parent = top;

  return TreeView(top);
}

// exclude root node
vector<int> get_neighbors(const node& n) {
  vector<int> neighbors;
  if (n.left) neighbors.push_back(n.left->name);
  if (n.right) neighbors.push_back(n.right->name);

  assert(n.parent);  // don't deal w/ root node

  if (n.parent->parent)
    neighbors.push_back(n.parent->name);
  else {
    node* parent = n.parent->left;
    if (&n == parent)
      parent = n.parent->right;
    neighbors.push_back(parent->name);
  }

  return neighbors;
}


TreeView tree::copy() const {
  return TreeView(root).copy();
}

vector<node*> find_nodes(node* root) {
  vector<node*> lookup;
  
  lookup.push_back(root);

  int start=0;
  do {
    int end = lookup.size();
    for(int i=start;i<end;i++) {
      node* left = lookup[i]->left;
      node* right = lookup[i]->right;
      if (left) lookup.push_back(left);
      if (right) lookup.push_back(right);
    }
    start = end;
  }  while(start<lookup.size());

  return lookup;
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

/* If we aren't deleting or adding nodes, then we don't change the leaf names */

vector<int> tree::renumber() {
  vector<node*> old_order = lookup;

  lookup = find_nodes(root);
  int size1 = lookup.size();

  // Mark all nodes as non-visted
  for(int i=0;i<lookup.size();i++)
    lookup[i]->name = -1;

  // Fill lookup with the leaves, and name them
  if (old_order.size() != lookup.size()) { 
    lookup.clear();
    find_leaves(root,lookup);
    for(int i=0;i<lookup.size();i++)
      lookup[i]->name = i;
  }
  else { // Don't change the leaf names!
    lookup.clear();
    for(int i=0;i<old_order.size() and leaf(*old_order[i]);i++) {
      old_order[i]->name = i;
      lookup.push_back(old_order[i]);
    }
  }

  n_leaves = lookup.size();

  /* Add nodes in order of distance from leaves */
  int start=0;
  do {
    int end = lookup.size();
    for(int i=start;i<end;i++) {
      node* parent = lookup[i]->parent;
      if (!parent) continue; // actually this would be a good exit condition

      if (parent->left and parent->right) {  // if there are 2 children
	if (parent->left->name == -1) continue;  // both children 
	if (parent->right->name == -1) continue; // must go on first
	if (lookup[i]->name < parent->left->name or 
	    lookup[i]->name < parent->right->name)
	  continue; // and only the second child puts the parent on
      }
      parent->name = lookup.size();
      lookup.push_back(parent);
    }
    start = end;
  } while (start < lookup.size());

  assert(size1 == lookup.size());
  for(int i=0;i<lookup.size();i++) {
    if (leaf(*lookup[i]))
      assert(lookup[i]->name < n_leaves);
    else
      assert(lookup[i]->name == i);
  }

  compute_ancestors();

  vector<int> mapping(old_order.size());
  for(int i=0;i<mapping.size();i++) {
    mapping[i] = old_order[i]->name;
    assert(mapping[i] >=0 and mapping[i] < lookup.size());
  }

  return mapping;
}

// With iterator, this would be:  *left  = (node* tv1);
void tree::add_left(node& parent,node& child) {
  parent.left = &child;
  parent.left->parent = &parent;
}

// With iterator, this would be:  *right  = (node* tv1);
void tree::add_right(node& parent,node& child) {
  parent.right = &child;
  parent.right->parent = &parent;
}

// Do I need to export an interface which maps each node name
// on the two old trees into a node name in the current tree?
// I could put a field 'old_name' in the 'struct node' for this...

// subtree(root,tree:left) = T
void tree::add_left(node& parent,const tree& T) {
  TreeView tv = T.copy();
  add_left(parent,*(node*)tv);
  renumber();
}

// subtree(root,tree:right) = T
void tree::add_right(node& parent,const tree& T) {
  TreeView tv = T.copy();
  add_right(parent,*(node*)tv);
  renumber();
}

void tree::compute_ancestors() {
  const tree& T = *this;

  ancestors.clear();
  for(int i=0;i<num_nodes();i++) {
    std::valarray<bool> mask(false,num_nodes());
    mask[i] = true;

    if (T[i].left) 
      mask |= ancestors[ T[i].left->name ];
    
    if (T[i].right)
      mask |= ancestors[ T[i].right->name ];
    ancestors.push_back(mask);
  }
}

vector<int> tree::path(int n1,int n2) const {
  const tree& T = *this;
  vector<int> result;

  if (ancestor(n1,n2)) {
    for(const node* here = &T[n2];here->name != n1;here = here->parent)
      result.insert(result.begin(),here->name);
    result.insert(result.begin(),n1);
  }
  else if (ancestor(n2,n1)) {
    for(const node* here = &T[n1];here->name != n2;here = here->parent)
      result.insert(result.end(),here->name);
    result.insert(result.end(),n2);
  }
  else {
    int top = n1;
    while(!ancestor(top,n2))
      top = T[top].parent->name;
    result = path(n1,top);
    result.pop_back();
    vector<int> temp = path(top,n2);
    result.insert(result.end(),temp.begin(),temp.end());
  }
  return result;
}

std::valarray<bool> tree::partition(int n1,int n2) const {
  const tree& T = *this;

  std::valarray<bool> mask(T.num_nodes());;

  if (n1 > n2 or
      (!T[n1].parent->parent and !T[n2].parent->parent))
    mask = ancestors[n2];
  else 
    mask = !ancestors[n1];
    
  return mask;
}

void tree::add_root() {
  assert(!root);
  root = new node;
  lookup.push_back(root);
  n_leaves = 1;
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
//    - this is easy in exchange_cousins, but takes works after renumber

void tree::exchange_cousins(int n1, int n2) {
  // our caller guarantees this won't happen
  assert(n1 != n2);
  assert(not ancestor(n1,n2) and not ancestor(n2,n1));
  assert(parent(n1) != parent(n2));

  node* node1 = lookup[n1];
  node* node2 = lookup[n2];

  node* p1 = node1->parent;
  node* p2 = node2->parent;

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
  
  branches_[n1].node1 = node1->parent->name;
  branches_[n2].node1 = node2->parent->name;
}


// if one of the subtrees contains the 
// branch connect the children of the root then
// we have to take special action in fixing up branch lengths

// if parent(node1) == parent(node2), which can happen at the top
// of the tree when p1 != p2, then 

vector<int> tree::exchange(int node1, int node2) {
  vector<node*> old_order = lookup;

  if (ancestor(node1,node2))
    std::swap(node1,node2);

  if (parent(node1) == parent(node2))
    ;
  else if (ancestor(node2,node1)) {  //

    vector<int> intermediate;
    node* here = lookup[node1]->parent;

    assert(here != lookup[node2]);
    do {
      node* there = here->left;
      if (ancestor(there->name,node1))
	  there = here->right;

      intermediate.push_back(there->name);
      here = here->parent;
    }
    while(here != lookup[node2]);

    for(int i=0;i<intermediate.size()/2;i++) {
      int j = intermediate.size()-1-i;
      exchange_cousins(intermediate[i],intermediate[j]);
    }
  }
  else
    exchange_cousins(node1,node2);
  
  vector<int> mapping = renumber();

  /*** fixup branches ***/
  vector<Branch> old_branches = branches_;

  for(int i=0;i<mapping.size();i++) {
    // here we've got i -> mapping[i];
    assert(mapping[i] < branches_.size());
    assert(i < branches_.size());
    branches_[mapping[i]] = old_branches[i];
  }


  return mapping;
}


tree& tree::operator=(const tree& t1) {
  n_leaves = t1.leaves();
  root = t1.copy();
  vector<node*> nodes = find_nodes(root);

  lookup = nodes;

// should now preserve node names
  if (nodes.size() > 1)
    for(int i=0;i<nodes.size();i++) {
      int j = nodes[i]->name;
      assert(0 <= j and j < lookup.size());
      lookup[j] = nodes[i];
    }

  compute_ancestors();
  
  // index numbers shouldn't change across copies
  branches_.clear();
  for(int i=0;i<num_nodes()-2;i++)
    branches_.push_back(t1.branch(i));

  return *this;
}


tree::tree(const tree& t1, const tree& t2) {
  root = new node;            // *here = new node;
  root->parent = 0;

  add_left(*root,t1);
  add_right(*root,t2);

  for(int i=0;i<num_nodes()-2;i++) {
    Branch b;
    b.node1 = parent(i);
    b.node2 = i;
    b.length = 0.2;
    branches_.push_back(b);
  }
}

tree::tree(const tree& t1, double d1, const tree& t2, double d2) {
  root = new node;            // *here = new node;
  root->parent = 0;

  add_left(*root,t1);
  add_right(*root,t2);
  
  //  add_edge(t1_root,new_root,d1)
  //  add_edge(t2_root,new_root,d2)
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
	here = T[here].parent->name;
	present(here) = 1;
      }
      int parent = here;
      
      here = top;
      while(here != parent) {
	here = T[here].parent->name;
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
      here = T[here].parent->name;
      if (present(here) != -1)
	goto done;
      present(here) = 0;
    }
    parent = here;
    
    here = top;
    while(here != parent) {
      here = T[here].parent->name;
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
    assert(T[n].left);
    assert(T[n].right);
    int left = T[n].left->name;
    int right = T[n].right->name;
    o<<"(";
    write(o,left);
    o<<":"<<T.branch(left).length<<",";
    write(o,right);
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

