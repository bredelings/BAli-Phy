#include "etree.H"
#include "exponential.H"

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

void tree::renumber() {
  lookup.clear();
  lookup.push_back(root);


  /* Mark each node as non-visited */
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

  for(int i=0;i<lookup.size();i++)
    lookup[i]->name = -1;

  int size1 = lookup.size();

  /* Add nodes in order of distance from leaves */

  lookup.clear();
  find_leaves(root,lookup);
  n_leaves = lookup.size();
  // Here we over-write the leaf names
  //  so they are determined by the tree struct
  //  instead of specified from outside
  for(int i=0;i<lookup.size();i++)
    lookup[i]->name = i;

  start=0;
  do {
    int end = lookup.size();
    for(int i=start;i<end;i++) {
      node* parent = lookup[i]->parent;
      if (!parent) continue; // actually this would be a good exit condition

      if (parent->left && parent->right) {  // if there are 2 children
	if (parent->left->name == -1) continue;  //both children 
	if (parent->right->name == -1) continue; //must go on first
	if (lookup[i]->name < parent->left->name || lookup[i]->name < parent->right->name)
	  continue; //and only the second child puts the parent on
      }
      parent->name = lookup.size();
      lookup.push_back(parent);
    }
    start = end;
  } while (start < lookup.size());

  assert(size1 == lookup.size());
  for(int i=0;i<lookup.size();i++)
    assert(lookup[i]->name == i);

  compute_ancestors();
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

  if (n1 > n2 ||
      (!T[n1].parent->parent && !T[n2].parent->parent))
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

tree& tree::operator=(const tree& t1) {
  root = t1.copy();
  renumber();
  return *this;
}


tree::tree(const tree& t1, const tree& t2) {
  root = new node;            // *here = new node;
  root->parent = 0;

  add_left(*root,t1);
  add_right(*root,t2);

  /*
  for(int i=0;i<num_nodes()-2;i++) {
    branches[i].node1 = i;
    branches[i].node2 = parent(i);
    branches[i].length = 1.0;
  }
  */
}

tree::tree(const tree& t1, double d1, const tree& t2, double d2) {
  root = new node;            // *here = new node;
  root->parent = 0;

  add_left(*root,t1);
  add_right(*root,t2);
  
  //  add_edge(t1_root,new_root,d1)
  //  add_edge(t2_root,new_root,d2)
}


