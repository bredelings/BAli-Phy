#include "tree.H"
#include "myexception.H"
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
    b.node1 = i;
    b.node2 = parent(i);
    b.length = 1.0;
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
    else if (c== ' ' || c=='\n' || c == 9) 
      ;
    else 
      name += c;
    //    std::cerr<<"char = "<<c<<"    depth = "<<depth<<"   stack size = "<<tree_stack.size()<<"    name = "<<name<<endl;

    pos++;
  } while (file && depth > 0);
  assert(tree_stack.size() == 1);
  (*this) = tree_stack[0];
}

