#include <iostream>
#include <fstream>
#include "sequencetree.H"
#include "myexception.H"
#include "util.H"

using namespace std;

int SequenceSet::index(const string& s) const {
  for(int i=0;i<sequences.size();i++)
    if (sequences[i] == s) return i;
  return -1;
}


//-------------------------- SequenceTree methods ----------------------------//
nodeview SequenceTree::prune_subtree(int branch) {
  // get pointers to current leaves
  vector<BranchNode*> leaves1(n_leaves());
  for(int i=0;i<leaves1.size();i++)
    leaves1[i] = nodes_[i];
  
  // remove the subtree
  nodeview node_remainder = Tree::prune_subtree(branch);
  
  // get pointers to NEW leaves
  vector<BranchNode*> leaves2(n_leaves());
  for(int i=0;i<leaves2.size();i++)
    leaves2[i] = nodes_[i];

  // figure out the mapping
  vector<string> newnames;
  for(int i=0;i<leaves2.size();i++) {
    int index = find_index(leaves1,leaves2[i]);
    if (index == -1)
      break;
    else
      newnames.push_back(sequences[index]);
  }
  assert(newnames.size() == leaves2.size());

  // select the new names
  sequences = newnames;

  return node_remainder;
}

vector<int> SequenceTree::prune_leaves(const vector<int>& remove) 
{
  // remove the subtree
  vector<int> mapping = Tree::prune_leaves(remove);

  // figure out the mapping
  vector<string> newnames(mapping.size());
  for(int i=0;i<mapping.size();i++)
    newnames[i] = sequences[mapping[i]];

  // select the new names
  sequences = newnames;

  return mapping;
}

void SequenceTree::read(const string& filename) {
  ifstream file(filename.c_str());
  if (not file) 
    throw myexception()<<"Couldn't open file '"<<filename<<"'";
  read(file);
  file.close();
}

void SequenceTree::read(std::istream& file) {
  assert(file);

  string total;
  string line;
  while(getline_handle_dos(file,line))
    total += line;
  parse(total);
}

string SequenceTree::write(bool print_lengths) const 
{
  RootedSequenceTree RT(*this,directed_branch(0).target());
  return RT.write(print_lengths);
}

vector<int> SequenceTree::standardize() {
  return Tree::standardize();
}


vector<int> SequenceTree::standardize(const vector<int>& lnames) {
  assert(lnames.size() == sequences.size());

  vector<string> old = sequences;
  for(int i=0;i<sequences.size();i++)
    sequences[lnames[i]] = old[i];

  return Tree::standardize(lnames);
}

void SequenceTree::parse(const string& s) {
  RootedSequenceTree RT;
  RT.parse(s);

  (*this) = RT;
}

SequenceTree::SequenceTree(const std::string& s) {
  add_first_node();
  sequences.push_back(s);
}

SequenceTree::SequenceTree(const Tree& T,const vector<string>& names)
  :Tree(T),SequenceSet(names)
{ }

SequenceTree::SequenceTree(const RootedSequenceTree& RT) 
  :Tree(RT),SequenceSet(RT)
{ }

//-------------------------- SequenceTree methods ----------------------------//
nodeview RootedSequenceTree::prune_subtree(int branch) {
  // get pointers to current leaves
  vector<BranchNode*> leaves1(n_leaves());
  for(int i=0;i<leaves1.size();i++)
    leaves1[i] = nodes_[i];
  
  // remove the subtree
  nodeview node_remainder = RootedTree::prune_subtree(branch);
  
  // get pointers to NEW leaves
  vector<BranchNode*> leaves2(n_leaves());
  for(int i=0;i<leaves2.size();i++)
    leaves2[i] = nodes_[i];

  // figure out the mapping
  vector<string> newnames;
  for(int i=0;i<leaves2.size();i++) {
    int index = find_index(leaves1,leaves2[i]);
    if (index == -1)
      break;
    else
      newnames.push_back(sequences[index]);
  }

  assert( (newnames.size() == n_leaves()) or 
	  ((newnames.size() == n_leaves()-1) and (root_->node == n_leaves())) );

  // select the new names
  sequences = newnames;

  return node_remainder;
}

vector<int> RootedSequenceTree::prune_leaves(const vector<int>& remove) 
{
  // remove the subtree
  vector<int> mapping = SequenceTree::prune_leaves(remove);

  // figure out the mapping
  vector<string> newnames(mapping.size());
  for(int i=0;i<mapping.size();i++)
    newnames[i] = sequences[mapping[i]];

  // select the new names
  sequences = newnames;

  // if we need to do this, virtualize unlink_subtree to complain if the subtree
  // contains the root.
  root_ = NULL;

  return mapping;
}

string write(const vector<string>& names, const_branchview b, bool print_lengths)
{
  string output;

  // If this is a leaf node, then print the name
  if (b.target().is_leaf_node())
    output += names[b.target()];
  // If this is an internal node, then print the subtrees
  else {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write(names,branches[i],print_lengths);

      if (i+1<branches.size())
	output += ",";
    }
    output += ")";
  }

  // print the branch length if requested
  if (print_lengths)
    output += ":" + convertToString(b.length());

  return output;
}

string write(const RootedTree& T, const vector<string>& names, bool print_lengths) 
{
  vector<const_branchview> branches = sorted_neighbors(T.root());

  string output = "(";
  for(int i=0;i<branches.size();i++) {
    output += write(names,branches[i],print_lengths);
    if (i+1 < branches.size())
      output += ',';
  }
  output += ");";
  return output;
}

string RootedSequenceTree::write(const_branchview b,bool print_lengths) const {
  return ::write(get_sequences(), b, print_lengths);
}

string RootedSequenceTree::write(bool print_lengths) const 
{
  return ::write(*this, get_sequences(), print_lengths);
}

// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
void RootedSequenceTree::parse(const string& line) 
{
  vector< vector<BranchNode*> > tree_stack(1);
  sequences.clear();

  const string delimiters = "(),:;";
  const string whitespace = "\t\n ";

  string prev;
  string word;
  for(int i=0;get_word(word,i,line,delimiters,whitespace);prev=word) 
  {
    //std::cerr<<"word = '"<<word<<"'    depth = "<<tree_stack.size()<<"   stack size = "<<tree_stack.back().size()<<std::endl;

    if (word == ";") break;

    //------ Process the data given the current state ------//
    if (word == "(") {
      tree_stack.push_back(vector<BranchNode*>());
      if (not (prev == "(" or prev == "," or prev == ""))
	throw myexception()<<"In tree file, found '(' in the middle of word \""<<word<<"\"";
    }
    else if (word == ")") {
      // We need at least 2 levels of trees
      if (tree_stack.size() < 2)
	throw myexception()<<"In tree file, too many end parenthesis.";

      // merge the trees in the top level
      BranchNode* BN = tree_stack.back()[0];
      for(int i=1;i<tree_stack.back().size();i++)
	TreeView::merge_nodes(BN,tree_stack.back()[i]);

      // destroy the top level
      tree_stack.pop_back();

      // insert merged trees into the next level down
      BN = ::add_node(BN);
      BN->out->length = BN->length = -1;
      tree_stack.back().push_back(BN);
    }
    else if (prev == "(" or prev == "," or prev == "") {
      BranchNode* BN = new BranchNode(-1,sequences.size(),-1);
      BN->out = BN->next = BN->prev = BN;

      BN = ::add_node(BN);
      BN->out->length = BN->length = -1;
      tree_stack.back().push_back(BN);

      sequences.push_back(word);
    }
    else if (prev == ":") {
      BranchNode* BN = tree_stack.back().back();
      BN->out->length = BN->length = convertTo<double>(word);
    }
  }


  if (tree_stack.size() != 1)
    throw myexception()<<"Attempted to read w/o enough left parenthesis";
  if (tree_stack.back().size() != 1)
    throw myexception()<<"Multiple trees on the same line";

  BranchNode* remainder = tree_stack.back()[0];
  root_ = TreeView::unlink_subtree(remainder->out);
  TreeView(remainder).destroy();

  reanalyze(root_);
}

RootedSequenceTree& RootedSequenceTree::operator=(const RootedSequenceTree& T)
{
  RootedTree::operator=(T);
  SequenceSet::operator=(T);

  return *this;
}


RootedSequenceTree::RootedSequenceTree(const RootedTree& T,const vector<string>& names)
  :Tree(T),RootedTree(T),SequenceTree(T,names)
{ }

RootedSequenceTree::RootedSequenceTree(const SequenceTree& T,int r) 
  :Tree(T),RootedTree(T,r),SequenceTree(T)
{ }


RootedSequenceTree::RootedSequenceTree(const string& s)
{
  add_first_node();
  sequences.push_back(s);
}

RootedSequenceTree::RootedSequenceTree(istream& file) {
  read(file);
}

RootedSequenceTree::RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2)
  :RootedTree(T1,T2) 
{
  // We will create new names which will be the same as
  //  T1.order + T2.order
  for(int i=0;i<T1.get_sequences().size();i++) 
    sequences.push_back(T1.seq(i));
  for(int i=0;i<T2.get_sequences().size();i++) 
    sequences.push_back(T2.seq(i));
}

//FIXME T.seq(i) -> T.leafname(i)
//FIXME T.get_sequences -> T.leafnames()
void delete_node(SequenceTree& T,const std::string& name) 
{
  int index = find_index(T.get_sequences(),name);
  nodeview n = T.prune_subtree(T.branch(index).reverse());
  T.remove_node_from_branch(n);
}

RootedSequenceTree add_root(SequenceTree T,int b) {
  int r = T.create_node_on_branch(b);
  return RootedSequenceTree(T,r);
}
  
RootedSequenceTree operator+(const RootedSequenceTree& t1,const RootedSequenceTree& t2) 
{
  RootedSequenceTree t3(t1,t2);
  int new_root = t3.add_node(t3.root());
  t3.reroot(new_root);

  return t3;
}

std::istream& operator >>(std::istream& i,SequenceTree& T) 
{
  string line;
  while(getline_handle_dos(i,line)) {
    if (not line.empty()) {
      T.parse(line);
      return i;
    }
  }
  throw myexception()<<"Failed to read tree: file ended.";
}

std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  return o<<T.write();
}

SequenceTree star_tree(const vector<string>& names) 
{
  return SequenceTree(star_tree(names.size()), names);
}

int find_partition(const valarray<bool>& p1, const vector<valarray<bool> >& pv) {
  valarray<bool> np1 = not p1;
  for(int i=0;i<pv.size();i++) {
    if (equal(pv[i],p1) or equal(pv[i],np1))
      return i;
  }
  return -1;
}

double branch_distance(const SequenceTree& T1, const SequenceTree& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  vector<double> d1(T1.n_branches());
  vector< valarray<bool> > part1(T1.n_branches(),valarray<bool>(false,T1.n_leaves()));

  vector<double> d2(T2.n_branches());
  vector< valarray<bool> > part2(T2.n_branches(),valarray<bool>(false,T2.n_leaves()));

  // get partitions and lengths for T1
  for(int b=0;b<T1.n_branches();b++) {
    d1[b] = T1.branch(b).length();
    part1[b] = branch_partition(T1,b);
  }

  // get partitions and lengths for T2
  for(int b=0;b<T2.n_branches();b++) {
    d2[b] = T2.branch(b).length();
    part2[b] = branch_partition(T2,b);
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
  for(int i=0;i<part2.size();i++) {
    int found = find_partition(part2[i],part1);
    if (found == -1)
      total += std::abs(d2[i]);
    else
      ; // this is already counted in the previous loop
  }
  return total;
}

double internal_branch_distance(const SequenceTree& T1, const SequenceTree& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  unsigned l1 = T1.n_leafbranches();
  unsigned l2 = T2.n_leafbranches();

  unsigned n1 = T1.n_branches() - l1;
  unsigned n2 = T2.n_branches() - l2;

  vector<double> d1(n1);
  vector<double> d2(n2);

  vector< valarray<bool> > part1(n1,valarray<bool>(false,T1.n_leaves()));
  vector< valarray<bool> > part2(n2,valarray<bool>(false,T2.n_leaves()));

  // get partitions and lengths for T1
  for(int i=0;i<n1;i++) {
    d1[i] = T1.branch(i+l1).length();
    part1[i] = branch_partition(T1,i+l1);
  }

  // get partitions and lengths for T2
  for(int i=0;i<n2;i++) {
    d2[i] = T2.branch(i+l2).length();
    part2[i] = branch_partition(T2,i+l2);
  }

  // Accumulate distances for T1 partitions
  vector<bool> found(n2,false);

  double total = 0;
  for(int i=0;i<part1.size();i++) {
    int j = find_partition(part1[i],part2);
    if (j == -1)
      total += d1[i];
    else {
      found[j] = true;
      total += abs(d1[i]-d2[j]);
    }
  }

  for(int i=0;i<part2.size();i++) {
    if (not found[i])
      total += d2[i];
  }

  return total;
}

unsigned topology_distance(const SequenceTree& T1, const SequenceTree& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  unsigned l1 = T1.n_leafbranches();
  unsigned l2 = T2.n_leafbranches();

  unsigned n1 = T1.n_branches() - l1;
  unsigned n2 = T2.n_branches() - l2;

  vector< valarray<bool> > part1(n1,valarray<bool>(false,T1.n_leaves()));
  vector< valarray<bool> > part2(n2,valarray<bool>(false,T2.n_leaves()));

  // get partitions and lengths for T1
  for(int i=0;i<n1;i++)
    part1[i] = branch_partition(T1,i+l1);

  // get partitions and lengths for T2
  for(int i=0;i<n2;i++)
    part2[i] = branch_partition(T2,i+l2);

  // Accumulate distances for T1 partitions
  unsigned shared=0;
  for(int i=0;i<part1.size();i++) {
    if (find_partition(part1[i],part2) != -1)
      shared++;
  }

  return (n1-shared) + (n2-shared);
}

double robinson_foulds_distance(const SequenceTree& T1, const SequenceTree& T2) 
{
  return 0.5*topology_distance(T1,T2);
}
