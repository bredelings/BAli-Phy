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

void SequenceTree::read(const string& filename) {
  RootedSequenceTree RT;
  RT.read(filename);
  
  // FIXME - but what if I WANT the node there?
  if (RT.root().neighbors().size() == 2)
    (*this) = remove_root(RT);
}

void SequenceTree::read(std::istream& file) {
  RootedSequenceTree RT;
  RT.read(file);
  
  // FIXME - but what if I WANT the node there?
  if (RT.root().neighbors().size() == 2)
    (*this) = remove_root(RT);
}

string SequenceTree::write(bool print_lengths) const 
{
  RootedSequenceTree RT = add_root(*this,0);
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

  // FIXME - but what if I WANT the node there?
  if (RT.root().neighbors().size() == 2)
    (*this) = remove_root(RT);
}


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


string RootedSequenceTree::write(const_branchview b,bool print_lengths) const {
  string output;

  // If this is a leaf node, then print the name
  if (b.target().is_leaf_node())
    output += sequences[b.target()];
  // If this is an internal node, then print the subtrees
  else {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write(branches[i],print_lengths);

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

string RootedSequenceTree::write(bool print_lengths) const 
{
  vector<const_branchview> branches = sorted_neighbors(root());

  string output = "(";
  for(int i=0;i<branches.size();i++) {
    output += write(branches[i],print_lengths);
    if (i+1 < branches.size())
      output += ',';
  }
  output += ");";
  return output;
}



void RootedSequenceTree::read(const string& filename) {
  ifstream file(filename.c_str());
  if (not file) 
    throw myexception()<<"Couldn't open file '"<<filename<<"'";
  read(file);
  file.close();
}


void RootedSequenceTree::read(istream& file) {
  assert(file);

  string total;
  string line;
  while(getline(file,line))
    total += line;
  parse(total);
}

SequenceTree remove_root_branch(RootedSequenceTree RT) {
  nodeview r1 = RT.root();
  nodeview r2 = *(RT.root().neighbors());
  RT.reroot(0);

  r2 = RT.prune_subtree(RT.directed_branch(r2,r1));
  RT.remove_node_from_branch(r2);
  return SequenceTree(RT);
}

// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
void RootedSequenceTree::parse(const string& line) {

  if (line[0] != '(') {
    (*this) = RootedSequenceTree(line);
    return;
  }

  vector< vector<RootedSequenceTree> > tree_stack(1);

  string word;
  for(int i=0;i<line.size();i++) {
    char c = line[i];

    if (c == ';') break;

    //------- Read the data from 'word' into the stacks ------//
    if (c == ':') {
      if (word.length() != 0) {
	RootedSequenceTree RT(word);
	int node = RT.add_node(RT.root());
	RT.reroot(node);
	tree_stack.back().push_back(RT);
	word = "";
      }
    }
    else if (c== ',' or c==')') {
      (*tree_stack.back().back().root().branches_out()).set_length(convertTo<double>(word));
      word = "";
    }


    //------ Process the data given the current state ------//
    if (c == '(') {
      tree_stack.push_back(vector<RootedSequenceTree>());
      if (word.length()!=0) 
	throw myexception()<<"In tree file, found '(' in the middle of word \""<<word<<"\"";
    }
    else if (c== ')') {
      // We need at least 2 levels of trees
      if (tree_stack.size() < 2)
	throw myexception()<<"In tree file, too many end parenthesis.";

      // We need at least 2 trees to merge
      const vector<RootedSequenceTree>& trees = tree_stack.back();
      if (not trees.size() == 2)
	throw myexception()<<"We can only handle binary trees";
      assert(trees.size() >= 2);

      // add the trees in the bottom level, and put them in the next level up
      tree_stack[tree_stack.size()-2].push_back(trees[0] + trees[1]);

      // destroy the bottom level
      tree_stack.pop_back();

      //      std::cerr<<"    leaves: "<<T1.n_leaves()<<" + "<<T2.n_leaves()<<" = "<<Join.n_leaves()<<endl;
    }
    else if (c== ' ' or c=='\n' or c == 9) 
      ;
    else if (c != ':' and c != ',')
      word += c;
    //    std::cerr<<"char = "<<c<<"    depth = "<<depth<<"   stack size = "<<tree_stack.size()<<"    word = "<<word<<endl;

  }
  if (tree_stack.size() != 1)
    throw myexception()<<"Attempted to read w/o enough left parenthesis";
  if (tree_stack.back().size() != 1)
    throw myexception()<<"Multiple trees on the same line";

  (*this) = tree_stack.back()[0];

  nodeview r1 = root();
  nodeview r2 = *(root().neighbors());
  reroot(r2);
  r2 = prune_subtree(directed_branch(r2,r1));
}

vector<int> RootedSequenceTree::standardize() {
  return RootedTree::standardize();
}


vector<int> RootedSequenceTree::standardize(const vector<int>& lnames) {
  assert(lnames.size() == sequences.size());

  vector<string> old = sequences;
  for(int i=0;i<sequences.size();i++)
    sequences[lnames[i]] = old[i];

  return RootedTree::standardize(lnames);
}

RootedSequenceTree::RootedSequenceTree(const SequenceTree& T,int r) 
  :RootedTree(T,r),SequenceSet(T)
{ }


RootedSequenceTree::RootedSequenceTree(const string& s)
{
  add_first_node();
  sequences.push_back(s);
}

RootedSequenceTree::RootedSequenceTree(istream& file) {
  read(file);
}

RootedSequenceTree::RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2):
  RootedTree(T1,T2) {
  
  // We will create new names which will be the same as
  //  T1.order + T2.order
  for(int i=0;i<T1.get_sequences().size();i++) 
    sequences.push_back(T1.seq(i));
  for(int i=0;i<T2.get_sequences().size();i++) 
    sequences.push_back(T2.seq(i));
}

//FIXME T.seq(i) -> T.leafname(i)
//FIXME T.get_sequences -> T.leafnames()
void delete_node(SequenceTree& T,const std::string& name) {
  std::abort(); // this isn't quite right yet..
  int index = find_index(T.get_sequences(),name);
  T.prune_subtree(index);
}



RootedSequenceTree add_root(SequenceTree T,int b) {
  int r = T.create_node_on_branch(b);
  return RootedSequenceTree(T,r);
}
  
SequenceTree remove_root(const RootedSequenceTree& RT) {
  SequenceTree T(RT);
  T.remove_node_from_branch(RT.root());
  return T;
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
  while(getline(i,line)) {
    if (not line.empty()) {
      T.parse(line);
      return i;
    }
  }
  throw myexception()<<"Failed to read tree: file ended.";
}

std::istream& operator >>(std::istream& i,RootedSequenceTree& RT) 
{
  string line;
  while(getline(i,line)) {
    if (not line.empty()) {
      RT.parse(line);
      return i;
    }
  }
  throw myexception()<<"Failed to read tree: file ended.";
}


std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  return o<<T.write();
}

std::ostream& operator <<(std::ostream& o,const RootedSequenceTree& RT) {
  return o<<RT.write();
}



