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
    output = "(";
    for(const_out_edges_iterator i = b.branches_after();i;) {
      output += write(*i,print_lengths);
      i++;
      if (i)
	output += ",";
    }
    output += ")";
  }

  // print the branch length if requested
  if (print_lengths)
    output += ":" + convertToString(b.length());

  return output;
}

string RootedSequenceTree::write(bool print_lengths) const {
  string output = "(";
  for(const_out_edges_iterator i = root_;i;) {
    const_branchview b = *i;
    output += write(b,print_lengths);

    i++;
    if (i)
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


// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
void RootedSequenceTree::parse(const string& line) {

  if (line[0] != '(') {
    (*this) = RootedSequenceTree(line);
    return;
  }

  int depth = 0;
  int pos=0;

  //how to turn pos into a string?

  vector<RootedSequenceTree> tree_stack;

  string word;
  for(int i=0;i<line.size();i++) {
    char c = line[i];

    //------* Read the data from 'word' into the stacks ------//
    if (c== ':') {
      if (word.length() != 0) {
	tree_stack.push_back(RootedSequenceTree(word));
	word = "";
      }
    }
    else if (c== ',' or c==')') {
      (*tree_stack.back().root().branches_out()).set_length(convertTo<double>(word));
      word = "";
    }


    //------ Process the data given the current state ------//
    if (c == '(') {
      depth++;
      if (word.length()!=0) 
	throw myexception(string("In tree file, found '(' in the middle of word \"") +
			  word + string("\""));
    }
    else if (c== ')') {
      assert(tree_stack.size() >= 2);

      RootedSequenceTree T1 = tree_stack.back();tree_stack.pop_back();
      RootedSequenceTree T2 = tree_stack.back();tree_stack.pop_back();

      tree_stack.push_back(RootedSequenceTree(T1,T2));
      //      std::cerr<<"    leaves: "<<T1.n_leaves()<<" + "<<T2.n_leaves()<<" = "<<Join.n_leaves()<<endl;
      depth--;
      if (depth < 0) 
	throw myexception(string("In tree file, too many end parenthesis."));
    }
    else if (c== ' ' or c=='\n' or c == 9) 
      ;
    else if (c != ':' and c != ',')
      word += c;
    //    std::cerr<<"char = "<<c<<"    depth = "<<depth<<"   stack size = "<<tree_stack.size()<<"    word = "<<word<<endl;

    pos++;
    if (depth <1) break;
  }
  assert(tree_stack.size() == 1);


  (*this) = tree_stack[0];
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

std::ostream& operator <<(std::ostream& o,const RootedSequenceTree& T) {
  return o<<T.write();
}

std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  return o<<add_root(T,0);
}

