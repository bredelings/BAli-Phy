#include "tree.H"
#include "myexception.H"

/************************** SequenceTree methods *****************************/
double convertToDouble(const string& s) {
   std::istringstream i(s);
   double d;
   i>>d;
   return d;
}


string convertToString(double x)
{
   std::ostringstream o;
   o.flags(o.flags() | std::ios::fixed);

   if (o << x)
     return o.str();
   // some sort of error handling goes here...
   return "conversion error";
} 

string SequenceTree::write(int n,bool lengths) const {
  assert(0 <= n and n < num_nodes());
  string output;

  if (n<leaves()) {
    output = seq(n);
    
  }
  else {
    const tree& T = *this;
    int left = T[n].left();
    int right = T[n].right();
    double llength = T.branch_up(left).length();
    double rlength = T.branch_up(right).length();

    if (right >= branches())
      rlength = 0;


    output = string("(") + write(left,lengths);

    if (lengths)
      output = output + ":" + convertToString(llength);

    output = output + "," + write(right,lengths);

    if (lengths)
      output = output + ":" + convertToString(rlength);

    output += ")";
  }

  return output;
}


string SequenceTree::write(bool lengths) const {
  return write(num_nodes()-1,lengths) + ";";
}

int SequenceTree::index(const string& s) const {
  for(int i=0;i<sequences.size();i++) {
    if (sequences[i] == s) return i;
  }
  return -1;
}


void SequenceTree::standardize(const vector<int>& lnames) {
  assert(lnames.size() == sequences.size());

  vector<string> old = sequences;
  for(int i=0;i<sequences.size();i++)
    sequences[lnames[i]] = old[i];

  tree::standardize(lnames);
}


SequenceTree::SequenceTree(const sequence& s)
{
  add_root();
  sequences.push_back(s.name);
}

SequenceTree::SequenceTree(const SequenceTree& T1, const SequenceTree& T2):tree(T1,T2) {
  
  // We will create new names which will be the same as
  //  T1.order + T2.order
  for(int i=0;i<T1.leaves();i++) 
    sequences.push_back(T1.seq(T1.get_nth(i)));
  for(int i=0;i<T2.leaves();i++) 
    sequences.push_back(T2.seq(T2.get_nth(i)));
}


SequenceTree::SequenceTree(const SequenceTree& T1, double b1, const SequenceTree& T2, double b2)
  :tree(T1,b1,T2,b2) {
  
  // We will create new names which will be the same as
  //  T1.order + T2.order
  for(int i=0;i<T1.leaves();i++) 
    sequences.push_back(T1.seq(T1.get_nth(i)));
  for(int i=0;i<T2.leaves();i++) 
    sequences.push_back(T2.seq(T2.get_nth(i)));
}

SequenceTree::SequenceTree(std::istream& file) {
  assert(file);

  string total;
  string line;
  while(getline(file,line))
    total += line;
  read(total);
}

void add_left(node*,node*);
void add_right(node*,node*);

node* connect(node* l,double lb,node* r,double rb) {
  node* n = new node;
  add_left(n,l);
  add_right(n,r);

  l->parent_branch->length = lb;
  r->parent_branch->length = rb;

  return n;
}


// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
void SequenceTree::read(const string& line) {

  if (line[0] != '(') {
    (*this) = SequenceTree();
    add_root();
    sequences.push_back(line);
    return;
  }

  int depth = 0;
  int pos=0;

  //how to turn pos into a string?

  vector<node*> tree_stack;
  vector<double> branch_stack;

  string name;
  for(int i=0;i<line.size();i++) {
    char c = line[i];

    /******* Read the data from 'name' into the stacks *******/
    if (c== ':') {
      if (name.length() != 0) {
	node* n = new node;
	n->name = sequences.size();
	sequences.push_back(name);
	tree_stack.push_back(n);

	name = "";
      }
    }
    else if (c== ',' or c==')') {
      branch_stack.push_back(convertToDouble(name));
      name = "";
    }


    /****** Process the data given the current state *******/
    if (c == '(') {
      depth++;
      if (name.length()!=0) 
	throw myexception(string("In tree file, found '(' in the middle of name \"") +name
			  +string("\""));
    }
    else if (c== ')') {
      assert(tree_stack.size() >= 2);
      assert(tree_stack.size() == branch_stack.size());

      double b2 = branch_stack.back();branch_stack.pop_back();
      double b1 = branch_stack.back();branch_stack.pop_back();

      node* n2 = tree_stack.back();tree_stack.pop_back();
      node* n1 = tree_stack.back();tree_stack.pop_back();

      node* n3 = connect(n1,b1,n2,b2);
      tree_stack.push_back(n3);
      //      std::cerr<<"    leaves: "<<T1.leaves()<<" + "<<T2.leaves()<<" = "<<Join.leaves()<<endl;
      depth--;
      if (depth < 0) 
	throw myexception(string("In tree file, too many end parenthesis."));
    }
    else if (c== ' ' or c=='\n' or c == 9) 
      ;
    else if (c != ':' and c != ',')
      name += c;
    //    std::cerr<<"char = "<<c<<"    depth = "<<depth<<"   stack size = "<<tree_stack.size()<<"    name = "<<name<<endl;

    pos++;
    if (depth <1) break;
  }
  assert(tree_stack.size() == 1);
  root = tree_stack[0];

  reanalyze();
}

