/* Version 1: based on operating on trees, then generating alignments */

#include <vector>
#include <string>
#include <fstream>

using std::vector;
using std::string;
using std::ifstream;

struct alphabet: public vector<char> {

  //substitution matrix?

  int operator[](char c) const;

  alphabet(const char*);
};

int alphabet::operator[](char c) const {
  for(int i=0;i<size();i++) {
    if ((*this)[i]==c)
      return i;
  }
  return -1;
}

alphabet::alphabet(const char* s) {
  string letters(s);
  vector<char>& view = *this;
  for(int i=0;i<letters.length();i++) 
    view[i] = letters[i];
}

struct sequence:vector<int> {
  alphabet& a;
  string name;
  sequence(alphabet& a1,const char* filename);
};

sequence::sequence(alphabet& a1,const char* filename): a(a1),name(filename) {
  ifstream file(filename);
  int length;
  file>>length;
  vector<int> data(length);
  for(int i=0;i<length;i++) {
    char c;
    file>>c;
    data[i]=a[c];
  }
}

struct gap {
  int start;
  int length;
  bool deletion;
};

struct branch {
  int node1;
  int node2;
  vector<gap> gaps;
};

struct node {
  int name;
  const sequence* s;

  node(sequence& s2): s(&s2) {}
};


// All internal nodes should be number higher than all leaves

/* These trees are going to be rooted.  
 *   o But we'll ignore the root I guess.
 *   o The root should have 2 neighbors
 * How do you store this?  Its a binary tree, but with labelled edges...
 */

struct tree {
private:
  int n_leaves;
public:
  vector<branch> branches;
  vector<double> lengths;
  
  vector<node> nodes;

  int leaves() const {return n_leaves;}

  tree() {n_leaves=0;};
  tree(sequence& s) { n_leaves=1;nodes.push_back(*new node(s)); }
};

tree operator+(const tree& t1,const tree& t2) {
  int t1_root = t1.nodes.size();
  int t2_root = t2.nodes.size();
  
  //  add_edge(t1_root,new_root)
  //  add_edge(t2_root,new_root)
  //  root = new_root;
  tree t3;
  return t3;
}


void shift(vector<int>& v,int start,int delta) {
  for(int i=start;i<v.size();i++)
    v[i] += delta;
}


struct alignment {
  tree& t;
  vector<int> names;
  vector< vector<int> > positions;
  alignment(tree&);
};

void alignment::modify(int nseq,branch& b) {
  for(int i=0;i<b.gaps.size();i++) {
    gap& g = b.gaps[i];
    if (g.deletion) { //deletion
      sequence& seq = positions[nseq];
      erase(seq.begin()+g.start,seq.begin()+g.start+g.length);
    }
    else { // insertion
      insert(seq.begin()+g.start,g.length,-1);

      for(int j=0;j<positions.size();j++) {
	if (j==nseq) continue;
	sequence& seq = positions[j];
	shift(seq,
      }
    }
  }
  
}

alignment::alignment(tree& t1): t(t1) {
  const int start_leaf = 1;

  vector<int> s1(t.nodes[start_leaf].s->size());
  for(int i=0;i<s1.size();i++)
    s1[i] = 1;

  names.push_back(start_leaf);
  positions.push_back(s1);

  
  boundary contains s1;
  do {
    foreach $node (@boundary) {
      remove sequence from alignment if not first sequence;
      foreach $child (children of $node) {
	insert copy of $node''s alignment that we haven''t already covered
	put the copy in new_boundary;
	modify the copy using gaps on the edge($node,$child)
      }
    }
    boundary = new_boundary;
  } while (not empty(boundary));
}

alignment construct_alignment(tree& t,int node) {
  alignment a(t,node);
}

double gap_init;
double gap_extend;

double probability(alignment& A) {

  /* we don't need this part - its in the proposal distribution
  double gap_ll=0;
  for(int i=0;i<t.branches.size();i++) {
    for(int j=0;j<t.branches[i].gaps.size();i++) 
      gap_ll += gap_init + gap_extend*(t.branches[i].gaps[j].length-1);
  } 
  */

  /* web search "multiple alignment" */
}


int main() {
  // also need to specify a substitution matrix!
  alphabet amino_acids("ARNDCQEGHILKMFPTWYV");

  sequence human     (amino_acids,"hemoglobin_hs");
  sequence chimp     (amino_acids,"hemoglobin_cz");
  sequence gorilla   (amino_acids,"hemoglobin_go");
  sequence orangutan (amino_acids,"hemoglobin_or");

  tree t = (human + chimp) + (gorilla + orangutan);

  alignment A(t);
  double p = probability(A);
  while(1) {
    alignment A1(t);
    // propose a new alignment

    double p1 = probability(A1);
  }

  return 1;
}
