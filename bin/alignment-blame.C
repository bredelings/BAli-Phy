#include <fstream>
#include <string>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"
#include "logsum.H"

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using namespace optimize;

class alignment_probability: public function {
  tree T;
  vector< vector<int> > labels;

  double alignment_probability::Pr(const vector<double>&,const vector<int>&,int);

  double alignment_probability::Pr(const vector<double>&,const vector<int>&);
public:
  double operator()(const vector<double>& v) const;

  alignment_probability(const vector< vector<int> >& v1,const tree& T1):labels(v1),T(T1) 
  { }
};

double alignment_probability::Pr(const vector<double>& v,const vector<int>& label,int mask) {
  double total=0;

  // map each branch to a group
  vector<int> branch_to_group(T.leafbranches());

  // map each group to a set of branches
  vector< vector<int> > groups;

  // compute the mappings
  for(int i=0;i<labels.size();i++) {
    int group==-1;
    for(int j=0;j<i;j++) {
      if (labels[j] == labels[i]) {
	group==j;
	break;
      }
    }

    if (group == -1) {
      branch_to_group[i] = groups.size();
      groups.push_back(vector<int>(1,i));
    }      
    else {
      branch_to_group[i] = group;
      groups[group].push_back(i);
    }
  }

  // calculate the probability of branches being connected to the center
  int nconnected=0;
  for(int i=0;i<T.leafbranches();i++) {
    if (mask && (1<<i)) {
      total += log(1.0-exp(-v[i]));
      nconnected++;
    }
    else {
      total += -v[i];
      if (groups[branch_to_group[i]].size() > 1)
	return log_0;
    }
  }
  
  // calculate the probability of there being groups.size() groups
  // given than nconnected leaves are connected to the center;
  total += log_fact(groups.size()) - log_fact(nconnected) + log_fact(nconnected-groups.size());
  for(int i=0;i<groups.size();i++)
    total += log_fact(groups.size()-1);

  return total;
}


double alignment_probability::Pr(const vector<double>& v,const vector<int>& label) {
  const int max = 1<<T.leafbranches();
  assert(max != 0);

  // sum of probability of each possibility for each branch
  double total = log_0;
  for(int i=0;i<max;i++)
    total = logsum(total,Pr(v,label,i));

  return total;
}


double alignment_probability::operator(const vector<double>& v) const {
  double total = log_0;

  // product of probability of each label
  for(int i=0;i<labels.size();i++) 
    total += Pr(labels[i],v);

  return total;
}



bool match_tag(const string& line,const string& tag) {
  if (line.size() < tag.size())
    return false;

  return (line.substr(0,tag.size()) == tag);
}

void do_setup(Arguments& args,vector<alignment>& alignments) {

  /* ----- Alphabets to try ------ */
  vector<alphabet> alphabets;
  alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
  alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
  alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));

  /* ----- Try to load alignments ------ */
  string tag = "align[sample";
  if (args.set("tag"))
    tag = args["tag"];

  string line;
  while(getline(cin,line)) {
    if (match_tag(line,tag)) {
      alignment A;
      A.load_phylip(alphabets,cin);

      remove_empty_columns(A);
      if (A.num_sequences() == 0) 
	throw myexception(string("Alignment didn't contain any sequences!"));
      alignments.push_back(A);
    }
  }
}

bool after(int c1, int c2, const alignment& A,const vector<int>& nodes) {
  assert(nodes.size() == A.num_sequences());

  for(int i=0;i<nodes.size();i++) {
    bool p1 = not A.gap(c1,nodes[i]);
    bool p2 = not A.gap(c2,nodes[i]);
    if (p2 and not p1)
      return true;
    if (p1 and not p2)
      return false;
  }
  return false;
}

bool intersect(int c1, int c2, const alignment& A) {
  for(int i=0;i<A.num_sequences();i++) {
    if (not A.gap(c1,i) and not A.gap(c2,i))
      return true;
  }
  return false;
}

vector<int> getorder(const alignment& A,int n1,int n2) {

  // Get node order
  vector<int> nodes;
  nodes.push_back(n1);
  nodes.push_back(n2);
  for(int i=0;i<A.num_sequences();i++)
    if (i != n1 and i != n2)
      nodes.push_back(i);

  // Get starting column arrangement
  vector<int> columns;
  for(int column=0;column<A.length();column++)
    columns.push_back(column);

  //-------- Re-order unordered columns by AP order ---------//
  for(int i=0;i<columns.size()-1;) {
    if (not intersect(columns[i],columns[i+1],A) and after(columns[i],columns[i+1],A,nodes)) {
      std::swap(columns[i],columns[i+1]);
      if (i>0) i--;
    }
    else
      i++;
  }

  vector<int> bits;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    int b = 0;
    if (not A.gap(column,n1))
      b |= (1<<0);
    if (not A.gap(column,n2))
      b |= (1<<1);
    if (b)
      bits.push_back(b);
  }

  return bits;
}

void getpath(const alignment& A,const vector<int>& bits,vector<int>& cx, vector<int>& cy) {
  cx.push_back(0);
  cy.push_back(0);
  int x=0;
  int y=0;
  for(int i=0;i<bits.size();i++) {
    int b = bits[i];
    if (b&(1<<0))
      x++;
    if (b&(1<<1))
      y++;
    cx.push_back(x);
    cy.push_back(y);
  }
}


int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    string name1 = argv[1];
    string name2 = argv[2];

    /*----------- Load alignment and tree ---------*/
    vector<alignment> alignments;
    do_setup(args,alignments);

    cerr<<"Read "<<alignments.size()<<" alignments\n";

    vector< ublas::matrix<int> > Matrices;

    for(int i=0;i<alignments.size();i++) {
      const alignment& A = alignments[i];

      int n1 = A.index(name1);
      if (n1 == -1)
	throw myexception(string("sequence ") + name1 + " not found.");

      int n2 = A.index(name2);
      if (n2 == -1)
	throw myexception(string("sequence ") + name2 + " not found.");

      if (Matrices.size() ==0) {
	int l1 = A.seqlength(n1);
	int l2 = A.seqlength(n2);
	Matrices.push_back(ublas::matrix<int>(l1+1,l2+1));
	Matrices.push_back(ublas::matrix<int>(l1+1,l2+1));
	Matrices.push_back(ublas::matrix<int>(l1+1,l2+1));

	for(int m=0;m<3;m++)
	  for(int i=0;i<Matrices[0].size1();i++)
	    for(int j=0;j<Matrices[0].size2();j++)
	      Matrices[m](i,j) = 0;
	      
      }

      vector<int> bits = getorder(A,n1,n2);

      vector<int> cx;
      vector<int> cy;
      getpath(A,bits,cx,cy);

      for(int i=0;i<cx.size()-1;i++) {
	int x1 = cx[i];
	int y1 = cy[i];

	int x2 = cx[i+1];
	int y2 = cy[i+1];

	int m=0;
	if (x2 == x1+1 and y2 == y1+1)
	  m=0;
	else if (x2 == x1+1 and y1 == y2)
	  m = 1;
	else if (x1 == x2 and y2 == y1+1)
	  m = 2;
	else
	  assert(0);

	Matrices[m](x1,y1)++;
      }
    }

    int nlines=0;
    for(int m=0;m<3;m++)
      for(int i=0;i<Matrices[0].size1();i++)
	for(int j=0;j<Matrices[0].size2();j++)
	  if (Matrices[m](i,j)) 
	    nlines++;
	    
    cout<<alignments.size()<<" "<<nlines<<" "<<name1<<" "<<name2<<endl;

    for(int m=0;m<3;m++)
      for(int i=0;i<Matrices[0].size1();i++)
	for(int j=0;j<Matrices[0].size2();j++) {
	  if (Matrices[m](i,j)) {
	    int i2 = i;
	    int j2 = j;
	    if (m ==0) {
	      i2++;
	      j2++;
	    }
	    if (m == 1)
	      i2++;
	    if (m == 2)
	      j2++;
	    cout<<i<<" "<<j<<"  "<<i2<<"  "<<j2<<"  "<<Matrices[m](i,j)<<endl;
	  }
	}
    
    /*---------- Load sampled alignments ----------*/
    // FIXME read PHYLIP alignments - we'll assume that they
    // correctly specify the length
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
