#include <fstream>
#include <string>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;

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

    cout<<alignments.size()<<" "<<name1<<" "<<name2<<endl;
    for(int i=0;i<alignments.size();i++) {
      const alignment& A = alignments[i];
      int n1 = A.index(name1);
      int n2 = A.index(name2);

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
