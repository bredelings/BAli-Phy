#include <fstream>
#include <string>
#include <map>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"

using std::map;
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

struct edge {
  int x1;
  int y1;
  int x2;
  int y2;
  edge(int a, int b, int c, int d):x1(a),y1(b),x2(c),y2(d) {}
};

struct edge_lessthan {
  bool operator()(const edge& e1,const edge& e2) const {
    if (e1.x1 != e2.x1)
      return (e1.x1 < e2.x1);
    else if (e1.y1 != e2.y1)
      return (e1.y1 < e2.y1);
    else if (e1.x2 != e2.x2)
      return (e1.x2 < e2.x2);
    else 
      return (e1.y2 < e2.y2);
  }
};

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

    map<edge,int,edge_lessthan> edges;

    /*---------- Count the edges ----------*/
    int nlines=0;
    for(int i=0;i<alignments.size();i++) {
      const alignment& A = alignments[i];

      int n1 = A.index(name1);
      if (n1 == -1)
	throw myexception(string("sequence ") + name1 + " not found.");

      int n2 = A.index(name2);
      if (n2 == -1)
	throw myexception(string("sequence ") + name2 + " not found.");

      vector<int> bits = getorder(A,n1,n2);

      vector<int> cx;
      vector<int> cy;
      getpath(A,bits,cx,cy);

      for(int i=0;i<cx.size()-1;i++) {
	int x1 = cx[i];
	int y1 = cy[i];

	int x2 = cx[i+1];
	int y2 = cy[i+1];

	edges[edge(x1,y1,x2,y2)]++;
	nlines++;
      }
    }

    cout<<alignments.size()<<" "<<nlines<<" "<<name1<<" "<<name2<<endl;

    /*----------- Write out edges and counts ----------*/
    for(map<edge,int,edge_lessthan>::iterator here = edges.begin();
	here != edges.end();here++) {
      cout<<here->first.x1<<" "
	  <<here->first.y1<<" "
	  <<here->first.x2<<" "
	  <<here->first.y2<<" "
	  <<here->second<<endl;
    }
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
