#include <fstream>
#include <string>
#include <map>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"
#include "alignmentutil.H"
#include "util.H"

using std::map;

void do_setup(Arguments& args,vector<alignment>& alignments) {

  /* ----- Alphabets to try ------ */
  vector<alphabet> alphabets;
  alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
  alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
  alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));

  /* ----- Try to load alignments ------ */
  int maxalignments = 1000;
  if (args.set("maxalignments"))
    maxalignments = convertTo<int>(args["maxalignments"]);

  string tag = "align[sample";
  if (args.set("tag"))
    tag = args["tag"];

  alignments = load_alignments(std::cin,tag,alphabets,maxalignments);
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

void write_truck_graph(std::ostream& ofile,const vector<alignment>& alignments,int t1,int t2,
		       const vector<string>& taxa) {

  map<edge,int,edge_lessthan> edges;
	
  /*---------- Count the edges ----------*/
  for(int i=0;i<alignments.size();i++) {
    const alignment& A = alignments[i];

    int n1 = A.index(taxa[t1]);
    if (n1 == -1)
      throw myexception(string("sequence ") + taxa[t1] + " not found.");

    int n2 = A.index(taxa[t2]);
    if (n2 == -1)
      throw myexception(string("sequence ") + taxa[t2] + " not found.");

    vector<int> bits = getorder(A,n1,n2);
      
    vector<int> cx;
    vector<int> cy;
    getpath(A,bits,cx,cy);

    for(int j=0;j<cx.size()-1;j++) {
      int x1 = cx[j];
      int y1 = cy[j];

      int x2 = cx[j+1];
      int y2 = cy[j+1];

      edges[edge(x1,y1,x2,y2)]++;
    }
  }
  
  /*----------- Write out header ----------*/
  ofile<<alignments.size()<<" "<<edges.size()<<" "<<taxa[t1]<<" "<<taxa[t2]<<endl;

  /*----------- Write out edges and counts ----------*/
  for(map<edge,int,edge_lessthan>::iterator here = edges.begin();
      here != edges.end();here++) {
    ofile<<here->first.x1<<" "
	<<here->first.y1<<" "
	<<here->first.x2<<" "
	<<here->first.y2<<" "
	<<here->second<<endl;
  }
}


int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    /*--------------------- Get taxon names --------------------*/
    if (not args.set("taxa") or args["taxa"] == "")
      throw myexception(string("No taxa specified [taxa=name1:name2:...]"));

    vector<string> taxa = split(args["taxa"],':');

    /*---------------- Load alignment and tree -----------------*/
    vector<alignment> alignments;
    do_setup(args,alignments);

    std::cerr<<"Read "<<alignments.size()<<" alignments\n";

    /*-------- Print the probabilistic truck graph for each pair -------*/
    for(int t1=0;t1<taxa.size();t1++)
      for(int t2=0;t2<t1;t2++)
	write_truck_graph(std::cout,alignments,t1,t2,taxa);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
