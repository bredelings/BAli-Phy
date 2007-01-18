// FIXME -  Try to put the variance and stuff on one line:
//   0.656 (56/100)   +- 0.007 -> 0.070
#ifdef NDEBUG
#undef NDEBUG
#endif

#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <cmath>
#include <fstream>
#include <boost/numeric/ublas/io.hpp>

#include "mytypes.H"
#include "myexception.H"
#include "sequencetree.H"
#include "util.H"
#include "statistics.H"
#include "bootstrap.H"
#include "tree-dist.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

using std::cout;
using std::cerr;
using std::endl;

using std::pair;

using namespace statistics;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

struct edge {
  int from;
  int to;
  int type;
  int partition;

  edge(int f,int t,int T,int p)
    :from(f),to(t),type(T),partition(p)
  {}
};

struct MC_tree {
  vector<Partition> partitions;
  int n_nodes;
  vector<edge> edges;

  const vector<string>& names() const {return partitions[0].names;}
};


double getsum(const valarray<double>& v) {
  return v.sum();
}


unsigned changes(const valarray<bool>& sample,bool value) 
{
  unsigned count=0;
  for(int i=0;i<sample.size()-1;i++) {
    if (sample[i] == value and sample[i+1] != value)
      count++;
  }
  return count;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("file",value<string>(),"predicates to examine")
    ;
  
  options_description all("All options");
  all.add(input);

  // positional options
  positional_options_description p;
  p.add("file", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: draw-graph <file1>\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("file"))
    throw myexception()<<"No file supplied.";

  return args;
}

vector<int> get_cliques(const ublas::matrix<int>& connected)
{
  const int N = connected.size1();
  vector<int> mapping(N,-1);

  int n_cliques = 0;

  for(int i=0;i<N;i++) {
    if (mapping[i] != -1) continue;

    vector<int> nodes;
    nodes.push_back(i);
    mapping[i] = n_cliques;

    for(int j=0;j<N;j++)
      if (connected(i,j)) {
	assert(connected(j,i));
	assert(mapping[j] == -1);
	mapping[j] = n_cliques;
	nodes.push_back(j);
      }

    for(int j=0;j<nodes.size();j++)
      for(int k=0;k<j;k++)
	assert(connected(nodes[j],nodes[k]));

    n_cliques++;
  }

  for(int i=0;i<N;i++)
    assert(mapping[i] != -1);

  return mapping;
}

// FIXME - page="8.5,11" ?
void draw_graph(const MC_tree& T,const string& name)
{
  const int N = T.n_nodes;

  cout<<"digraph "<<name<<" { \n\
\n\
      nodesep=1.0\n\
      ratio=auto\n\
\n\
      node[shape=none,width=auto]\n\n";

  // edges
  for(int i=0;i<T.edges.size();i++) {
    const edge& e = T.edges[i];
    cout<<"      N"<<e.from<<" -> N"<<e.to;

    vector<string> attributes;
    vector<string> styles;

    if (e.type == 1) {
      attributes.push_back("arrowhead=none");
      const Partition& p = T.partitions[e.partition];
      if (informative(p))
	styles.push_back("setlinewidth(2)");
      if (not p.full())
	attributes.push_back("color=green2");
    }
    else {
      styles.push_back("dashed");
      attributes.push_back("weight=0");
    }

    if (styles.size()) {
      string style = "style=\"" + join(styles,',') + "\"";
      attributes.push_back(style);
    }
    
    cout<<" ["<<join(attributes,',')<<"]";
    cout<<"\n";
  }
  cout<<endl;

  // leaf names
  vector<string> names = T.names();
  for(int i=0;i<names.size();i++)
    cout<<"      N"<<i<<" [label=\""<<names[i]<<"\"]\n";
  cout<<endl;

  for(int i=names.size();i<N;i++)
    cout<<"      N"<<i<<" [label=\"\",shape=circle,hight=0.02,width=0.02,fontsize=1]\n";
  cout<<endl;

  cout<<"}\n";

}

int main(int argc,char* argv[]) 
{ 
  try {

    cout.precision(3);
    cout.setf(ios::fixed);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load Partitions ---------------//
    vector<vector<Partition> > partitions1;
    string filename = args["file"].as<string>();
    load_partitions(filename, partitions1);

    vector<Partition> partitions = partitions1[0];

    // check that the taxon names are all the same
    vector<string> names = partitions[0].names;
    for(int i=0;i<partitions.size();i++) {

      if (partitions[i].size() != names.size())
	throw myexception()<<"Partition "<<i+1<<" has "<<partitions[i].size()<<" taxa instead of "<<partitions[0].size();

      if (partitions[i].names != names)
	throw myexception()<<"Partition "<<i+1<<" has different taxa than partition 1!";
      if (not informative(partitions[i]))
	throw myexception()<<"Partition "<<i+1<<" is not informative.";
    }

    vector<Partition> partitions_old = partitions;
    partitions = get_moveable_tree(partitions);
    // check that the tree is an MC tree
    if (partitions.size() != partitions_old.size())
      cerr<<"Removing "<<partitions_old.size() - partitions.size()<<"/"<<partitions_old.size()<<" partitions to yield an MC  tree."<<endl;

    // add leaf branches
    for(int i=0;i<names.size();i++) {
      valarray<bool> m(true,names.size());
      m[i] = false;
      partitions.insert(partitions.begin()+i,Partition(names,m));
    }

    // add reversed branches
    const int N = partitions.size();
    for(int i=0;i<N;i++)
      partitions.push_back(partitions[i].reverse());

    // left_of
    ublas::matrix<int> left_of(2*N,2*N);
    for(int i=0;i<2*N;i++)
      for(int j=0;j<2*N;j++)
	left_of(i,j) = partition_less_than(partitions[i],partitions[j])?1:0;

    // wanders_over
    ublas::matrix<int> wanders_over(2*N,2*N);
    for(int i=0;i<2*N;i++)
      for(int j=0;j<2*N;j++)
	wanders_over(i,j) = partition_wanders_over(partitions[i],partitions[j])?1:0;

    // directly_left_of
    ublas::matrix<int> directly_left_of = left_of;
    for(int i=0;i<2*N;i++)
      for(int j=0;j<2*N;j++)
	if (left_of(i,j))
	  for(int k=0;k<2*N;k++)
	    if (left_of(i,k) and left_of(k,j))
	      directly_left_of(i,j)=0;


    // directly_wanders_over
    ublas::matrix<int> directly_wanders_over = wanders_over;
    for(int i=0;i<2*N;i++)
      for(int j=0;j<2*N;j++)
	if (wanders_over(i,j))
	  for(int k=0;k<2*N;k++)
	    if (left_of(i,k) and wanders_over(k,j))
	      directly_wanders_over(i,j)=0;

    vector<bool> directly_wanders(2*N,false);
    for(int i=0;i<2*N;i++)
      for(int j=0;j<2*N;j++)
	if (directly_wanders_over(i,j))
	  directly_wanders[i] = true;

    // connected_to
    ublas::matrix<int> connected_to(2*N,2*N);
    for(int i=0;i<2*N;i++)
      for(int j=0;j<2*N;j++)
	if (directly_wanders[i] or directly_wanders[j])
	  connected_to(i,j)=0;
	else
	  connected_to(i,j) = directly_left_of(i, (j+N)%(2*N) );

    /*
    for(int i=0;i<2*N;i++)
      for(int j=0;j<i;j++)
	if (connected_to(i,j)) {
	  cerr<<"("<<partitions[i]<<") <-> ("<<partitions[j]<<")\n";
	  assert(connected_to(j,i));
	}
    */

    // map nodes to clique indices
    vector<int> mapping = get_cliques(connected_to);
    
    int C = max(mapping)+1;

    MC_tree T;
    T.partitions = partitions;
    T.n_nodes = C;

    // connected
    ublas::matrix<int> connected(C,C);
    for(int i=0;i<C;i++)
      for(int j=0;j<C;j++)
	connected(i,j) = 0;

    for(int i=0;i<N;i++) {
      assert(not connected_to(i,i+N));
      assert(mapping[i] != mapping[i+N]);
      connected(mapping[i],mapping[i+N])=1;
      connected(mapping[i+N],mapping[i])=1;
      T.edges.push_back(edge(mapping[i],mapping[i+N],1,i));
    }

    for(int i=0;i<2*N;i++) 
      for(int j=0;j<2*N;j++) 
	if (directly_wanders_over(i,j)) {
	  assert(directly_wanders_over(i,(j+N)%(2*N)));
	  connected(mapping[i],mapping[j])=2;
	}

    for(int i=0;i<C;i++)
      for(int j=0;j<C;j++)
	if (connected(i,j)==2)
	  T.edges.push_back(edge(i,j,2,-1));

    // remove the pathname 
    while(filename.find('/') != -1) 
      filename = filename.substr(filename.find('/')+1);

    // remove the extension
    int dot = filename.find('.');
    string name = filename;
    if (dot != -1)
      name = filename.substr(0,dot);

    //draw the graph
    draw_graph(T,name);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

