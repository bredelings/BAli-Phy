#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include <numeric>
#include <set>
#include "myexception.H"
#include "alignment.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment-util.H"
#include "distance-methods.H"
#include "rng.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();

  string tag = "align[";
  tag += args["tag"].as<string>();

  // --------------- Alphabets to try --------------- //
  vector<OwnedPointer<alphabet> > alphabets;
  alphabets.push_back(DNA());
  alphabets.push_back(RNA());
  alphabets.push_back(AminoAcids());

  // --------------------- try ---------------------- //
  std::cerr<<"Loading alignments...";
  list<alignment> As = load_alignments(std::cin,tag,alphabets,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample is empty.";
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("seed", value<unsigned long>(),"random seed")
    ("tag", value<string>()->default_value("sample"),"only read alignments preceded by 'align[<tag>'")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("cutoff",value<double>()->default_value(0.75),"ignore events below this probability")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-consensus [OPTIONS] < in-file\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}



struct Edge 
{
  int s1;
  int x1;

  int s2;
  int x2;

  double p;
};

struct edge_comp {
bool operator()(const Edge& E1, const Edge& E2) {
  return E1.p  > E2.p;
}
};

class Edges: public std::multiset<Edge,edge_comp>
{
public:
};


void add_edges(Edges& E, const vector< ublas::matrix<int> >& Ms,
		   int s1,int s2,int L1, int L2) 
{ 
  ublas::matrix<int> count(L1+1,L2+1);
  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++)
      count(i,j) = 0;


  // get counts of each against each
  for(int i=0;i<Ms.size();i++) {
    const ublas::matrix<int>& M = Ms[i];

    for(int c=0;c<M.size1();c++) {
      int index1 = M(c,s1);
      int index2 = M(c,s2);
      count(index1 + 1, index2 + 1)++;
    }
  }
  count(0,0) = 0;


  // determine Ml pairs
  for(int i=0;i<count.size1();i++) 
    for(int j=0;j<count.size2();j++) 
    {
      if (2*count(i,j) > Ms.size()) {
	Edge e;
	e.s1 = s1;
	e.x1 = i-1;

	e.s2 = s2;
	e.x2 = j-1;

	e.p  = double(count(i,j))/Ms.size();

	E.insert(e);
      }
    }
}

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cerr<<"random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;
    vector<ublas::matrix<int> > Ms;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (not alignments.size()) 
      throw myexception()<<"Didn't read any alignments!";      

    int N = alignments[0].size2();
    assert(alignments[1].size2() == N);
    assert(alignments[1].seqlength(N-1) == alignments[0].seqlength(N-1));

    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));

    vector< vector< vector<int> > >  column_indexes;
    for(int i = 0;i<alignments.size();i++)
      column_indexes.push_back( column_lookup(alignments[i]) );

    //--------- Get list of supported pairs ---------//
    Edges E;

    for(int s1=0;s1<N;s1++) {
      int L1 = alignments[0].seqlength(s1);
      for(int s2=0;s2<s1;s2++) {
	int L2 = alignments[0].seqlength(s2);
	add_edges(E,Ms,s1,s2,L1,L2);
      }
    }

    //--------- Build alignment from list ---------//
    double cutoff = args["cutoff"].as<double>();
    if (cutoff < 0.5) cutoff = 0.5;

    foreach(e,E) {
      if (e->p < cutoff) break;

      std::cout<<"s1 = "<<e->s1<<" x1 = "<<e->x1<<"       s2 = "<<e->s2<<" x2 = "<<e->x2<<"     p = "<<e->p<<"\n";
    }
    
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
