#include <iostream>
#include <list>
#include <utility>
#include "tree.H"
#include "sequencetree.H"
#include "tree-util.H"
#include "tree-dist.H"
#include "myexception.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::list;
using std::valarray;
using std::pair;

// mark nodes in T according to what node of Q they map to
vector<int> get_nodes_map(const SequenceTree& Q,const SequenceTree& T,
			  const vector<int>& branches_map)
{
  assert(branches_map.size() == Q.n_branches() * 2);

  vector<int> nodes_map(T.n_nodes(),-1);

  // map nodes from T -> Q that are in both trees
  for(int b=0;b<Q.n_branches();b++)
  {
    int Q_source = Q.branch(b).source();
    int Q_target = Q.branch(b).target();

    int b2 = branches_map[b];

    int T_source = T.directed_branch(b2).source();
    int T_target = T.directed_branch(b2).target();

    if (nodes_map[T_source] == -1)
      nodes_map[T_source] = Q_source;
    else
      assert(nodes_map[T_source] == Q_source);

    if (nodes_map[T_target] == -1)
      nodes_map[T_target] = Q_target;
    else
      assert(nodes_map[T_target] == Q_target);
  }

  // map the rest of the nodes from T -> Q
  for(int i=Q.n_leaves();i<Q.n_nodes();i++) 
  {
    unsigned D = Q[i].degree();
    if (D <= 3) continue;

    // get a branch of Q pointing into the node
    const_branchview outside = *(Q[i].branches_in());
    // get a branch of T pointing into the node
    outside = T.directed_branch(branches_map[outside.name()]);

    list<const_branchview> branches;
    typedef list<const_branchview>::iterator list_iterator;
    append(outside.branches_after(),branches);
    for(list_iterator b = branches.begin() ; b != branches.end();)
    {
      int node = (*b).target();
      if (nodes_map[node] == -1)
	nodes_map[node] = i;

      if (nodes_map[node] == i) {
	append((*b).branches_after(),branches);
	b++;
      }
      else {
	list_iterator prev = b;
	b++;
	branches.erase(prev);
      }
    }
    assert(branches.size() == D-3);
  }

  for(int i=0;i<nodes_map.size();i++)
    assert(nodes_map[i] != -1);

  return nodes_map;
}


bool update_lengths(const SequenceTree& Q,const SequenceTree& T,
		    valarray<double>& branch_lengths, 
		    valarray<double>& branch_lengths_squared, 
		    valarray<double>& node_lengths)
{
  // map branches from Q -> T
  vector<int> branches_map = extends_map(T,Q);
  if (not branches_map.size())
    return false;

  // incorporate lengths of branches that map to Q
  for(int b=0;b<Q.n_branches();b++)
  {
    int b2 = branches_map[b];
    double L = T.directed_branch(b2).length();
    branch_lengths[b] += L;
    branch_lengths_squared[b] += L*L;
  }

  // map nodes from T -> Q
  vector<int> nodes_map = get_nodes_map(Q,T,branches_map);

  // incorprate lengths of branches that map to nodes in Q
  for(int i=T.n_leafbranches();i<T.n_branches();i++) 
  {
    const_branchview b = T.branch(i);
    int n1 = nodes_map[b.source()];
    int n2 = nodes_map[b.target()];

    if (n1 == n2)
      node_lengths[n1] += T.branch(i).length();
  }

  return true;
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("tree", value<string>(),"tree to re-root")
    ("skip",value<int>()->default_value(0),"number of tree samples to skip")
    ("max",value<int>(),"maximum number of tree samples to read")
    ("sub-sample",value<int>()->default_value(1),"factor by which to sub-sample")
    ("var","report standard deviation of branch lengths instead of mean")
    ("no-node-lengths","ignore branches not in the specified topology")
    ("safe","Don't die if no trees match the topology")
    ("special","Output special format")
    ;

  // positional options
  positional_options_description p;
  p.add("tree", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: tree-mean-lengths <tree-file> < in-file\n";
    cout<<"Compute the mean lengths for branches in the given topology.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


struct accum_branch_lengths: public accumulator<SequenceTree>
{
  int n_samples;
  int n_matches;

  SequenceTree Q;

  valarray<double> m1;
  valarray<double> m2;
  valarray<double> n1;

  void operator()(const SequenceTree&);

  void finalize() 
  {
    if (n_samples == 0)
      throw myexception()<<"No trees were read in!";
  
    if (n_matches == 0)
      throw myexception()<<"No trees matched the specified topology!";

    m1 /= n_matches;
    m2 /= n_matches;
    n1 /= n_matches;

    m2 -= m1*m1;
    m2 = sqrt(m2);
  }

  accum_branch_lengths(const SequenceTree T)
    :
    n_samples(0),
    n_matches(0),
    Q(T),
    m1(0.0, Q.n_branches()),
    m2(0.0, Q.n_branches()),
    n1(0.0, Q.n_nodes())
  {}
};

void accum_branch_lengths::operator()(const SequenceTree& T)
{
  n_samples++;
  if (update_lengths(Q,T,m1,m2,n1))
    n_matches++;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  ----------//
    variables_map args = parse_cmd_line(argc,argv);

    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    int subsample = args["sub-sample"].as<int>();

    //----------- Read the topology -----------//
    SequenceTree Q = load_T(args);
    standardize(Q);
    const int B = Q.n_branches();
    const int N = Q.n_nodes();

    //-------- Read in the tree samples --------//
    accum_branch_lengths A(Q);

    try {
      scan_trees(std::cin,skip,subsample,max,A);
    }
    catch (std::exception& e) 
    {
      if (args.count("safe"))
	cout<<Q.write(false)<<endl;
      std::cerr<<"Exception: "<<e.what()<<endl;
      exit(0);
    }

    std::cerr<<A.n_matches<<" out of "<<A.n_samples<<" trees matched the topology";
    std::cerr<<" ("<<double(A.n_matches)/A.n_samples*100<<"%)"<<std::endl;

    //------- Merge lengths and topology -------//
    if (args.count("special")) {
      for(int b=0;b<Q.n_branches();b++) {
	cout<<"branch "<<A.m1[b]<<endl;
	cout<<partition_from_branch(Q,b)<<endl;
      }
      for(int n=0;n<Q.n_nodes();n++) {
	if (A.n1[n] > 0) {
	  cout<<"node "<<A.n1[n]<<endl;
	  int b = (*Q[n].branches_in()).name();
	  cout<<partition_from_branch(Q,b)<<endl;
	}
      }
      exit(0);
    }

    if (args.count("var"))
      for(int b=0;b<B;b++)
	Q.branch(b).set_length(A.m2[b]);
    else 
    {
      for(int b=0;b<B;b++)
	Q.branch(b).set_length(A.m1[b]);

      if (not args.count("no-node-lengths")) {
	for(int n=0;n<N;n++) {
	  int degree = Q[n].neighbors().size();
	  for(out_edges_iterator b = Q[n].branches_out();b;b++)
	    (*b).set_length((*b).length() + A.n1[n]/degree);
	}
      }
    }

    //------- Merge lengths and topology -------//

    std::cout<<Q<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
