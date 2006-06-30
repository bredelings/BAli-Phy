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
    branch_lengths[b] += T.directed_branch(b2).length();
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
    ("no-node-lengths","ignore branches not in the specified topology")
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
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  ----------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Read in the topology ----------//
    SequenceTree Q = load_T(args);
    standardize(Q);
    
    //-------- Read in the tree samples --------//
    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    valarray<double> branch_lengths(0.0,Q.n_branches());
    valarray<double> node_lengths(0.0,Q.n_nodes());

    int lines=0;
    string line;
    int n_samples=0;
    int n_matches=0;
    while(getline(std::cin,line)) {

      // don't start if we haven't skipped enough trees
      if (lines++ < skip) continue;

      // quit if we've read in 'max' trees
      if (max >= 0 and n_samples == max) break;

      //--------- Count how many of each topology -----------//
      SequenceTree T;
      try {
	// This should make all the branch & node numbers the same if the topology is the same
	T = standardized(line);
      }
      catch (std::exception& e) {
	std::cerr<<"Exception: "<<e.what()<<endl;
	std::cerr<<" Quitting read of tree file"<<endl;
	break;
      }

      if (update_lengths(Q,T,branch_lengths,node_lengths)) {
	n_matches++;
      }
      n_samples++;
    }

    if (n_samples == 0)
      throw myexception()<<"No trees were read in!";
  
    if (n_matches == 0)
      throw myexception()<<"No trees matched the specified topology!";
  
    std::cerr<<n_matches<<" out of "<<n_samples<<" trees matched the topology";
    std::cerr<<" ("<<double(n_matches)/n_samples*100<<"%)"<<std::endl;

    //------- Merge lengths and topology -------//

    branch_lengths /= n_matches;
    node_lengths /= n_matches;

    for(int b=0;b<branch_lengths.size();b++)
      Q.branch(b).set_length(branch_lengths[b]); 

    if (not args.count("no-node-lengths")) {
      for(int n=0;n<node_lengths.size();n++) {
	int degree = Q[n].neighbors().size();
	for(out_edges_iterator b = Q[n].branches_out();b;b++)
	  (*b).set_length((*b).length() + node_lengths[n]/degree);
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
