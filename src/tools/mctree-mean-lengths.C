#ifdef NDEBUG
#undef NDEBUG
#endif
#include <iostream>
#include <list>
#include <utility>
#include "tree.H"
#include "sequencetree.H"
#include "tree-util.H"
#include "tree-dist.H"
#include "myexception.H"
#include "mctree.H"
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


bool update_lengths(const MC_tree& Q,const SequenceTree& T,
		    valarray<double>& branch_lengths, 
		    valarray<double>& node_lengths)
{
  // check that this tree is consistent with the MC Tree Q
  for(int i=0;i<Q.branch_order.size();i++) 
  {
    int b = Q.branch_order[i];
    if (not implies(T,Q.partitions[b]))
      return false;
  }

  // compute node masks
  vector<valarray<bool> > node_masks;
  for(int n=0;n<Q.n_nodes();n++)
  {
    cout<<"node "<<Q.partitions[Q.branch_to_node(n)]<<endl;
    cout<<"degree = "<<Q.degree(n)<<endl;
    valarray<bool> mask(true,Q.n_leaves());
    for(int i=0;i<2*Q.n_branches();i++)
    {
      if (Q.mapping[i] == n and not Q.directly_wanders[i])
	mask = mask and Q.partitions[i].mask();
    }
    node_masks.push_back(mask);
    cout<<"mask = ";
    for(int i=0;i<mask.size();i++)
      if (mask[i])
	cout<<Q.names()[i]<<" ";
    cout<<endl;
    cout<<endl;
  }

  // incorporate lengths of branches that map to Q
  vector<Partition> partitions2;
  for(int i=0;i<Q.branch_order.size();i++) 
  {
    int b = Q.branch_order[i];
    int n1 = Q.mapping[b];
    int n2 = Q.mapping[Q.reverse(b)];

    Partition P = Q.partitions[b];
    valarray<bool> mask = P.mask();

    // find non-wandering branches directly left of me
    for(int j=0;j<2*Q.n_branches();j++)
    {
      if (Q.directly_left_of(j,b) and 
	  not Q.directly_wanders[j])
	mask = mask and Q.partitions[j].mask();
    }

    // find non-wandering branches directly right of me
    for(int j=0;j<2*Q.n_branches();j++)
    {
      if (Q.directly_left_of(b,j) and 
	  not Q.directly_wanders[Q.reverse(j)])
	mask = mask and Q.partitions[j].mask();
    }

    // partition masks should be computable from the masks
    //    of both endpoint nodes.
    // but how about degree=0 nodes? this shouldn't work then
    assert(equal(mask,node_masks[n1] and node_masks[n2]));

    P.group1 = P.group1 and mask;
    P.group2 = P.group2 and mask;

    partitions2.push_back(P);
  }

  // map branches of the input tree
  for(int b=0;b<T.n_branches();b++)
  {
    Partition P = partition_from_branch(T,b);
    vector<int> branches;
    for(int i=0;i<Q.branch_order.size();i++) 
    {
      if (implies(P,partitions2[i]))
	branches.push_back(i);
    }

    vector<int> nodes;
    for(int n=0;n<Q.n_nodes();n++) 
    {
      if (Q.degree(n) == 0) continue;

      Partition P2 = P;
      P2.group1 = P2.group1 and node_masks[n];
      P2.group2 = P2.group2 and node_masks[n];
      if (n_elements(P2.group1) == 0 or n_elements(P2.group2) == 0)
	continue;

      bool ok = true;
      int count = 0;
      for(int b=0;b<2*Q.n_branches() and ok;b++)
      {
	if (Q.mapping[b] != n)  continue;
	count++;
	Partition P3 = Q.partitions[b];
	P3.group1 = P3.group1 and node_masks[n];
	P3.group2 = P3.group2 and node_masks[n];
	if (partition_less_than(P3,P2) or partition_less_than(P3,P2.reverse()))
	  ;
	else
	  ok=false;
      }

      if (ok) {
	nodes.push_back(n);
	assert(count == Q.degree(n));
      }
    }

    cout<<"Branch maps to "<<branches.size()<<" branches."<<endl;
    if (nodes.size()) {
      cout<<"Branch inside node(s):"<<endl;
      cout<<P<<endl;
      for(int i=0;i<nodes.size();i++)
	cout<<"    "<<Q.partitions[Q.branch_to_node(nodes[i])]<<endl;
    }
    assert(nodes.size() < 2);
    if (branches.size()) assert(not nodes.size());
    assert(branches.size() + nodes.size() > 0);

    const double L = T.branch(b).length();

    for(int i=0;i<branches.size();i++)
      branch_lengths[branches[i]] += L/branches.size();

    for(int i=0;i<nodes.size();i++)
      node_lengths[nodes[i]] += L/nodes.size();
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
    ("drop-partial","Remove partial branches")
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

  MC_tree Q;

  valarray<double> m1;
  valarray<double> n1;

  void operator()(const SequenceTree&);

  void finalize() 
  {
    if (n_samples == 0)
      throw myexception()<<"No trees were read in!";
  
    if (n_matches == 0)
      throw myexception()<<"No trees matched the specified topology!";

    m1 /= n_matches;
    n1 /= n_matches;
  }

  accum_branch_lengths(const MC_tree& T)
    :
    n_samples(0),
    n_matches(0),
    Q(T),
    m1(0.0, Q.n_branches()),
    n1(0.0, Q.n_nodes())
  {}
};

void accum_branch_lengths::operator()(const SequenceTree& T)
{
  n_samples++;
  if (update_lengths(Q,T,m1,n1))
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
    MC_tree Q = load_MC_tree(args["tree"].as<string>());

    //-------- Read in the tree samples --------//
    accum_branch_lengths A(Q);

    try {
      scan_trees(std::cin,skip,subsample,max,A);
    }
    catch (std::exception& e) 
    {
      if (args.count("safe"))
	cout<<Q<<endl;
      throw myexception()<<e.what();
    }

    std::cerr<<A.n_matches<<" out of "<<A.n_samples<<" trees matched the topology";
    std::cerr<<" ("<<double(A.n_matches)/A.n_samples*100<<"%)"<<std::endl;

    //------- Merge lengths and topology -------//
    for(int i=0;i<Q.branch_order.size();i++) {
      int b = Q.branch_order[i];
      cout<<"branch "<<A.m1[i]<<endl;
      cout<<Q.partitions[b]<<endl;
    }

    for(int n=0;n<Q.n_nodes();n++) 
    {
      if (A.n1[n] > 0) {
	cout<<"node "<<A.n1[n]<<endl;
	int b = Q.branch_to_node(n);
	//FIXME! don't reverse this - use the L->R direction to point to the node
	cout<<Q.partitions[Q.reverse(b)]<<endl;
      }
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
