#include <iostream>
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
using std::valarray;
using std::pair;

int find_branch(const SequenceTree& T,const valarray<bool>& partition2)
{
  valarray<bool> partition1(T.n_leaves());

  const_branchview current = T.directed_branch(0);

  // check the leaf branch 0 
  partition1 = T.partition(0);
  if (equal(partition1,partition2) or equal(partition1,not partition2))
    return 0;

  while (true)
  {
    int outside = 0;
    for(const_out_edges_iterator b = current.branches_after();b;b++) 
    {
      partition1 = T.partition(*b);

      int bb = intersect(partition1,partition2)?1:0;
      int ba = intersect(partition1,not partition2)?1:0;
      int ab = intersect(not partition1,partition2)?1:0;
      int aa = intersect(not partition1,not partition2)?1:0;
      
      int total = aa + ab + ba + bb;
      
      // partition contradicts 'current'
      if (total == 4)
	return -1;

      // partition equals 'current'
      else if (total == 2)
	return (*b).undirected_name();

      // partition is after 'current'
      else if (ba and bb) {
	outside++;
	current = *b;
      }
      // partition is before 'current' - or we have a problem!
      else 
	assert(aa and ab);
    }
    assert(outside == 1);
  }
}


pair<int,bool> find_branch_or_node(const SequenceTree& T,const valarray<bool>& partition2)
{
  valarray<bool> partition1(T.n_leaves());

  const_branchview current = T.directed_branch(0);

  // check the leaf branch 0 
  partition1 = T.partition(0);
  if (equal(partition1,partition2) or equal(partition1,not partition2))
    return std::pair<int,bool>(0,true);

  while (true) 
  {
    int outside = 0;
    for(const_out_edges_iterator b = current.branches_after();b;b++) 
    {
      partition1 = T.partition(*b);

      int bb = intersect(partition1,partition2)?1:0;
      int ba = intersect(partition1,not partition2)?1:0;
      int ab = intersect(not partition1,partition2)?1:0;
      int aa = intersect(not partition1,not partition2)?1:0;
      
      int total = aa + ab + ba + bb;
      
      // partition contradicts 'current'
      if (total == 4)
	return pair<int,bool>(-1,true);

      // partition equals 'current'
      else if (total == 2)
	return pair<int,bool>((*b).undirected_name(),true);

      // partition is after 'current'
      else if (ba and bb) {
	outside++;
	current = *b;
      }
      // partition is before 'current' - or we have a problem!
      else 
	assert(aa and ab);
    }
    assert(outside <= 1);

    if (outside == 0)
      return pair<int,bool>(current.target(),false);
  }
}


bool update_lengths(const SequenceTree& T,const SequenceTree& sample,
		    valarray<double>& branch_lengths, valarray<double>& node_lengths)
{
  valarray<double> branches(T.n_branches());
  valarray<double> nodes(T.n_nodes());

  valarray<bool> partition1(T.n_leaves());
  valarray<bool> partition2(T.n_leaves());

  for(int b2=0;b2<sample.n_branches();b2++) {

    partition2 = sample.partition(b2);

    assert(b2 == find_branch(sample,partition2));

    // map branch b2 in sample to branch/node in T
    pair<int,bool> result = find_branch_or_node(T,partition2);

    // do nothing if the topologies conflict
    if (result.first == -1)
      return false;
    
    // store branch lengths if we map to a branch
    if (result.second)
      branches[result.first] += sample.branch(b2).length();
    // store node lengths if we map to a node
    else
     nodes[result.first] += sample.branch(b2).length();
  }


  branch_lengths += branches;
  node_lengths += nodes;

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
    cout<<"Usage: tree-get-lengths <tree-file> < in-file\n";
    //    cout<<all<<"\n";
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
    SequenceTree topology = load_T(args);
    standardize(topology);
    
    //-------- Read in the tree samples --------//
    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    valarray<double> branch_lengths(0.0,topology.n_branches());
    valarray<double> node_lengths(0.0,topology.n_branches());

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

      if (update_lengths(topology,T,branch_lengths,node_lengths))
	n_matches++;
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
      topology.branch(b).set_length(branch_lengths[b]); 

    if (not args.count("no-node-lengths")) {
      for(int n=0;n<node_lengths.size();n++) {
	int degree = topology[n].neighbors().size();
	for(out_edges_iterator b = topology[n].branches_out();b;b++)
	  (*b).set_length((*b).length() + node_lengths[n]/degree);
      }
    }

    //------- Merge lengths and topology -------//

    std::cout<<topology<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
