#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>
#include <cmath>

#include <boost/numeric/ublas/matrix.hpp>
#include "sequencetree.H"
#include "util.H"
#include "tree-util.H"
#include "tree-dist.H"

#include <boost/program_options.hpp>

namespace ublas = boost::numeric::ublas;
namespace po = boost::program_options;
using po::variables_map;

using namespace std;

ublas::matrix<double> remove_duplicates(const ublas::matrix<double>& D)
{
  assert(D.size1() == D.size2());
  int N = D.size1();

  // find duplicates
  vector<int> remove;
  for(int i=0;i<N;i++) {
    bool found=false;
    for(int j=0;j<i and not found;j++)
      if (D(i,j) == 0.0)
	found=true;
    if (found)
      remove.push_back(i);
  }

  if (not remove.size()) return D;

  cerr<<"removing "<<remove.size()<<" duplicates."<<endl;

  // compute mapping of old to new indices
  vector<int> indices(N-remove.size());
  int k=0;
  int p=0;
  for(int i=0;i<indices.size();i++)
  {
    while (p<remove.size() and k==remove[p]) {
      k++;
      p++;
    }
    indices[i] = k++;
  }

  // construct the new matrix
  ublas::matrix<double> D2(indices.size(),indices.size());

  for(int i=0;i<D2.size1();i++)
    for(int j=0;j<D2.size2();j++)
      D2(i,j) = D(indices[i],indices[j]);

  return D2;
}

SequenceTree unit_branch_tree(const SequenceTree& T1)
{
  SequenceTree T2 = T1;
  for(int i=0;i<T2.n_branches();i++)
    T2.branch(i).set_length(1.0);
  return T2;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("no-remove-duplicates","don't avoid zeros in off-diagonal zeros")
    ("topology", "Ignore branch lengths")
    ("matrix", "write out the distance matrix")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max",value<int>(),"maximum number of tree samples to read")
    ("sub-sample",value<int>()->default_value(1),"factor by which to sub-sample")
    ("window",value<int>(),"maximum length to consider for autocorrelation")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: tree-dist-autocorrelation < trees-file\n";
    cout<<"Compute the mean lengths for branches in the given topology.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


// use magic-squares...
// That is a good way to get pairs.
// It is still random though.

// We can cull to some specific length... e.g. 1000

// Add trees1 vs trees2 arguments to compare distances from different programs

int main(int argc,char* argv[]) 
{ 
  try 
  {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    bool topology_only = args.count("topology")>0;

    unsigned skip = args["skip"].as<unsigned>();

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    //----------- read in trees ------------//
    vector<SequenceTree> trees = load_trees(cin,skip,subsample,max);

    if (not trees.size())
      throw myexception()<<"No trees were read in!";
  
    cerr<<"# There were "<<trees.size()<<" trees scanned\n";

    // calculate the pairwise distances
    ublas::matrix<double> D(trees.size(),trees.size());
    for(int i=0;i<trees.size();i++) {
      for(int j=0;j<i;j++) {
	double d=0;
	if (topology_only)
	  d = topology_distance(trees[i],trees[j]);
	else
	  d = internal_branch_distance(trees[i],trees[j]);
	D(i,j) = D(j,i) = d;
      }
      D(i,i) = 0;
    }

    if (args.count("matrix")) {
      if (not args.count("no-remove-duplicates"))
	D = remove_duplicates(D);
      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }
    }
    else {

      // set the window size
      int window = int( double(trees.size()/20.0 + 1.0 ) );
      if (args.count("window"))
	window = min(window,args["window"].as<int>());
      
      // bound the window
      if (window >= trees.size()/2)
	window = trees.size()/2;
      cerr<<"# window size = "<<window<<endl;

      // write out the average distances
      valarray<double> distances(0.0,window);
      for(int d=0;d<distances.size();d++) {
	double dd = 0;
	for(int i=0;i+d<trees.size();i++)
	  dd += D(i,i+d);
	distances[d] = dd/(trees.size() - d);
      }
      
      // write out the average distances
      for(int i=0;i<distances.size();i++)
	cout<<i<<"   "<<distances[i]<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
