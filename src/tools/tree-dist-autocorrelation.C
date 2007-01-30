#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>
#include <cmath>
#include <fstream>
#include <boost/numeric/ublas/matrix.hpp>
#include "statistics.H"

#include "sequencetree.H"
#include "util.H"
#include "tree-util.H"
#include "tree-dist.H"

#include <boost/program_options.hpp>

namespace ublas = boost::numeric::ublas;
namespace po = boost::program_options;
using po::variables_map;

using namespace std;
using namespace statistics;

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
  options_description invisible("Invisible options");
  invisible.add_options()
    ("files",value<vector<string> >()->composing(),"tree samples to examine")
    ;

  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max",value<int>(),"maximum number of tree samples to read")
    ("sub-sample",value<int>()->default_value(1),"factor by which to sub-sample")
    ;

  options_description analysis("Analysis options");
  analysis.add_options()
    ("analysis", value<string>()->default_value("matrix"), "Analysis: matrix, autocorrelation, diameter, compare, convergence, converged,")
    ("metric", value<string>()->default_value("topology"),"Tree distance: topology, branch, internal-branch")
    ("no-remove-duplicates","[matrix]: allow zero distances  between points.")
    ("max-lag",value<int>(),"[autocorrelation]: max lag to consider.")
    ;

  options_description visible("All options");
  visible.add(input).add(analysis);

  options_description all("All options");
  all.add(invisible).add(input).add(analysis);

  // positional options
  positional_options_description p;
  p.add("analysis", 1);
  p.add("files", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: trees-distances <analysis> trees-file1 [trees-file2 ...]\n";
    cout<<"Compute autocorrelations or other functions of tree distances.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

typedef double (*tree_metric_fn)(const SequenceTree&,const SequenceTree&);

ublas::matrix<double> distances(const vector<SequenceTree>& trees, 
				tree_metric_fn metric_fn
				)
{
    ublas::matrix<double> D(trees.size(),trees.size());

    // calculate the pairwise distances
    for(int i=0;i<trees.size();i++) {
      for(int j=0;j<i;j++)
	D(i,j) = D(j,i) = metric_fn(trees[i],trees[j]);
      D(i,i) = 0;
    }
    return D;
}

double distance(const SequenceTree& T, 
		const vector<SequenceTree>& trees,
		tree_metric_fn metric_fn
		)
{
  double D=0;
  for(int i=0;i<trees.size();i++)
    D += metric_fn(T,trees[i]);
  D /= trees.size();
  return D;
}

void check_supplied_filenames(int n,vector<string>& files)
{
  if (files.size() == n-1)
    files.insert(files.begin(),"-");
  if (files.size() == n)
    return;

  if (files.size() < n)
    throw myexception()<<"Wanted "<<n<<" filenames, but got only "<<files.size()<<".";
  if (files.size() > n)
    cerr<<"Warning: ignoring "<<files.size()-n<<" extra filenames."<<endl;
}

vector<SequenceTree> load_trees(string filename,int skip,int subsample,int max)
{
  vector<SequenceTree> trees;
  if (filename == "-") {
    filename = "STDIN";
    trees = load_trees(cin,skip,subsample,max);
  }
  else {
    ifstream file(filename.c_str());
    if (not file)
      throw myexception()<<"Can't open file '"<<filename<<"'.";
    trees = load_trees(file,skip,subsample,max);
    file.close();
  }
  for(int i=0;i<trees.size();i++)
    standardize(trees[i]);
  if (not trees.size())
    throw myexception()<<filename<<": no trees were read in!";
  cerr<<filename<<": scanned "<<trees.size()<<" trees.\n";
  return trees;
}

void report_distances(const valarray<double>& distances,const string& name=string(""))
{
  cout<<"  E D"<<name<<" = "<<distances.sum()/distances.size();
  cout<<"   [+- "<<sqrt(Var(distances))<<"]"<<endl;
  
  pair<double,double> interval = confidence_interval(distances,0.95);
  cout<<"    D"<<name<<" ~ "<<median(distances);
  cout<<"   ("<<interval.first<<", "<<interval.second<<")"<<endl;
}

void diameter(const ublas::matrix<double>& D,const string& name=string(""))
{
  const unsigned N = D.size1();

  if (N == 1) {
    cout<<"    D = 0   (1 pt)"<<endl;
    return;
  }

  double diameter = 0;
  valarray<double> distances(0.0, N);
  for(int i=0;i<D.size1();i++)
    for(int j=0;j<i;j++) {
      diameter = std::max(diameter,D(i,j));
      distances[i] += D(i,j);
      distances[j] += D(i,j);
    }
  distances /= (N-1);

  cout<<"max D"<<name<<" = "<<diameter<<endl;
  report_distances(distances,name);
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

    string analysis = args["analysis"].as<string>();

    unsigned skip = args["skip"].as<unsigned>();

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    tree_metric_fn metric_fn = NULL;
    string metric = args["metric"].as<string>();
    if (metric == "topology" or metric == "RF")
      metric_fn = &robinson_foulds_distance;
    else if (metric == "branch")
      metric_fn = &branch_distance;
    else if (metric == "internal-branch")
      metric_fn = &internal_branch_distance;
      
    //----------- read in trees ------------//
    vector<string> files;
    if (args.count("files"))
      files = args["files"].as<vector<string> >();


    //----------- task "matrix" ------------//
    if (analysis == "matrix") 
    {
      check_supplied_filenames(1,files);
      vector<SequenceTree> trees = load_trees(files[0],skip,subsample,max);

      ublas::matrix<double> D = distances(trees,metric_fn);

      if (not args.count("no-remove-duplicates"))
	D = remove_duplicates(D);

      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }
    }

    else if (analysis == "autocorrelation") 
    {
      check_supplied_filenames(1,files);
      vector<SequenceTree> trees = load_trees(files[0],skip,subsample,max);

      ublas::matrix<double> D = distances(trees,metric_fn);
      
      // set the window size
      int max_lag = int( double(trees.size()/20.0 + 1.0 ) );
      if (args.count("max-lag"))
	max_lag = min(max_lag,args["max-lag"].as<int>());
      
      // bound the max_lag
      if (max_lag >= trees.size()/2)
	max_lag = trees.size()/2;
      cerr<<"# max lag = "<<max_lag<<endl;

      // write out the average distances
      valarray<double> distances(0.0,max_lag);
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

    else if (analysis == "diameter") 
    {
      check_supplied_filenames(1,files);
      vector<SequenceTree> trees = load_trees(files[0],skip,subsample,max);
      if (trees.size() < 2)
	throw myexception()<<"diameter: only 1 point in set.";

      ublas::matrix<double> D = distances(trees,metric_fn);
      
      diameter(D);
    }

    else if (analysis == "compare") 
    {
      check_supplied_filenames(2,files);

      vector<SequenceTree> trees1 = load_trees(files[0],skip,subsample,max);
      vector<SequenceTree> trees2 = load_trees(files[1],skip,subsample,max);
      const unsigned N1 = trees1.size();
      const unsigned N2 = trees2.size();

      vector<SequenceTree> both = trees1;
      both.insert(both.end(),trees2.begin(),trees2.end());

      ublas::matrix<double> D1 = distances(trees1,metric_fn);
      ublas::matrix<double> D2 = distances(trees2,metric_fn);
      ublas::matrix<double> D  = distances(both,metric_fn);
      
      diameter(D1,"1");cout<<endl;
      diameter(D2,"2");cout<<endl;

      double diameter = 0;
      valarray<double> distances(0.0, N1*N2);
      for(int i=0;i<N1;i++)
	for(int j=0;j<N2;j++) {
	  diameter = std::max(diameter,D(i,N1+j));
	  distances[i*N1+j] += D(i,N1+j);
	}

      cout<<"max D12 = "<<diameter<<endl;
      report_distances(distances,"12");
    }

    else if (analysis == "convergence") 
    {
      check_supplied_filenames(2,files);

      vector<SequenceTree> trees1 = load_trees(files[0],skip,subsample,max);
      vector<SequenceTree> trees2 = load_trees(files[1],skip,subsample,max);

      for(int i=0;i<trees1.size();i++)
	cout<<distance(trees1[i],trees2,metric_fn)<<"\n";
    }
    else if (analysis == "converged") 
    {
      check_supplied_filenames(2,files);

      vector<SequenceTree> trees1 = load_trees(files[0],skip,subsample,max);
      vector<SequenceTree> trees2 = load_trees(files[1],skip,subsample,max);

      ublas::matrix<double> D2 = distances(trees2,metric_fn);
      valarray<double> distances(0.0, trees2.size());
      for(int i=0;i<D2.size1();i++)
	for(int j=0;j<i;j++) {
	  distances[i] += D2(i,j);
	  distances[j] += D2(i,j);
	}
      distances /= (trees2.size()-1);
      double target = median(distances);

      for(int i=0;i<trees1.size();i++)
	if (distance(trees1[i],trees2,metric_fn) <= target) {
	  cout<<"converged = "<<(i+1)<<endl;
	  cout<<"target distance = "<<target<<endl;
	  return 0;
	}
      cout<<"Did not converge! ("<<trees1.size()<<" samples)."<<endl;
      cout<<"target distance = "<<target<<endl;
    }
    else
      throw myexception()<<"Analysis '"<<analysis<<"' not recognized.";
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
