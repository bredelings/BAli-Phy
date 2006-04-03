// FIXME -  Try to put the variance and stuff on one line:
//   0.656 (56/100)   +- 0.007 -> 0.070

#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <cmath>
#include <fstream>
#include <sstream>
#include <map>
#include <list>

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

double greater_than(const valarray<double>& d1,const valarray<double>d2,double dx)
{
  assert(dx >= 0.0);

  // Make distribution D1
  vector<double> D1(d1.size());
  for(int i=0;i<D1.size();i++)
    D1[i] = d1[i];

  std::sort(D1.begin(),D1.end());

  // Make distribution D2
  vector<double> D2(d2.size());
  for(int i=0;i<D2.size();i++)
    D2[i] = d2[i];

  std::sort(D2.begin(),D2.end());

   // Compute distribution F2
  vector<double> F2(D1.size());

  for(int i=0,j=0;i<D1.size();i++) {
    while((D2[j] + dx < D1[i]) and j < D2.size()) j++;

    F2[i] = double(j)/D2.size();
  }
  
  // Compute confidence
  double confidence = 0;
  for(int i=0;i<F2.size();i++)
    confidence += F2[i]/F2.size();

  return confidence;
}

double separated_by(const valarray<double>& d1,const valarray<double>d2,double dx)
{
  return std::max(greater_than(d1,d2,dx),greater_than(d2,d1,dx));
}



bool report_sample(std::ostream& o,
		   unsigned blocksize,
		   const vector<valarray<bool> >& samples, 
		   const vector<valarray<double> >& distributions,
		   int pseudocount, double confidence, double dx=-1) {
  o.precision(3);
  o.setf(ios::fixed);

  const int n_dists = samples.size();

  //---------- Basic statistics -------------//
  vector<int> N(n_dists);
  vector<int> n(n_dists);
  vector<double> P(n_dists);
  vector<double> O(n_dists);
  for(int i=0;i<n_dists;i++) {
    N[i] = samples[i].size();
    n[i] = statistics::count(samples[i]);
    P[i] = fraction(n[i],N[i],pseudocount);
    O[i] = odds(n[i],N[i],pseudocount);
  }

  vector< valarray<double> > values;

  vector< pair<double,double> > CI(n_dists);
  vector< pair<double,double> > log_CI(n_dists);

  vector< unsigned > nchanges ( n_dists);
  vector< unsigned > nchanges_ave (n_dists);

  vector<double> Var_perfect(n_dists);
  vector<double> Var_bootstrap(n_dists);
  vector<double> stddev_bootstrap(n_dists);
  vector<double> Ne(n_dists);

  vector<double> tau(n_dists);

  vector<valarray<double> > LOD_distributions = distributions;

  for(int i=0;i<n_dists;i++) {

    //---------- Confidence Interval -------------//
    CI[i] = statistics::confidence_interval(distributions[i],confidence);
    log_CI[i].first = log10(odds(CI[i].first));
    log_CI[i].second = log10(odds(CI[i].second));

    //--------- LOD distributions ---------------//
    for(int j=0;j<LOD_distributions[i].size();j++)
      LOD_distributions[i][j] = log10(odds(LOD_distributions[i][j]));

    //------- Numbers of Constant blocks ---------//
    nchanges[i] = changes(samples[i],true) + changes(samples[i],false);

    nchanges_ave[i] = (nchanges[i] + 1)/2;

    //----------------- Variances ---------------//
    Var_perfect[i] = P[i]*(1.0-P[i])/N[i];

    Var_bootstrap[i] = statistics::Var(distributions[i]);

    stddev_bootstrap[i] = sqrt( Var_bootstrap[i] );

    Ne[i] = P[i]*(1.0-P[i])/Var_bootstrap[i];

    //---------- Autocorrelation times ----------//
    valarray<double> scratch(N[i]);
    for(int j=0;j<scratch.size();j++)
      if (samples[i][j])
	scratch[j]=1.0;
      else
	scratch[j]=0.0;

    tau[i] = autocorrelation_time(scratch,blocksize);
  }

  bool different = (dx <= 0) or n_dists==1;
  if (not different) {
    for(int i=0;i<n_dists;i++)
      for(int j=0;j<i;j++) {
	if (separated_by(LOD_distributions[i],LOD_distributions[j],dx) >= confidence)
	  different = true;
      }
  }
  if (not different)
    return false;

  for(int i=0;i<n_dists;i++) {

    //------------- Write things out -------------//
    o<<"   P"<<i<<" = "<<P[i]<<"  in  ("<<CI[i].first<<","<<CI[i].second<<")        (1="<<n[i]<<"  0="<<N[i]-n[i]<<")  ["<<nchanges_ave[i];
    if (nchanges[i] <= 4) {
      o<<" !!!";
    }
    else if (nchanges[i] <= 20) {
      o<<" !!";
    }
    else if (nchanges[i] <= 50) {
      o<<" !";
    }
    o<<"]";
    if (nchanges_ave[i] > 6)
      o<<"     sigma = "<<stddev_bootstrap[i];
    o<<endl;
  }    
  o<<endl;
    
  for(int i=0;i<n_dists;i++) 
  {
    o<<"   10s = "<<log10(O[i])<<"  in  ("<<log_CI[i].first<<","<<log_CI[i].second<<")";
    if (nchanges_ave[i] > 6) {
      //      o<<"    [Var]x = "<<Var_bootstrap[i]/Var_perfect[i]<<"          Ne = "<<Ne[i]<<endl;
      o<<"    [Var]x = "<<tau[i]<<"          Ne = "<<N[i]/tau[i];
    }
    o<<endl;
  }

  return true;
}

unsigned count(const vector<int>& indices, const valarray<bool>& results,unsigned pseudocount) {
  unsigned total = 0;
  for(int i=0;i<indices.size();i++) 
  {
    int k=indices[i];
    if (k<pseudocount)
      continue; // false
    else
      k -= pseudocount;

    if (k >= results.size()) {
      total++;
      continue; //true
    }

    if (results[k]) total++;
  }
  return total;
}

double fraction(const vector<int>& indices, const valarray<bool>& results,unsigned pseudocount) {
  return double(count(indices,results,pseudocount))/indices.size();
}



variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("skip",value<unsigned>()->default_value(0),"number of trees to skip")
    ("max",value<unsigned>(),"maximum number of trees to read")
    ("sub-sample",value<unsigned>(),"factor by which to sub-sample")
    ("files",value<vector<string> >(),"tree files to examine")
    ("predicates",value<string>(),"predicates to examine")
    ;
  
  options_description bootstrap("Block bootstrap options");
  bootstrap.add_options()
    ("samples",value<unsigned>()->default_value(10000U),"number of bootstrap samples")
    ("pseudocount",value<unsigned>()->default_value(0U),"extra 0/1 to add to bootstrap samples")
    ("blocksize",value<unsigned>(),"block size to use in block boostrap")
    ("seed", value<unsigned long>(),"random seed")
    ;
    
  options_description reporting("Reporting options");
  reporting.add_options()
    ("separation",value<double>()->default_value(0),"Only report trees/partitions if they differ by this many LODs")
    ("confidence",value<double>()->default_value(0.95),"Width of confidence intervals")
    ;
    
  options_description all("All options");
  all.add(input).add(bootstrap).add(reporting);

  // positional options
  positional_options_description p;
  p.add("files", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: trees-bootstrap <file1> [<file2> ... ] --predicates <predicate file> [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("predicates"))
    throw myexception()<<"No predicates supplied.";

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {

    cout.precision(3);
    cout.setf(ios::fixed);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //--------------------- Initialize ---------------------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    
    int pseudocount = args["pseudocount"].as<unsigned>();

    double compare_dx = args["separation"].as<double>();
    
    int skip = args["skip"].as<unsigned>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<unsigned>();

    int subsample=1;
    if (args.count("sub-sample"))
      subsample = args["sub-sample"].as<unsigned>();

    double confidence = args["confidence"].as<double>();

    //-------------- Read in tree distributions --------------//
    if (not args.count("files"))
      throw myexception()<<"Tree files not specified!";

    vector<string> files = args["files"].as< vector<string> >();
    vector<tree_sample> tree_dists;
    for(int i=0;i<files.size();i++) 
    {
      ifstream file(files[i].c_str());
      if (not file)
	throw myexception()<<"Couldn't open file "<<files[i];
      
      cout<<"# Loading trees from '"<<files[i]<<"'...\n";
      tree_dists.push_back(tree_sample(file,skip,max,subsample));

      if (i > 0 and tree_dists.back().names() != tree_dists[0].names())
	throw myexception()<<"Tree loaded from file '"<<files[i]<<"' has different taxa than previous trees.";
    }
    
    //----------  Determine block size ----------//
    unsigned blocksize = tree_dists[0].size()/100+1;
    for(int i=1;i<tree_dists.size();i++)
      blocksize = std::min(blocksize,tree_dists[i].size()/100+1);

    if (args.count("blocksize"))
      blocksize = args["blocksize"].as<unsigned>();

    cout<<"# [ seed = "<<seed<<"    pseudocount = "<<pseudocount<<"    blocksize = "<<blocksize<<" ]"<<endl<<endl;

    //----------- Load Partitions ---------------//
    vector<vector<Partition> > partitions;
    load_partitions(args["predicates"].as<string>(), partitions);

    //------- evaluate/cache predicate for each topology -------//
    vector< vector<valarray<bool> > > results(partitions.size());

    for(int p=0;p<partitions.size();p++) {
      results[p].resize(tree_dists.size());

      for(int d=0;d<tree_dists.size();d++) {

	unsigned size = tree_dists[d].size();

	results[p][d].resize(size);

	for(int i=0;i<size;i++) {
	  int t = tree_dists[d].which_topology[i];
	  results[p][d][i] = implies(tree_dists[d].topologies[t].partitions, partitions[p]);
	}
      }
    }

    //----------  Compute bootstrap samples of fraction ----------//
    vector< vector< valarray<double> > > distributions(partitions.size(),
						       vector<valarray<double> >(tree_dists.size()));

    const unsigned n_samples = args["samples"].as<unsigned>();
    vector<int> resample;
      
    for(int d=0;d<tree_dists.size();d++) {
 
      for(int p=0; p<partitions.size(); p++)
	distributions[p][d].resize(n_samples);
	
      for(int s=0; s<n_samples; s++) {
	resample = bootstrap_sample_indices(tree_dists[d].size() + 2*pseudocount, blocksize);
	
	for(int p=0; p<partitions.size(); p++)
	  distributions[p][d][s] = fraction(resample,results[p][d],pseudocount);
      }
    }


    //------- Print out support for each partition --------//
    cout<<"Support for the different predicates: \n\n";
    for(int p=0;p<partitions.size();p++) {
      std::ostringstream report;

      bool show = report_sample(report, blocksize, results[p], distributions[p], pseudocount, confidence, compare_dx);

      //-------- Determine and print the partition -----------//
      if (not show) continue;

      for(int i=0;i<partitions[p].size();i++)
	cout<<partitions[p][i]<<endl;

      cout<<report.str();

      cout<<endl<<endl;
    }

  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
