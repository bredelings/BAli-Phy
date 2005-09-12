// FIXME -  Try to put the variance and stuff on one line:
//   0.656 (56/100)   +- 0.007 -> 0.070

// Find some way to put the correlation into the model
// So tha the correlation doesn't keep on going up w/
// Distance, but goes up quickly

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


double conservative(const vector<double>& CI) {
  if (0.5 <= CI[0])
    return CI[0];
  else if (CI[0] <= 0.5 and 0.5 <= CI[1])
    return 0.5;
  else if (CI[1] <= 0.5)
    return CI[1];
  else
    std::abort();
}
  

bool separated_by(const vector<double>& CI1,const vector<double>& CI2,double dx) 
{
  assert(CI1.size() == 2);
  assert(CI2.size() == 2);

  if ((log10(CI1[1]) + dx < log10(CI2[0])) and (log10(CI1[1] + dx < 0 or dx < log10(CI2[0]))) )
    return true;

  if ((log10(CI2[1]) + dx < log10(CI1[0])) and (log10(CI2[1] + dx < 0 or dx < log10(CI1[0]))) )
    return true;

  return false;
}

bool report_sample(std::ostream& o,
		   const vector<valarray<bool> >& samples, 
		   const vector<valarray<double> >& distributions,
		   int pseudocount, double dx=-1) {
  o.precision(3);
  o.setf(ios::fixed);

  const int n_dists = samples.size();

  //---------- Basic statistics -------------//
  vector<int> N(n_dists);
  vector<int> n(n_dists);
  vector<double> P(n_dists);
  for(int i=0;i<n_dists;i++) {
    N[i] = samples[i].size();
    n[i] = statistics::count(samples[i]);
    P[i] = double(n[i])/N[i];
  }

  vector< valarray<double> > values;

  vector< vector<double> > CI(n_dists);
  vector< unsigned > nchanges ( n_dists);
  vector< unsigned > nchanges_ave (n_dists);

  vector<double> Var_perfect(n_dists);
  vector<double> Var_bootstrap(n_dists);
  vector<double> stddev_bootstrap(n_dists);
  vector<double> Ne(n_dists);

  for(int i=0;i<n_dists;i++) {

    //---------- Confidence Interval -------------//
    CI[i] = statistics::confidence_interval(distributions[i],0.95);

    //------- Numbers of Constant blocks ---------//
    nchanges[i] = changes(samples[i],true) + changes(samples[i],false);

    nchanges_ave[i] = (nchanges[i] + 1)/2;

    //----------------- Variances ---------------//
    Var_perfect[i] = P[i]*(1.0-P[i])/N[i];
    Var_bootstrap[i] = statistics::Var(distributions[i]);

    stddev_bootstrap[i] = sqrt( Var_bootstrap[i] );

    Ne[i] = P[i]*(1.0-P[i])/Var_bootstrap[i];
  }

  bool different = (dx <= 0) or n_dists==1;
  if (not different) {
    for(int i=0;i<n_dists;i++)
      for(int j=0;j<i;j++)
	if (separated_by(CI[i],CI[j],dx))
	  different = true;
  }
  if (not different)
    return false;


  for(int i=0;i<n_dists;i++) {
    //------------- Write things out -------------//
    o<<"   P"<<i<<" = "<<P[i]<<"  in  ("<<CI[i][0]<<","<<CI[i][1]<<")        (1="<<n[i]<<"  0="<<N[i]-n[i]<<")  ["<<nchanges_ave[i];
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
    
  for(int i=0;i<n_dists;i++) {
    o<<"   10s = "<<log10(statistics::odds(P[i]))<<"  in  ("<<log10(statistics::odds(CI[i][0]))<<","<<log10(statistics::odds(CI[i][1]))<<")";
    if (nchanges_ave[i] > 6)
      o<<"    [Var]x = "<<Var_bootstrap[i]/Var_perfect[i]<<"          Ne = "<<Ne[i];
    o<<endl;
  }

  return true;
}

unsigned count(const vector<int>& indices, const valarray<bool>& results) {
  unsigned total = 0;
  for(int i=0;i<indices.size();i++)
    if (results[indices[i]]) total++;
  return total;
}

double fraction(const vector<int>& indices, const valarray<bool>& results) {
  return double(count(indices,results))/indices.size();
}



variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("sub-sample",value<int>(),"factor by which to sub-sample")
    ("files",value<vector<string> >(),"tree files to examine")
    ("predicates",value<vector<string> >()->composing(),"tree files to examine")
    ;
  
  options_description bootstrap("Block bootstrap options");
  bootstrap.add_options()
    ("pseudocount",value<int>()->default_value(0),"extra 0/1 to add to bootstrap samples")
    ("blocksize",value<int>(),"block size to use in block boostrap")
    ("seed", value<unsigned long>(),"random seed")
    ;
    
  options_description reporting("Reporting options");
  reporting.add_options()
    ("separation",value<double>()->default_value(0),"Only report trees/partitions if they differ by this many LODs")
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
    cout<<"Usage: tree-dist-compare <file1> <file2> ... <[OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {

    std::cout.precision(3);
    std::cout.setf(ios::fixed);

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
    cout<<"random seed = "<<seed<<endl<<endl;
    
    int pseudocount = args["pseudocount"].as<int>();
    std::cout<<"pseudocount = "<<pseudocount<<endl<<endl;

    double compare_dx = args["separation"].as<double>();
    
    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    int subsample=1;
    if (args.count("sub-sample"))
      subsample = args["sub-sample"].as<int>();

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
      
      tree_dists.push_back(tree_sample(file,skip,max,subsample));

      if (i > 0 and tree_dists.back().names() != tree_dists[0].names())
	throw myexception()<<"Tree loaded from file '"<<files[i]<<"' has different taxa than previous trees.";
    }
    
    //----------  Determine block size ----------//
    unsigned blocksize = tree_dists[0].size()/100+1;
    for(int i=1;i<tree_dists.size();i++)
      blocksize = std::min(blocksize,tree_dists[i].size()/100+1);

    if (args.count("blocksize"))
      blocksize = args["blocksize"].as<int>();
    
    std::cout<<"blocksize = "<<blocksize<<endl<<endl;

    //----------- Load Partitions ---------------//
    vector<string> predicate_files = args["predicates"].as< vector<string> >();
    
    vector<vector<Partition> > partitions;
    for(int i=0;i<predicate_files.size();i++) 
      load_partitions(predicate_files[i],partitions);

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

    const int n_samples = 10000;
    vector<int> resample(n_samples);
      
    for(int d=0;d<tree_dists.size();d++) {
 
      for(int p=0; p<partitions.size(); p++)
	distributions[p][d].resize(n_samples);
	
      for(int s=0; s<n_samples; s++) {
	resample = bootstrap_sample_indices(tree_dists[d].size(), blocksize);
	
	for(int p=0; p<partitions.size(); p++)
	  distributions[p][d][s] = fraction(resample,results[p][d]);
      }
    }


    //------- Print out support for each partition --------//
    cout<<"Support for the different predicates: \n\n";
    for(int p=0;p<partitions.size();p++) {
      std::ostringstream report;

      bool show = report_sample(report, results[p], distributions[p], pseudocount, compare_dx);

      //-------- Determine and print the partition -----------//
      if (not show) continue;

      for(int i=0;i<partitions[p].size();i++)
	cout<<partitions[p][i]<<endl;

      cout<<report.str();

      cout<<endl<<endl;
    }

  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
