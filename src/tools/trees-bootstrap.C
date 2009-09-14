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
#include "consensus-tree.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

using std::cout;
using std::cerr;
using std::endl;
using std::map;
using std::pair;

using boost::dynamic_bitset;

using namespace statistics;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

bool operator==(const vector<Partition>& p1,const vector<Partition>& p2)
{
  if (p1.size() != p2.size())
    return false;

  for(int i=0;i<p1.size();i++)
    if (not includes(p2,p1[i]))
      return false;

  return true;
}


vector<vector<Partition> > remove_duplicates(vector<vector<Partition> >& partitions)
{
  vector<vector<Partition> > partitions2;
  for(int i=0;i<partitions.size();i++)
    if (not includes(partitions2,partitions[i]))
      partitions2.push_back(partitions[i]);

  return partitions2;
}


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


// In order to make the things computed here more available, I should make a "statistics" object
// To cache these computed things on.  Then it would be easier (for example) to report the worst
// Report:
// - ratio of 80% confidence interval for "total" to 80% confidence interval for "individual"
//   .... but how do you merge them?
// - 




bool report_sample(std::ostream& o,
		   unsigned blocksize,
		   const vector<valarray<bool> >& samples, 
		   const vector<valarray<double> >& distributions,
		   int pseudocount, double confidence, double dx=-1) {
  o.precision(3);
  o.setf(ios::fixed);

  const int n_dists = samples.size();

  //FIXME - we need to create a merged sample so that we can calculate statistics from that.

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

  for(int i=0;i<n_dists;i++) 
  {
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

    tau[i] = autocorrelation_time(scratch);
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
    o<<"   PP";
    if (i<n_dists-1)
      o<<i;
    else if (n_dists >= 2)
      o<<" ";
    o<<" = "<<P[i]<<"  in  ("<<CI[i].first<<","<<CI[i].second<<")        (1="<<n[i]<<"  0="<<N[i]-n[i]<<")  ["<<nchanges_ave[i];

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
    
  double sum_tau=0;
  double sum_CI=0;
  for(int i=0;i<n_dists;i++) 
  {
    o<<"  LOD";
    if (i<n_dists-1)
      o<<i;
    else if (n_dists >= 2)
      o<<" ";
    o<<" = "<<log10(O[i])<<"  in  ("<<log_CI[i].first<<","<<log_CI[i].second<<")";
    if (nchanges_ave[i] > 6) {
      //      o<<"    [Var]x = "<<Var_bootstrap[i]/Var_perfect[i]<<"          Ne = "<<Ne[i]<<endl;
      o<<"       ACT = "<<tau[i]<<"          Ne = "<<N[i]/tau[i];
    }
    o<<endl;
    if (i < n_dists-1) {
      sum_CI += std::abs(CI[i].second - CI[i].first);
      sum_tau += tau[i];
    }
    else if (n_dists > 1) {
      o<<"  RNe = "<<tau[i]/(sum_tau/(n_dists-1))<<endl;
      o<<"  RCI = "<<std::abs(CI[i].second - CI[i].first)/(sum_CI/(n_dists-1))<<endl;
    }
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
    ("min-support",value<double>()->default_value(0.1),"Minimum value of predicates to consider interesting.")
    ("verbose,v","Output more log messages on stderr.")
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
    cout<<"Compare support for partitions between different files.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

tree_sample load_tree_file(const variables_map& args, const string& filename)
{
  int skip = args["skip"].as<unsigned>();

  int max = -1;
  if (args.count("max"))
    max = args["max"].as<unsigned>();

  int subsample=1;
  if (args.count("sub-sample"))
    subsample = args["sub-sample"].as<unsigned>();

  ifstream file(filename.c_str());
  if (not file)
    throw myexception()<<"Couldn't open file "<<filename;
  
  cout<<"# Loading trees from '"<<filename<<"'...\n";
  return tree_sample(file,skip,max,subsample);
}

/*
 * How can we compute the ASDSF values?  How can we compute over convergence values?
 * 1. Compute the splits with support <asdsf-min> or higher
 * 2. Consider all of these splits - be able to access the support of each chain for each split.
 
 * Do I really want to FIRST decompose the trees into splits?
 * Yeah, I guess that's fine.

 */

/* FIXME - how can we list (say) the Ne/ACT of the worst split */

// Measures: I could report the sum of Ne[i] over Ne of the concatenated data -- at least for equal run lengths.
// Ne is a measure of within-chain variance... this is better than other things, because:
// * it is the variance of the estimate of the mean, not the variance AROUND the mean
// * 
// .. but does this get at confidence intervals?
// .. and what is the scenario underlying confidence intervals, anyway?

/// Compute the average standard deviation of split frequencies
pair<double,double> am_sdsf(const vector<tree_sample>& tree_dists, const map<dynamic_bitset<>, p_counts>& counts, double min_f)
{
  double msdsf = 0;
  // tree_dists is only used to get the number of distributions, and the number of samples for each one.
  if (tree_dists.size() < 2)
    throw myexception()<<"You can't calculate the ASDSF for less than two runs!";

  // Get minimum counts
  vector<int> min(tree_dists.size());
  for(int i=0;i<tree_dists.size();i++)
    min[i] = (int)(min_f*tree_dists[i].size());

  // How many partitions did we count?
  int n_partitions = 0;

  // What is the running sum of the Standard Deviation of Split Frequencies?
  double asdsf = 0;

  // For each sampled partition
  foreach(record,counts) 
  {
    // Skip records that have a frequency that is too low
    bool skip=true;
    const vector<int>& x = record->second.counts;
    for(int i=0;i<x.size();i++)
      if (x[i] > min[i]) {
	skip = false;
	break;
      }
    
    // Compute the empirical moments
    double sum=0;
    double sumsq=0;
    for(int i=0;i<x.size();i++)
    {
      double q = double(x[i])/tree_dists[i].size();
      sum += q;
      sumsq += q*q;
    }

    // Compute an estimate of the average: the variance
    double f = (sumsq - sum*sum/x.size()) / (x.size()-1);

    // Take a sqrt to compute the standard deviation
    if (f < 0.0)
      f = 0;
    else
      f = sqrt(f);

    // Add this standard deviation to the sum
    if (not skip ) {
      asdsf += f;
      // Increment the number of partitions that were considered.
      n_partitions++;
    }

    msdsf = std::max(msdsf,f);
  }

  // Divide the sum by the total number to compute the average
  asdsf /= n_partitions;

  return pair<double,double>(asdsf,msdsf);
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
    
    double confidence = args["confidence"].as<double>();

    double min_support = args["min-support"].as<double>();
    //-------------- Read in tree distributions --------------//
    if (not args.count("files"))
      throw myexception()<<"Tree files not specified!";

    vector<string> files = args["files"].as< vector<string> >();
    vector<tree_sample> tree_dists;

    vector<string> leaf_names;
    for(int i=0;i<files.size();i++) 
    {
      tree_dists.push_back(load_tree_file(args,files[i]));

      if (i==0)
	leaf_names = tree_dists.back().names();
      else if (tree_dists.back().names() != leaf_names)
	throw myexception()<<"Trees loaded from file '"<<files[i]<<"' have different taxon than previous files.";
    }
    
    int D = tree_dists.size();
    if (D>1) D++;

    //----------  Determine block size ----------//
    unsigned blocksize = tree_dists[0].size()/50+1;
    for(int i=1;i<tree_dists.size();i++)
      blocksize = std::min(blocksize,tree_dists[i].size()/50+1);

    if (args.count("blocksize"))
      blocksize = args["blocksize"].as<unsigned>();

    cout<<"# [ seed = "<<seed<<"    pseudocount = "<<pseudocount<<"    blocksize = "<<blocksize<<" ]"<<endl<<endl;

    //-------- Scan the full partitions ----------//
    map< dynamic_bitset<>, p_counts> counts = get_multi_partitions_and_counts(tree_dists);


    //----------- Load Partitions ---------------//
    vector<vector<Partition> > partitions;
    if (args.count("predicates")) {
      load_partitions(args["predicates"].as<string>(), partitions);
      partitions = remove_duplicates(partitions);
    }
    else {
      // Get minimum counts
      vector<int> min(tree_dists.size());
      for(int i=0;i<tree_dists.size();i++)
	min[i] = (int)(min_support*tree_dists[i].size());

      vector<dynamic_bitset<> > splits;
      vector<unsigned> counts2;

      // For each sampled partition
      int n_splits=0;
      foreach(record,counts) 
      {
	n_splits++;
	// Skip records in which all frequencies are too low
	bool skip=true;
	int count = 0;
	const vector<int>& x = record->second.counts;
	for(int i=0;i<x.size();i++) {
	  count += x[i];
	  if (x[i] > min[i])
	    skip = false;
	}
    
	// Add this standard deviation to the sum
	if (not skip ) {
	  splits.push_back(record->first);
	  counts2.push_back(count);
	}
      }

      vector<int> order = iota<int>(counts2.size());
      std::sort(order.begin(),order.end(),sequence_order<unsigned>(counts2));
      std::reverse(order.begin(),order.end());
      for(int i=0;i<order.size();i++)
	partitions.push_back(vector<Partition>(1,Partition(leaf_names,splits[order[i]])));
      if (log_verbose) cerr<<n_splits<<" total splits        "<<order.size()<<" splits with PP > "<<min_support<<endl;
    }



    // FIXME - there is a faster way to bookstrap resample
    // -- Compute the cumulative sums, and then when we choose a block we know
    //     the sum over the block already.  (sum[i+W]-sum[i]).

    //------- evaluate/cache predicate for each topology -------//
    vector< vector<valarray<bool> > > results(partitions.size());

    for(int p=0;p<partitions.size();p++) 
    {
      results[p].resize(D);

      int total_size=0;
      for(int d=0;d<tree_dists.size();d++) {

	unsigned size = tree_dists[d].size();

	total_size += size;

	results[p][d].resize(size);

	results[p][d] = tree_dists[d].support(partitions[p]);
      }

      if (D > 1) {
	// concatenate the individual runs to yield the total
	results[p][tree_dists.size()].resize(total_size);
	for(int d=0,i=0;d<tree_dists.size();d++) 
	  for(int j=0;j<results[p][d].size();j++)
	    results[p][tree_dists.size()][i++] = results[p][d][j];
      }
    }

    //----------  Compute bootstrap samples of fraction ----------//
    vector< vector< valarray<double> > > distributions(partitions.size(),
						       vector<valarray<double> >(D));

    const unsigned n_samples = args["samples"].as<unsigned>();
    vector<int> resample;
      
    for(int d=0;d<D;d++) {
 
      for(int p=0; p<partitions.size(); p++)
	distributions[p][d].resize(n_samples);
	
      for(int s=0; s<n_samples; s++) {
	resample = bootstrap_sample_indices(results[0][d].size() + 2*pseudocount, blocksize);
	
	for(int p=0; p<partitions.size(); p++)
	  distributions[p][d][s] = fraction(resample,results[p][d],pseudocount);
      }
    }


    //------- Print out support for each partition --------//
    cout<<"Support for the different predicates: \n\n";
    for(int p=0;p<partitions.size();p++) 
    {
      std::ostringstream report;

      bool show = report_sample(report, blocksize, results[p], distributions[p], pseudocount, confidence, compare_dx);

      /*
      /// Check if our counts match the ones calculated the hard way.
      /// FIXME - how about pseudocounts?
      if (partitions[p].size() == 1 and partitions[p][0].full()) 
      {
	dynamic_bitset<> partition = partitions[p][0].group1;

	if (not partition[0])
	  partition.flip();

	map<dynamic_bitset<>, p_counts>::iterator record = counts.find(partition);

	if (record == counts.end())
	  for(int i=0;i<tree_dists.size();i++)
	    cout<<"PP"<<i<<"* = "<<fraction(0,tree_dists[i].size(),pseudocount)<<endl;
	else
	  for(int i=0;i<tree_dists.size();i++) {
	    const vector<int>& temp = record->second.counts;
	    cout<<"PP"<<i<<"* = "<<fraction(record->second.counts[i],tree_dists[i].size(),pseudocount)<<endl;
	  }
      }
      */

      //-------- Determine and print the partition -----------//
      if (not show) continue;

      for(int i=0;i<partitions[p].size();i++)
	cout<<partitions[p][i]<<endl;

      cout<<report.str();

      cout<<endl<<endl;
    }

    if (tree_dists.size() >= 2) {
      pair<double,double> p = am_sdsf(tree_dists, counts, min_support);
      cout<<"ASDSF[min="<<min_support<<"] = "<<p.first<<"     MSDSF = "<<p.second<<endl;
    }
  }
  catch (std::exception& e) {
    cerr<<"trees-bootstrap: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
