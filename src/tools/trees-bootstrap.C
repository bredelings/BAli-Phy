/*
   Copyright (C) 2004-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

// FIXME -  Try to put the variance and stuff on one line:
//   0.656 (56/100)   +- 0.007 -> 0.070

// FIXME - lining up columns by adding spaces produces inability to use pickout -> write out table directly?
// FIXME - Construct RCI for branch lengths, condition on branch existing?
// FIXME - 2*9000 trees takes 354 Mb for 68 tips, ~200 partitions: too much RAM to do 100,000 trees!
// FIXME - could we get some compression by storing identical partitions once?
// FIXME - var_stats::calculate( ) takes too long for all-1's partitions?
// FIXME - speed, in general.
// FIXME - Block-Bootstrap-based RCI: require individual runs to have at least 20 regenerations.


// NOTE - the number of regenerations that we see definitely depends on how much we subsample.

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

class tree_sample_collection
{
  vector<tree_sample> tree_dists;
  vector<int> n_samples_;
  vector< vector<int> > index_;

  vector<string> leaf_names_;
public:

  const vector<string>& leaf_names() const { return leaf_names_;}

  int index(int d,int i) const {return index_[d][i];}

  int n_dists() const {return index_.size();}

  int n_samples(int d) const {return n_samples_[d];}

  int n_samples() const {return tree_dists.size();}

  const tree_sample& sample(int d,int i) const {return tree_dists[index(d,i)];}
        tree_sample& sample(int d,int i)       {return tree_dists[index(d,i)];}

  const tree_sample& sample(int i) const {return tree_dists[i];}
        tree_sample& sample(int i)       {return tree_dists[i];}

  const vector<tree_sample>& all_samples() const {return tree_dists;}

  int add_sample(int d, const tree_sample& s) 
  {
    int i = tree_dists.size();
    index_[d].push_back(i);
    tree_dists.push_back(s);

    n_samples_[d]++;

    if (not leaf_names_.size())
      leaf_names_ = tree_dists.back().names();
    else if (tree_dists.back().names() != leaf_names_)
      throw myexception()<<"Trees have different taxa than previous files.";

    return i;
  }

  int add_sample_new_distribution(const tree_sample& s)
  {
    int d = n_samples_.size();
    index_.push_back(vector<int>());
    n_samples_.push_back(0);
    add_sample(d,s);
    return d;
  }

  tree_sample_collection() {}

  tree_sample_collection(const vector<vector<string> >& filenames,int skip, int subsample, int max)
  {
    for(int i=0;i<filenames.size();i++) 
    {
      if (filenames[i].size() < 1)
	throw myexception()<<"Group "<<i+1<<" doesn't contain any files!";

      cout<<"# Loading trees from '"<<filenames[i][0]<<"'...\n";
      int d = add_sample_new_distribution(tree_sample(filenames[i][0],skip,subsample,max));
      for(int j=1;j<filenames[i].size();j++) {
	cout<<"# Loading trees from '"<<filenames[i][j]<<"'...\n";
	add_sample(d,tree_sample(filenames[i][j],skip,subsample,max));
      }
    }
  }
};


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

// Things we calculate or cache per partition
struct var_stats
{
  int N;
  int n;
  double P;
  double O;
  valarray<bool> results;
  valarray<double> distributions;
  valarray<double> LOD_distributions;

  pair<double,double> CI;
  pair<double,double> log_CI;

  unsigned nchanges;
  unsigned nchanges_ave;

  double Var_perfect;
  double Var_bootstrap;
  double stddev_bootstrap;
  double Ne;
  double tau;

  void calculate(double, double);
};


void var_stats::calculate(double pseudocount, double confidence) 
{
  using namespace statistics;

  N = results.size();
  n = statistics::count(results);
  P = fraction(n, N, pseudocount);
  O = odds(n, N, pseudocount);

  //---------- Confidence Interval -------------//
  if (distributions.size()) {
    CI = statistics::confidence_interval(distributions, confidence);
    log_CI.first = log10(odds(CI.first));
    log_CI.second = log10(odds(CI.second));

    Var_bootstrap = statistics::Var(distributions);

    stddev_bootstrap = sqrt( Var_bootstrap );
  }
  
  //--------- LOD distributions ---------------//
  if (distributions.size()) {
    LOD_distributions.resize(distributions.size());
    for(int j=0;j<LOD_distributions.size();j++)
      LOD_distributions[j] = log10(odds(distributions[j]));
  }

  //------- Numbers of Constant blocks ---------//
  nchanges = changes(results,true) + changes(results,false);

  nchanges_ave = (nchanges + 1)/2;

  //----------------- Variances ---------------//
  Var_perfect = P*(1.0-P)/N;

  // Ne = P*(1.0-P)/Var_bootstrap;
  
  //---------- Autocorrelation times ----------//
  vector<double> scratch(N);
  for(int j=0;j<scratch.size();j++)
    if (results[j])
      scratch[j]=1.0;
    else
      scratch[j]=0.0;
  
  tau = autocorrelation_time(scratch);

  Ne = N/tau;
}

void write_LODs(const string& filename, vector< vector< var_stats> >& VS)
{
  std::ofstream file(filename.c_str());

  int n_partitions = VS[0].size();
  for(int p=0; p<n_partitions; p++)
  {
    vector<double> LOD;
    for(int d=0;d<VS.size();d++)
      LOD.push_back( log10(VS[d][p].O) );
    file<<join(LOD,'\t')<<endl;
  }
  file.close();
}

bool report_sample(std::ostream& o,
		   bool bootstrapped,
		   const tree_sample_collection& tree_dists,
		   vector< vector< vector< var_stats > > >& VS,
		   int p,
		   int pseudocount, double confidence, double dx=-1) 
{

  o.precision(3);
  o.setf(ios::fixed);

  bool any_mixing=false;

  vector<int> D(tree_dists.n_dists());
  for(int d=0;d<D.size();d++) {
    D[d] = tree_dists.n_samples(d);
    if (D[d] > 1) {
      D[d]++;
      any_mixing=true;
    }
  }

  //---------- Basic statistics -------------//
  for(int g=0; g<tree_dists.n_dists(); g++) 
    for(int d=0; d<D[g] ;d++) {
      int pseudo = pseudocount;
      if (any_mixing and d == D[g]-1)
	pseudo *= (D[g]-1);
      VS[g][d][p].calculate(pseudo, confidence);
    }

  bool different = (dx <= 0) or tree_dists.n_dists()==1;
  if (bootstrapped and not different) {

    for(int i=0;i<tree_dists.n_dists();i++)
      for(int j=0;j<i;j++) {
	if (separated_by(VS[i][D[i]][p].LOD_distributions,VS[j][D[j]][p].LOD_distributions,dx) >= confidence)
	  different = true;
      }
  }
  if (not different)
    return false;

  for(int g=0;g<tree_dists.n_dists();g++) 
  {
    if (g > 0) o<<endl;
  
    // int n_dists = tree_dists.n_samples(g);
    for(int d=0;d<D[g];d++)
    {
      const var_stats& vs = VS[g][d][p];
      o<<"   PP"<<g+1;
      if (d<D[g]-1)
	o<<"-"<<d+1;
      else if (any_mixing)
	o<<"  ";
      o<<" = "<<vs.P;

      // report confidence interval for the PP if we bootstrapped
      if (bootstrapped) o<<"  in  ("<<vs.CI.first<<","<<vs.CI.second<<")";

      o<<"       (1="<<vs.n<<"  0="<<vs.N-vs.n<<")  ["<<vs.nchanges_ave;

      if (vs.nchanges <= 4) {
	o<<" !!!";
      }
      else if (vs.nchanges <= 20) {
	o<<" !!";
      }
      else if (vs.nchanges <= 50) {
	o<<" !";
      }
      o<<"]";
      if (vs.nchanges_ave > 6 and bootstrapped)
	o<<"     sigma = "<<vs.stddev_bootstrap;
      o<<endl;
    }
  }
  o<<endl;


  vector<double> sum_tau(tree_dists.n_dists(), 0);
  vector<double> sum_CI(tree_dists.n_dists(), 0);

  for(int g=0;g<tree_dists.n_dists();g++) 
  {
    if (g > 0) o<<endl;
    
    int n_dists = tree_dists.n_samples(g);
    for(int d=0;d<D[g];d++)
    {
      const var_stats& vs = VS[g][d][p];
      o<<"  LOD"<<g+1;
      if (d < D[g]-1)
	o<<"-"<<d+1;
      else if (any_mixing)
	o<<"  ";
      o<<" = "<<log10(vs.O);

      if (bootstrapped) 
	o<<"  in  ("<<vs.log_CI.first<<","<<vs.log_CI.second<<")";

      if (vs.nchanges_ave > 6) {
      //      o<<"    [Var]x = "<<vs.Var_bootstrap/vs.Var_perfect<<"          Ne = "<<vs.Ne<<endl;
	o<<"       ACT = "<<vs.tau<<"          Ne = "<<vs.N/vs.tau;
      }
      o<<endl;
      if (d < D[g]-1 and bootstrapped) {
	sum_CI[g] += std::abs(vs.CI.second - vs.CI.first);
	sum_tau[g] += vs.tau;
      }
    }
  }

  if (any_mixing) o<<endl;

  for(int g=0;g<tree_dists.n_dists();g++) 
  {
    int n_dists = tree_dists.n_samples(g);
    if (n_dists > 1) {
      const var_stats& vs = VS[g][n_dists][p];
      o<<"  RNe"<<g+1<<" = "<<vs.tau/(sum_tau[g]/n_dists)<<endl;
      if (bootstrapped) o<<"  RCI"<<g+1<<" = "<<std::abs(vs.CI.second - vs.CI.first)/(sum_CI[g]/n_dists)<<endl;
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
  options_description invisible("Invisible options");
  invisible.add_options()
    ("files",value<vector<string> >(),"Tree files to examine.")
    ;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help,h", "Produce help message.")
    ("skip,s",value<string>()->default_value("10%"),"Number of trees to skip.")
    ("max,m",value<unsigned>(),"Maximum number of trees to read.")
    ("sub-sample,x",value<unsigned>(),"Factor by which to sub-sample.")
    ("predicates",value<string>(),"Predicates to examine.")
    ("min-support",value<double>()->default_value(0.1,"0.1"),"Minimum value of predicates to consider interesting..")
    ("verbose,v","Output more log messages on stderr.")
    ;
  
  options_description bootstrap("Block bootstrap options");
  bootstrap.add_options()
    ("bootstrap","Do block bootstrapping to get a CI on the posterior probabilities.")
    ("samples",value<unsigned>()->default_value(10000U),"Number of bootstrap samples.")
    ("pseudocount",value<unsigned>()->default_value(1U),"Extra 0/1 to add to bootstrap samples.")
    ("blocksize",value<unsigned>(),"Block size to use in block boostrap.")
    ("seed", value<unsigned long>(),"Random seed.")
    ;
    
  options_description reporting("Reporting options");
  reporting.add_options()
    ("separation",value<double>()->default_value(0),"Only report trees/partitions if they differ by this many LODs")
    ("confidence",value<double>()->default_value(0.95,"0.95"),"Width of confidence intervals")
    ("LOD-table",value<string>(),"Write the partitions LOD10's to a file.")
    ;
    
  options_description all("All options");
  all.add(invisible).add(input).add(bootstrap).add(reporting);

  options_description visible("All options");
  visible.add(input).add(bootstrap).add(reporting);

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
    cout<<visible<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
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
pair<double,double> 
am_sdsf(const tree_sample_collection& tree_dists, int d, const map<dynamic_bitset<>, p_counts>& counts, double min_f)
{
  double msdsf = 0;
  // tree_dists is only used to get the number of distributions, and the number of samples for each one.
  if (tree_dists.n_samples(d) < 2)
    throw myexception()<<"You can't calculate the ASDSF for less than two runs!";

  // Get minimum counts
  vector<int> min(tree_dists.n_samples(d));
  for(int i=0;i<tree_dists.n_samples(d);i++)
    min[i] = (int)(min_f*tree_dists.sample(d,i).size());

  // How many partitions did we count?
  int n_partitions = 0;

  // What is the running sum of the Standard Deviation of Split Frequencies?
  double asdsf = 0;

  int N = tree_dists.n_samples(d);
  
  // For each sampled partition
  foreach(record,counts) 
  {
    // Skip records that have a frequency that is too low
    bool skip=true;
    const vector<int>& x = record->second.counts;
    for(int i=0;i<N;i++) {
      if (x[tree_dists.index(d,i)] > min[i]) {
	skip = false;
	break;
      }
    }
    
    // Compute the empirical moments
    double sum=0;
    double sumsq=0;
    for(int i=0;i<N;i++)
    {
      double q = double(x[tree_dists.index(d,i)])/tree_dists.sample(d,i).size();
      sum += q;
      sumsq += q*q;
    }

    // Compute an estimate of the average: the variance
    double f = (sumsq - sum*sum/N) / (N-1);

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

    int skip = 0;
    double skip_fraction=0;
    {
      string s = args["skip"].as<string>();
      if (not can_be_converted_to<int>(s,skip))
      {
	skip = 0;
	if (not s.size() or s[s.size()-1] != '%')
	  throw myexception()<<"Argument to --skip="<<s<<" is neither an integer nor a percent";
	else
	  skip_fraction = convertTo<double>(s.substr(0,s.size()-1))/100;
      }
    }


    int max = -1;
    if (args.count("max"))
      max = args["max"].as<unsigned>();
    
    int subsample=1;
    if (args.count("sub-sample"))
      subsample = args["sub-sample"].as<unsigned>();

    //-------------- Read in tree distributions --------------//
    if (not args.count("files"))
      throw myexception()<<"Tree files not specified!";

    vector<string> files = args["files"].as< vector<string> >();
    vector< vector<string> > filenames(1);
    for(int i=0; i < files.size(); i++)
    {
      if (files[i] != "::")
	filenames.back().push_back(files[i]);
      else
	filenames.push_back(vector<string>());
    }

    tree_sample_collection tree_dists(filenames,skip,subsample,max);

    vector<int> D(tree_dists.n_dists());
    for(int d=0;d<D.size();d++) {
      D[d] = tree_dists.n_samples(d);
      if (D[d] > 1)
	D[d]++;
    }

    //----------  Skip some fraction of trees, if asked ----------//
    int min_trees = tree_dists.sample(0).size();

    for(int i=1;i<tree_dists.n_samples();i++) 
      min_trees = std::min<int>( min_trees, tree_dists.sample(i).size());

    int min_skip = 0;
    if (skip == 0)
      min_skip = (int)(skip_fraction * min_trees);

    if (log_verbose and min_skip > 0)
      cerr<<"Skipping "<<skip_fraction*100<<"% of "<<min_trees<<" = "<<min_skip<<endl;

    for(int i=0;i<tree_dists.n_samples();i++) 
    {
      tree_sample& trees = tree_dists.sample(i);
      if (skip == 0 and skip_fraction > 0) {
	int my_skip = std::min<int>(min_skip, trees.trees.size());
	trees.trees.erase(trees.trees.begin(), trees.trees.begin() + my_skip);
      }
    }

    min_trees = tree_dists.sample(0).size();
    for(int i=1;i<tree_dists.n_samples();i++) 
      min_trees = std::min<int>( min_trees, tree_dists.sample(i).size());

    //----------  Determine block size ----------//
    unsigned blocksize = min_trees/50+1;

    if (args.count("blocksize"))
      blocksize = args["blocksize"].as<unsigned>();
    bool bootstrapped = (args.count("bootstrap") > 0);

    cout<<"# [ seed = "<<seed<<"    pseudocount = "<<pseudocount<<"    blocksize = "<<blocksize<<" ]"<<endl<<endl;

    //-------- Scan the full partitions ----------//
    map< dynamic_bitset<>, p_counts> counts = get_multi_partitions_and_counts(tree_dists.all_samples());


    //----------- Load Partitions ---------------//
    vector<vector<Partition> > partitions;
    if (args.count("predicates")) {
      load_partitions(args["predicates"].as<string>(), partitions);
      partitions = remove_duplicates(partitions);
    }
    else {
      // Get minimum counts
      vector<int> min(tree_dists.n_samples());
      for(int i=0;i<tree_dists.n_samples();i++)
	min[i] = (int)(min_support*tree_dists.sample(i).size());

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
	partitions.push_back(vector<Partition>(1,Partition(tree_dists.leaf_names(),splits[order[i]])));
      if (log_verbose) cerr<<n_splits<<" total splits        "<<order.size()<<" splits with PP > "<<min_support<<endl;
    }



    // FIXME - there is a faster way to bootstrap resample
    // -- Compute the cumulative sums, and then when we choose a block we know
    //     the sum over the block already.  (sum[i+W]-sum[i]).

    //------- evaluate/cache predicate for each topology -------//
    vector< vector< vector< var_stats > > > VS (tree_dists.n_dists() );

    for(int g=0;g<tree_dists.n_dists();g++) 
    {
      VS[g].resize(D[g]);

      int total_size=0;

      // Analyze the group members
      for(int d=0;d<tree_dists.n_samples(g);d++)
      {
	VS[g][d].resize(partitions.size());

	int size = tree_dists.sample(g,d).size();
	total_size += size;

	for(int p=0; p<partitions.size(); p++) 
	{
	  VS[g][d][p].results.resize( size );
	  VS[g][d][p].results = tree_dists.sample(g,d).support(partitions[p]);
	}
      }

      // Analyze the group total
      if (D[g] > 1) 
      {
	int T = tree_dists.n_samples(g);
	VS[g][T].resize(partitions.size());
	// concatenate the individual runs to yield the total
	for(int p=0; p<partitions.size(); p++) 
	{
	  VS[g][T][p].results.resize(total_size);

	  for(int d=0,i=0;d<tree_dists.n_samples(g);d++) 
	    for(int j=0;j<VS[g][d][p].results.size();j++)
	      VS[g][T][p].results[i++] = VS[g][d][p].results[j];
	}
      }
    }

    //----------  Compute bootstrap samples of fraction ----------//

    // generate a bootstrap sample for each g,d,s : but all the p share the same bootstrap sample.

    const unsigned n_samples = args["samples"].as<unsigned>();
      
    if (bootstrapped)
    for(int g=0;g<tree_dists.n_dists();g++)
      for(int d=0;d<D[g];d++) 
      {
	for(int p=0; p<partitions.size(); p++)
	  VS[g][d][p].distributions.resize(n_samples);

	for(int s=0; s<n_samples; s++) {
	  vector<int> resample = bootstrap_sample_indices(VS[g][d][0].results.size() + 2*pseudocount, blocksize);

	  for(int p=0; p<partitions.size(); p++) {
	    VS[g][d][p].distributions[s] = fraction(resample, VS[g][d][p].results, pseudocount);
	  }
	}
      }

    //------- Print out support for each partition --------//
    cout<<"Support for the different predicates: \n\n";
    for(int p=0;p<partitions.size();p++) 
    {
      std::ostringstream report;

      bool show = report_sample(report, bootstrapped, tree_dists, VS, p, pseudocount, confidence, compare_dx);

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

      cout<<p+1<<"/"<<partitions.size()<<":"<<endl;
      for(int i=0;i<partitions[p].size();i++)
	cout<<partitions[p][i]<<endl;

      cout<<report.str();

      cout<<endl<<endl;
    }

    if (args.count("LOD-table"))
      write_LODs(args["LOD-table"].as<string>(), VS[0]);

    for(int d=0;d<tree_dists.n_dists();d++) {
      if (tree_dists.n_samples(d) >= 2) {
	pair<double,double> p = am_sdsf(tree_dists, d, counts, min_support);
	cout<<"ASDSF[min="<<min_support<<"] = "<<p.first<<"     MSDSF = "<<p.second<<endl;
      }
      index_value<double> worst_Ne;
      int T = VS[d].size()-1;
      for(int p=0;p<partitions.size();p++) {
	if (VS[d][T][p].nchanges_ave > 10)
	  worst_Ne.check_min(p,VS[d][T][p].Ne);
      }
      cout<<"min Ne = "<<worst_Ne.value<<"    (partition = "<<worst_Ne.index+1<<")"<<endl;
    }
  }
  catch (std::exception& e) {
    cerr<<"trees-bootstrap: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
