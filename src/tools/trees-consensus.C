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
#include "mctree.H"
#include "rng.H"

#include <boost/program_options.hpp>

/// FIXME - construct a full tree by extending the moveable
///  a) randomly
///  b) according to posterior probabilities (?greedy?)

// TODO: 
// 1. Construct a set of @n trees that contain splits down to a frequency of 0.1
// 2. Consider seeding the branches-to-unplug with splits of lower frequency than 0.5?
//  2a. But, perhaps we should try the 0.5 branches FIRST.
//  2b. Or, perhaps we should only try branches consistent with the high-prob branches.
//      In short, construct a greedy tree, and use that...
// 3. Consider trying to construct disjoint sets of trees with the most resolved branches total.
/// 3a. This is better than trying to consider the highest MAP trees, since those trees may all
///     be very similar to each other: imagine of one branch has 1/3 on each NNI possibility...
/// 3b. This approach would capture correlations and anti-correlations, as well...


namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using namespace std;

using std::cout;
using std::cerr;
using std::endl;

using statistics::odds;

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


// FIXME - if n is small, first level may be ~ 0.57 -- good or bad?
vector<double> get_consensus_levels(const string& s) {
  vector<double> levels;
  if (s.size())
    levels = split<double>(s,',');

  std::sort(levels.begin(),levels.end());

  // remove levels below 0.5
  while(not levels.empty() and levels[0] < 0.5) {
    std::cerr<<"Ignoring bad consensus level '"<<levels[0]<<"'"<<std::endl;
    levels.erase(levels.begin());
  }

  // remove levels above 1.0
  while(not levels.empty() and levels.back() > 1.0) {
    std::cerr<<"Ignoring bad consensus level '"<<levels.back()<<"'"<<std::endl;
    levels.pop_back();
  }
  
  // Make sure we consider 0.5
  if (levels.empty() or levels[0] != 0.5)
    levels.insert(levels.begin(),0.5);

  return levels;
}

/// Add partitions in 'delta' if none of them are IDENTICAL to any partition in 'partitions'
void add_unique_partitions(vector<Partition>& partitions,const vector<Partition>& delta) {
  for(int j=0;j<delta.size();j++)
    if (not includes(partitions,delta[j]))
      partitions.push_back(delta[j]);
}

double odds_ratio(const vector<pair<Partition,unsigned> >& partitions, const pair<Partition,unsigned>& delta,
		  unsigned N, unsigned pseudocount=0)
{
  unsigned n1 = 0;
  for(int i=0;i<partitions.size();i++)
    if (implies(partitions[i].first, delta.first))
      n1 = std::max(n1, partitions[i].second);

  unsigned n2 = delta.second;

  return statistics::odds_ratio(n1,n2,N,pseudocount);
}

double odds_ratio(vector<pair<Partition,unsigned> > partitions, int i,
		  unsigned N, unsigned pseudocount=0)
{
  pair<Partition,unsigned> delta = partitions[i];
  partitions.erase(partitions.begin()+i);
  return odds_ratio(partitions,delta,N,pseudocount);
}



bool covers(const pair<Partition,unsigned>& p1,const pair<Partition,unsigned>& p2, 
	    double ratio,unsigned N,unsigned pseudocount)
{
  if (not implies(p1.first, p2.first))
    return false;

  int n1 = p1.second;
  int n2 = p2.second;

  return (statistics::odds_ratio(n1,n2,N,pseudocount) < ratio);
}

bool covers(const vector<pair<Partition,unsigned> >& partitions, int exclude,
	    const pair<Partition,unsigned>& delta,double ratio, unsigned N, unsigned pseudocount=0)
{
  for(int i=0;i<partitions.size();i++)
    if (i != exclude and covers(partitions[i],delta,ratio,N,pseudocount))
      return true;
  return false;
}

bool merge(vector<pair<Partition,unsigned> >& partitions,
	   const pair<Partition,unsigned>& delta,
	   double ratio,
	   unsigned N,
	   unsigned pseudocount=0)
{
  for(int i=partitions.size()-1;i>=0;i--) 
    if (covers(delta,partitions[i],ratio,N,pseudocount))
      partitions.erase(partitions.begin()+i);
    else if (covers(partitions[i],delta,ratio,N,pseudocount))
      return false;

  partitions.push_back(delta);

  return true;
}

vector<pair<Partition,unsigned> > 
thin(const vector<pair<Partition,unsigned> >& partitions, unsigned N, double ratio)
{
  vector<pair<Partition,unsigned> > thinned;
  for(int i=0;i<partitions.size();i++)
    merge(thinned,partitions[i],ratio,N,5);

  return thinned;
}

vector<Partition> strip(const vector<pair<Partition,unsigned> >& partitions) 
{
  vector<Partition> stripped;
  for(int i=0;i<partitions.size();i++)
    stripped.push_back(partitions[i].first);

  return stripped;
}


/// Merge the lists in 'partition_sets' while removing duplicates
vector<Partition> merge(const vector<vector<Partition> >& partition_sets) 
{
  vector<Partition> partitions;
  for(int i=0;i<partition_sets.size();i++) 
    add_unique_partitions(partitions,partition_sets[i]);
  return partitions;
}

vector< vector<Partition> >
get_sub_partitions(const tree_sample& tree_dist,const vector<double>& levels,int depth=1) 
{
  vector< vector<Partition> > partition_sets(levels.size());
  for(int i=0;i<partition_sets.size();i++)
    partition_sets[i] = get_Ml_sub_partitions(tree_dist,levels[i],depth);

  return partition_sets;
}

vector< vector<Partition> >
get_full_partitions(const tree_sample& tree_dist,const vector<double>& levels) 
{
  vector< vector<Partition> > partition_sets(levels.size());
  for(int i=0;i<partition_sets.size();i++)
    partition_sets[i] = get_Ml_partitions(tree_dist,levels[i]);

  return partition_sets;
}

vector<Partition> 
get_Ml_partitions(const vector<pair<Partition,unsigned> >& sp, unsigned min_count)
{
  vector<Partition> partitions;
  for(int i=0;i<sp.size();i++) {
    if (sp[i].second >= min_count)
      merge_partition(partitions,sp[i].first);
  }

  return partitions;
}


vector<Partition> 
get_Ml_partitions(const vector<pair<Partition,unsigned> >& sp, double l, unsigned total)
{
  unsigned min_count = std::min(1+(unsigned)(total*l),total);

  return get_Ml_partitions(sp,min_count);
}

using std::set;
vector<unsigned> get_Ml_levels(const vector<pair<Partition,unsigned> >& sp,unsigned N,double min_support)
{
  set<unsigned> levels;

  levels.insert((unsigned)(N*min_support+1));
  levels.insert(N);

  for(int i=0;i<sp.size();i++) {
    levels.insert(sp[i].second);
    levels.insert(std::min(sp[i].second+1,N));
  }

  vector<unsigned> levels2(levels.size());
  copy(levels.begin(),levels.end(),levels2.begin());
  return levels2;
}

unsigned
get_partition_count(const vector<pair<Partition,unsigned> >& sp, const Partition& p)
{
  for(int i=0;i<sp.size();i++) {
    if (sp[i].first == p)
      return sp[i].second;
  }

  throw myexception()<<"Can't find partition "<<p;
}



// FIXME - we use all full parition in 'sub'
//   - only do exhaustive search on partial partitions?
//   - divide into connected components?

// This is a greedy search.
// It would be nice to also pick which branches are "least informative".

vector<Partition> Ml_min_Hull(const vector<Partition>& full,const vector<Partition>& sub)
{
  // compute full partitions to keep
  dynamic_bitset<> keep(full.size());
  dynamic_bitset<> covered(sub.size());

  while (covered.count() < covered.size()) 
  {
    // how many UNCOVERED subs does each UNKEPT full branch imply?
    vector<int> covers(full.size(),0);

    for(int i=0;i<sub.size();i++) {
      if (covered[i]) continue;
      for(int j=0;j<full.size();j++)
	if (implies(full[j],sub[i])) {
	  assert(not keep[j]);
	  covers[j]++;
	}
    }

    // only do the argmax if there are args to max over!
    if (not covers.size())
      break;

    // choose full branch to keep
    int best = argmax(covers);    // all KEPT branches cover 0 uncovered branches.

    if (covers[best] == 0) 
      break;

    keep[best] = true;

    // mark its covered sub-branches as covered
    for(int i=0;i<sub.size();i++)
      if (not covered[i] and implies(full[best],sub[i]))
	covered[i] = true;
  }

  // collect the full partitions that have been chosen
  vector<Partition> hull;
  for(int i=0;i<full.size();i++)
    if (keep[i])
      hull.push_back(full[i]);

  return hull;
}

// Is there a way to choose branches that would imply sub-partitions that are
// not in sub because other full branches imply them?  

// That might be best...

vector<Partition> Ml_max_Hull(const vector<Partition>& full,const vector<Partition>& sub)
{
  vector<Partition> hull;

  for(int i=0;i<full.size();i++) 
  {
    bool ok = false;
    for(int j=0;j<sub.size() and not ok;j++)
      if (implies(full[i],sub[j])) 
	ok = true;
   
    if (ok) hull.push_back(full[i]);
  }

  return hull;
}


//FIXME - put leaf partitions back into get_sub_partitions( )

void show_level(const tree_sample& tree_dist,
		unsigned level,
		const vector<Partition>& skeleton,
		const vector<pair<Partition,unsigned> >& all_partitions,
		bool show_sub,
		bool show_PP)
{
  vector<Partition> full_skeleton = select(skeleton,&Partition::full);

  const unsigned N = tree_dist.size();

  cout.unsetf(ios::fixed | ios::showpoint);
	
  const vector<Partition> sub = get_Ml_partitions(all_partitions,level);
  const vector<Partition> full = select(sub,&Partition::full);

  //  const vector<Partition> moveable = get_moveable_tree(sub);
  
  //  vector<Partition> full_hull = Ml_min_Hull(full_skeleton,sub);
  //  vector<Partition> sub_hull = Ml_min_Hull(skeleton,sub);
    
  double fraction = double(level)/N;

  double LOD = log10(odds(level,N,1));

  cout<<"   level = "<<fraction*100
      <<"   LOD = "<<LOD
      <<"   full = "<<count(full, informative);

  if (show_sub) {
    cout<<"   sub = "       <<count(sub,informative);
    //    cout<<"   consistent = "<<count(moveable,informative);
    //    cout<<"   sub(50) = "<<count(moveable,informative);
    //    cout<<"   sub#1 = "     <<count(full_hull,informative);
    //    cout<<"   sub#2 = "     <<count(sub_hull,informative);
  }

  if (show_PP)
    cout<<"   PP = "<<100*tree_dist.PP(full);

  cout<<endl;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("file",value<string>(),"tree sample to examine")
    ("seed", value<unsigned long>(),"Random seed")
    ;

  options_description input("Input options");
  input.add_options()
    ("help,h", "produce help message")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("sub-sample",value<int>()->default_value(1),"factor by which to sub-sample")
    ;
  
  options_description reporting("Reporting options");
  reporting.add_options()
    ("ignore", value<string>(),"comma-separated list of taxa to ignore in partitions")
    ("map-trees",value<int>()->default_value(1),"Only report the top <arg> trees per file")
    ("min-support",value<double>()->default_value(0.25),"Only examine partitions w/ PP more than this\n")
    ("consensus",value<string>(),"Report consensus trees at these comma-separated levels in [0.5, 1.0]")
    ("sub-partitions","look for partitions of taxa subsets")
    ("depth",value<int>()->default_value(1),"depth at which to look for partitions of taxa subsets")
    ("rooting",value<double>()->default_value(0.9),"depth at which to look for partitions of taxa subsets")
    ("odds-ratio",value<double>()->default_value(1.5),"Report sub-partitions if removing taxa improves the odds by at least this ratio.")
    ("verbose,v","Output more log messages on stderr.")
    ;
    
  options_description visible("All options");
  visible.add(input).add(reporting);

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("file", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: trees-consensus <file> [OPTIONS]\n";
    cout<<"Compute consensus trees and partitions.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}


struct count_more {
  bool operator()(const pair<Partition,unsigned>& p1,const pair<Partition,unsigned>& p2) const {
    return p1.second > p2.second;
  }
};
    

int main(int argc,char* argv[]) 
{ 
  try {

    std::cout.precision(3);
    std::cout.setf(ios::fixed);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //--------------------- Initialize ---------------------//
    if (args.count("seed")) {
      unsigned long seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      myrand_init();

    int skip = args["skip"].as<int>();

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    double min_support = args["min-support"].as<double>();

    // leaf taxa to ignore
    vector<string> ignore;
    if (args.count("ignore") and args["ignore"].as<string>().size() > 0)
      ignore = split(args["ignore"].as<string>(),',');

    // consensus levels 
    string c_levels = args.count("consensus") ? args["consensus"].as<string>() : "";
    vector<double> consensus_levels = get_consensus_levels(c_levels);

    double report_ratio = args["odds-ratio"].as<double>();

    bool show_sub = args.count("sub-partitions");

    //-------------- Read in tree distributions --------------//
    string filename = args["file"].as<string>();
    ifstream file(filename.c_str());
    if (not file)
      throw myexception()<<"Couldn't open file "<<filename;
      
    tree_sample tree_dist(file,skip,max,subsample,ignore);
    const unsigned N = tree_dist.size();

    dynamic_bitset<> ignore_mask = group_from_names(tree_dist.names(),vector<string>());

    //------ Compute Ml partitions or sub-partitions --------//
    vector< pair<Partition,unsigned> > all_partitions;

    if (show_sub)
    {
      int depth = args["depth"].as<int>();

      double min_rooting = args["rooting"].as<double>();

      all_partitions = get_Ml_sub_partitions_and_counts(tree_dist,min_support, ~ignore_mask,min_rooting,depth);
      //      std::cerr<<"n_sub_partitions = "<<all_partitions.size()<<"\n";
    }
    else
      all_partitions = get_Ml_partitions_and_counts(tree_dist,min_support, ~ignore_mask);


    //------  Topologies to analyze -----//
    vector<string> topologies;

    cout<<"\nTopology support: \n\n";
    for(int i=0;i < args["map-trees"].as<int>() ;i++) 
    {
      if (i >= tree_dist.topologies.size()) continue;

      string t = tree_dist.topologies[tree_dist.order[i]].topology;

      unsigned n = tree_dist.topologies[tree_dist.order[i]].count;
      double PP = double(n)/N;
      double o = odds(n,N,1);

      cout<<"MAP-"<<i<<" = "<<t<<endl;
      cout<<"   PP = "<<PP<<"       LOD = "<<log10(o)<<endl;
      cout<<"\n";
    }

    
    for(int i=0,n=0;i<tree_dist.topologies.size();i++) 
    {
      n += tree_dist.topologies[tree_dist.order[i]].count;
      double PP = double(n)/N;

      if (PP >= 0.95) {
	cout<<"95% credible set contains "<<i+1<<" topologies."<<endl;
	break;
      }
    }
    cout<<"\n\n";


    //------- Print out support for each partition --------//
    cout<<"Partition support: \n\n";

    vector<pair<Partition,unsigned> > good_partitions = thin(all_partitions, N, report_ratio);

    sort(good_partitions.begin(),good_partitions.end(), count_more());

    for(int i=0;i<good_partitions.size();i++) 
    {
      if (not informative(good_partitions[i].first))
	continue;

      unsigned n = good_partitions[i].second;

      double PP = double(n)/N;
      double o = odds(n,N,1);

      cout<<"   PP = "<<PP<<"       LOD = "<<log10(o);

      if (not good_partitions[i].first.full()) {
	double ratio = odds_ratio(good_partitions,i,N,1);
	cout<<"       ratio = "<<log10(ratio);
      }
      cout<<"       pi = "<<good_partitions[i].first<<endl;

      cout<<endl<<endl;
    }


    //----------- display M[l] consensus levels ----------//
    std::cout.precision(4);
    cout<<"\n\nConsensus levels:\n\n";

    vector<Partition> c50_sub_partitions = get_Ml_partitions(all_partitions, 0.5, N);
    vector<Partition> c50_full_partitions = select(c50_sub_partitions,&Partition::full);
    c50_sub_partitions = get_moveable_tree(c50_sub_partitions);


    vector<unsigned> levels = get_Ml_levels(all_partitions,N,min_support);

    levels.push_back(N+1);

    for(int j=0,k=0;j<levels.size() and k < consensus_levels.size();j++) 
    {
      unsigned clevel = (unsigned)(consensus_levels[k]*N);

      while (k<consensus_levels.size() and clevel < levels[j]) 
      {
	clevel = (unsigned)(consensus_levels[k]*N);

	vector<Partition> all  = get_Ml_partitions(all_partitions,consensus_levels[k],N);
	vector<Partition> sub;
	vector<Partition> full;
	for(int i=0;i<all.size();i++)
	  if (all[i].full())
	    full.push_back(all[i]);
	  else
	    sub.push_back(all[i]);

	SequenceTree consensus = get_mf_tree(tree_dist.names(),full);
	SequenceTree consensus2 = consensus;
      
	double L = consensus_levels[k]*100;
	
	cout.unsetf(ios::fixed | ios::showpoint);
	
	vector<double> bf(consensus.n_branches(),1.0);
	for(int i=0;i<bf.size();i++) 
	{
	  if (consensus.branch(i).is_leaf_branch())
	    bf[i] = -1.0;
	  else {
	    dynamic_bitset<> mask = branch_partition(consensus,i);
	    Partition p(tree_dist.names(),mask);
	    unsigned count = get_partition_count(all_partitions,p);
	    bf[i] = double(count)/N;
	  }
	  consensus2.branch(i).set_length(bf[i]);
	}

	
	cout<<" "<<L<<"-consensus-PP = "<<consensus2.write(true)<<std::endl;
	//cout<<" "<<L<<"-consensus-PP2 = "<<consensus.write_with_bootstrap_fraction(bf,false)<<std::endl;
	cout<<" "<<L<<"-consensus = "<<consensus.write(false)<<std::endl;
	
	if (show_sub) {
	  for(int i=0;i<sub.size();i++)
	    cout<<sub[i]<<endl;
	}
	cout<<endl<<endl;

	k++;
      }
	

      if (levels[j] <=N) {
	show_level(tree_dist,levels[j],c50_sub_partitions,all_partitions,show_sub,false);
	cout<<endl;
      }

    }
  }
  catch (std::exception& e) {
    std::cerr<<"trees-consensus: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
