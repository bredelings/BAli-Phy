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

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

double getsum(const valarray<double>& v) {
  return v.sum();
}

unsigned changes(const valarray<bool>& sample,bool value) {
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
  

bool separated_by(const vector<double>& CI1,const vector<double>& CI2,double dx) {
  assert(CI1.size() == 2);
  assert(CI2.size() == 2);

  if ((log10(CI1[1]) + dx < log10(CI2[0])) and (log10(CI1[1] + dx < 0 or dx < log10(CI2[0]))) )
    return true;

  if ((log10(CI2[1]) + dx < log10(CI1[0])) and (log10(CI2[1] + dx < 0 or dx < log10(CI1[0]))) )
    return true;

  return false;
}

bool report_sample(std::ostream& o,const vector<valarray<bool> >& sample_in,int pseudocount,vector<double>& support,
		   int blocksize, double dx=-1) {
  o.precision(3);
  o.setf(ios::fixed);

  vector<valarray<bool> > sample;
  for(int i=0;i<sample_in.size();i++)
    sample.push_back( statistics::add_pseudocount(sample_in[i],pseudocount) );

  support.resize(sample.size());
  /*---------- Basic statistics -------------*/
  vector<int> N(sample.size());
  vector<int> n(sample.size());
  vector<double> P(sample.size());
  for(int i=0;i<sample.size();i++) {
    N[i] = sample[i].size();
    n[i] = statistics::count(sample[i]);
    P[i] = double(n[i])/N[i];
  }

  vector< valarray<double> > values;
  vector< vector<double> > CI(sample.size());
  vector< unsigned > nchanges ( sample.size());
  vector< unsigned > nchanges_ave (sample.size());

  vector<double> Var_perfect(sample.size());
  vector<double> Var_bootstrap(sample.size());
  vector<double> stddev_bootstrap(sample.size());
  vector<double> Ne(sample.size());

  for(int i=0;i<sample.size();i++) {

    //---------- Bootstrap samples of P -------------//
    values.push_back( bootstrap_apply<bool,double>(sample[i],statistics::Pr,10000,blocksize) );

    //---------- Confidence Interval -------------//
    CI[i] = statistics::confidence_interval(values[i],0.95);
    support[i] = conservative(CI[i]);

    //------- Numbers of Constant blocks ---------//
    nchanges[i] = changes(sample[i],true) + changes(sample[i],false);

    nchanges_ave[i] = (nchanges[i] + 1)/2;

    //----------------- Variances ---------------//
    Var_perfect[i] = P[i]*(1.0-P[i])/N[i];
    Var_bootstrap[i] = statistics::Var(values[i]);

    stddev_bootstrap[i] = sqrt( Var_bootstrap[i] );

    Ne[i] = P[i]*(1.0-P[i])/Var_bootstrap[i];
  }

  bool different = (dx <= 0) or sample.size()==1;
  if (not different) {
    for(int i=0;i<sample.size();i++)
      for(int j=0;j<i;j++)
	if (separated_by(CI[i],CI[j],dx))
	  different = true;
  }
  if (not different)
    return false;


  for(int i=0;i<sample.size();i++) {
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
    
  for(int i=0;i<sample.size();i++) {
    o<<"   10s = "<<log10(statistics::odds(P[i]))<<"  in  ("<<log10(statistics::odds(CI[i][0]))<<","<<log10(statistics::odds(CI[i][1]))<<")";
    if (nchanges_ave[i] > 6)
      o<<"    [Var]x = "<<Var_bootstrap[i]/Var_perfect[i]<<"          Ne = "<<Ne[i];
    o<<endl;
  }

  return true;
}

vector<double> get_consensus_levels(const string& s) {
  vector<double> levels;
  if (s.size())
    levels = split<double>(s,',');

  std::sort(levels.begin(),levels.end());

  // remove levels below 0.5
  while(not levels.empty() and levels[0] < 0.5) {
    std::cerr<<"Ignorning bad consensus level '"<<levels[0]<<"'"<<std::endl;
    levels.erase(levels.begin());
  }

  // remove levels above 1.0
  while(not levels.empty() and levels.back() > 1.0) {
    std::cerr<<"Ignorning bad consensus level '"<<levels.back()<<"'"<<std::endl;
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

/// Merge the lists in 'partition_sets' while removing duplicates
vector<Partition> merge(const vector<vector<Partition> >& partition_sets) 
{
  vector<Partition> partitions;
  for(int i=0;i<partition_sets.size();i++) 
    add_unique_partitions(partitions,partition_sets[i]);
  return partitions;
}

bool full_partition(const Partition& P) {
  for(int i=0;i<P.size();i++)
    if (not (P.group1[i] or P.group2[i]))
      return false;
  return true;
}

vector<Partition> get_full_partitions(const vector<Partition>& partitions) {
  vector<Partition> full;
  for(int i=0;i<partitions.size();i++)
    if (full_partition(partitions[i]))
      full.push_back(partitions[i]);
  return full;
}


vector< vector<Partition> >
get_sub_partitions(const tree_sample& tree_dist,const vector<double>& levels) 
{
  vector< vector<Partition> > partition_sets(levels.size());
  for(int i=0;i<partition_sets.size();i++)
    partition_sets[i] = get_Ml_sub_partitions(tree_dist,levels[i]);

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

vector< vector<vector<Partition> > >
get_sub_partitions(const vector<tree_sample>& tree_dists,const vector<double>& levels) 
{
  vector< vector< vector<Partition> > > partitions(tree_dists.size());
  for(int i=0;i<partitions.size();i++)
    partitions[i] = get_sub_partitions(tree_dists[i],levels);

  return partitions;
}

vector< vector<vector<Partition> > >
get_full_partitions(const vector<tree_sample>& tree_dists,const vector<double>& levels) 
{
  vector< vector< vector<Partition> > > partitions(tree_dists.size());
  for(int i=0;i<partitions.size();i++)
    partitions[i] = get_full_partitions(tree_dists[i],levels);

  return partitions;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("delete", value<string>(),"comma-separated list of taxa to remove from trees")
    ("ignore", value<string>(),"comma-separated list of taxa to ignore in partitions")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("files",value<vector<string> >()->multitoken(),"tree files to examine")
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
    ("map-trees",value<int>()->default_value(1),"Only report the top <arg> trees per file")
    ("consensus",value<string>(),"comma-separated consensus levels in [0.5, 1.0] for majority tree")
    ("sub-partitions","look for partitions of taxa subsets")
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
    cout<<"Usage: tree-dist-compare <file1> <file2> ... [OPTIONS]\n";
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

    vector<string> remove;
    if (args.count("delete"))
      remove = split(args["delete"].as<string>(),',');

    double compare_dx = args["separation"].as<double>();
    
    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    //-------------- Read in tree distributions --------------//
    if (not args.count("files"))
      throw myexception()<<"Tree files not specified!";

    vector<string> files = args["files"].as< vector<string> >();
    vector<tree_sample> tree_dists;
    vector<SequenceTree> MAP_trees;

    for(int i=0;i<files.size();i++) {
      
      ifstream file(files[i].c_str());
      if (not file)
	throw myexception()<<"Couldn't open file "<<files[i];
      
      tree_dists.push_back(tree_sample(file,remove,skip,max));

      MAP_trees.push_back( tree_dists.back().topologies[tree_dists.back().order[0]].T );

      if (i > 0 and MAP_trees[i].get_sequences() != MAP_trees[i-1].get_sequences())
	throw myexception()<<"Tree load from file '"<<files[i]<<"' has different taxa that previous trees.";
    }
    
    //----------  Determine block size ----------//
    int blocksize = tree_dists[0].size()/100+1;
    for(int i=1;i<tree_dists.size();i++)
      blocksize = std::min(blocksize,tree_dists[i].size()/100+1);

    if (args.count("blocksize"))
      blocksize = args["blocksize"].as<int>();
    
    std::cout<<"blocksize = "<<blocksize<<endl<<endl;

    //--------------- Distance between MAP trees ----------//
    cout<<endl;
    for(int i=0;i<tree_dists.size();i++)
      for(int j=0;j<i;j++) {
	cout<<"Topology distance between MAP trees for "<<i<<" and "<<j<<" = "<<topology_distance(MAP_trees[i],MAP_trees[j])<<endl;
	cout<<"Branch distance between MAP trees for "<<i<<" and "<<j<<" = "<<branch_distance(MAP_trees[i],MAP_trees[j])<<endl;
      }
    cout<<endl;

    for(int i=0;i<tree_dists.size();i++) 
      cout<<"MAPtree_"<<i<<" = "<<MAP_trees[i]<<endl<<endl;

    //----------  Calculate mask of leaf taxa to ignore in partitions ----------//
    valarray<bool> mask = valarray<bool>(true,MAP_trees[0].n_leaves());

    vector<string> ignore;
    if (args.count("ignore") and args["ignore"].as<string>().size() > 0)
      ignore = split(args["ignore"].as<string>(),',');

    for(int i=0;i<ignore.size();i++) {
      int j = find_index(MAP_trees[0].get_sequences(),ignore[i]);
      assert(j != MAP_trees[0].get_sequences().size());
      mask[j] = false;
    }

    //---------- compute consensus levels ----------------//
    string c_levels = "";
    if (args.count("consensus"))
      c_levels = args["consensus"].as<string>();
    vector<double> consensus_levels = get_consensus_levels(c_levels);

    //------ Compute Ml partitions or sub-partitions --------//
    vector< vector< vector< Partition > > > sub_partitions;
    vector< vector< vector< Partition > > > full_partitions;

    if (args.count("sub-partitions"))
    {
      sub_partitions = get_sub_partitions(tree_dists,consensus_levels);

      full_partitions = sub_partitions;
      for(int i=0;i<full_partitions.size();i++)
	for(int j=0;j<full_partitions[i].size();j++)
	  full_partitions[i][j] = get_full_partitions( full_partitions[i][j] );
    }
    else {
      full_partitions = get_full_partitions(tree_dists,consensus_levels);
      sub_partitions = full_partitions;
    }


    vector<vector< Partition> > dist_partitions(sub_partitions.size());
    for(int i=0;i<dist_partitions.size();i++)
      dist_partitions[i] = merge(sub_partitions[i]);

    vector< Partition > partitions = merge(dist_partitions);

    //----- Add partitions from branches of MAP trees -------//
    vector< vector< int> > branch_to_partitions(tree_dists.size());
    for(int i=0;i<tree_dists.size();i++) {
      // add MAP tree partitions to 'partitions' if not yet there
      branch_to_partitions[i].resize(MAP_trees[i].n_branches());
      for(int b=MAP_trees[i].n_leaves();b<MAP_trees[i].n_branches();b++) {
	valarray<bool> p1 = branch_partition(MAP_trees[i],b);
	
	Partition p(MAP_trees[i].get_sequences(),p1,mask);
	
	if (not includes(partitions,p)) {
	  branch_to_partitions[i][b] = partitions.size();
	  partitions.push_back(p);
	}
	else
	  branch_to_partitions[i][b] = find_index(partitions,p);
      }
    }

    //------  Topologies to analyze -----//
    vector<string> topologies;
    for(int j=0;j<tree_dists.size();j++) {

      for(int i=0;i < args["map-trees"].as<int>() ;i++) 
      {
	if (i >= tree_dists[j].topologies.size()) continue;

	string t = tree_dists[j].topologies[tree_dists[j].order[i]].topology;

	if (not includes(topologies,t))
	  topologies.push_back(t);

      }
    }

    //------ Create support bitvectors for the hypotheses (topology,partitions) ----//
    vector< vector< valarray<bool> > > topology_series(topologies.size());

    for(int i=0;i<topologies.size();i++)
      for(int j=0;j<tree_dists.size();j++)
	topology_series[i].push_back( tree_dists[j].supports_topology( topologies[i] ) );


    vector< vector< valarray<bool> > >partition_series(partitions.size());

    for(int i=0;i<partitions.size();i++) 
      for(int j=0;j<tree_dists.size();j++)
	partition_series[i].push_back( tree_dists[j].supports_partition( partitions[i] ) );

    //---------------  Summarize best trees ---------------//
    cout<<"Best Topologies: \n";
    for(int i=0;i<topologies.size();i++) {
      std::ostringstream report;
      vector<double> support;
      bool show = report_sample(report,
				topology_series[i],
				pseudocount,
				support,
				blocksize,
				compare_dx);

      if (not show)
	continue;
      cout<<"------------------------------------------------------------------"<<endl;
      cout<<topologies[i]<<endl;
      cout<<endl;
      
      cout<<report.str();
	
      cout<<endl;
    }
    cout<<endl<<endl;

    //------- Print out support for each partition --------//
    vector< vector<double> > partition_support(partition_series.size());
    cout<<"Support for the different partitions: \n\n";
    for(int i=0;i<partitions.size();i++) {
      std::ostringstream report;
      bool show = report_sample(report,
				partition_series[i],
				pseudocount,
				partition_support[i],
				blocksize,
				compare_dx);

      //-------- Determine and print the partition -----------//
      if (not show) continue;

      cout<<partitions[i]<<endl;

      cout<<report.str();

      cout<<endl<<endl;
    }


    //----------- display M[l] consensus trees ----------//
    cout<<"\n\nConsensus trees:\n";
    for(int l=0;l<consensus_levels.size();l++) {
	
      for(int i=0;i<tree_dists.size();i++) 
      {
	const vector<Partition>& full = full_partitions[i][l];

	SequenceTree MF = get_mf_tree(MAP_trees[i].get_sequences(),full);

	cout<<"\n";
	cout<<" sample = "<<i;
 	cout<<"   level = "<<consensus_levels[l];
	cout<<"   full = "<<full.size()<<"/"<<MAP_trees[i].n_leaves()-3;
	if (sub_partitions.size())
	  cout<<"   sub = "<<sub_partitions[i][l].size();
	cout<<"   PP = "<<tree_dists[i].PP(full)<<"\n";;
	cout<<"\n";
	  
	cout<<" consensus-"<<i<<" = "<<MF.write(false)<<std::endl;
      }
      cout<<std::endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
