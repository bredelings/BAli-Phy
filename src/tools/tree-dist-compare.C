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

#include "sequencetree.H"
#include "arguments.H"
#include "util.H"
#include "statistics.H"
#include "bootstrap.H"
#include "tree-dist.H"

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

    /*------- Numbers of Constant blocks ---------*/
    nchanges[i] = changes(sample[i],true) + changes(sample[i],false);

    nchanges_ave[i] = (nchanges[i] + 1)/2;

    //  double nchanges_perfect[i] = (N[i]-1)*P[i]*(1.0-P[i])*2.0;
  
    //----------------- Variances ---------------//
    Var_perfect[i] = P[i]*(1.0-P[i])/N[i];
    Var_bootstrap[i] = statistics::Var(values[i]);

    stddev_bootstrap[i] = sqrt( Var_bootstrap[i] );

    Ne[i] = P[i]*(1.0-P[i])/Var_bootstrap[i];
  }

  bool different=(dx < 0) or sample.size()==1;
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

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  std::cout.precision(3);
  std::cout.setf(ios::fixed);

  try {
    //--------------------- Initialize ---------------------//
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    std::cout<<"random seed = "<<seed<<endl<<endl;
    
    int pseudocount = args.loadvalue("pseudocount",0);
    std::cout<<"pseudocount = "<<pseudocount<<endl<<endl;

    vector<string> remove;
    if (args.set("delete"))
      remove = split(args["delete"],':');

    double compare_dx = -1;
    if (args.set("compare_only"))
      compare_dx=1.0;
    compare_dx = args.loadvalue("separation",compare_dx);
    
    int skip = args.loadvalue("skip",0);

    //-------------- Read in tree distributions --------------//
    if (not args.set("files") or args["files"] == "")
      throw myexception()<<"Tree files not specified!";

    vector<string> files = split(args["files"],',');
    vector<tree_sample> tree_dists;
    vector<SequenceTree> MAP_trees;

    for(int i=0;i<files.size();i++) {

      ifstream file(files[i].c_str());
      if (not file)
	throw myexception()<<"Couldn't open file "<<files[i];
      
      tree_dists.push_back(tree_sample(file,remove,skip));

      MAP_trees.push_back( tree_dists.back().tree_mean[tree_dists.back().order[0]] );

      if (i > 0 and MAP_trees[i].get_sequences() != MAP_trees[i-1].get_sequences())
	throw myexception()<<"Tree load from file '"<<files[i]<<"' has different taxa that previous trees.";
    }
    
    //----------  Determine block size ----------//

    int blocksize = tree_dists[0].size()/100+1;
    for(int i=1;i<tree_dists.size();i++)
      blocksize = std::min(blocksize,tree_dists[i].size()/100+1);

    blocksize = args.loadvalue("blocksize",blocksize);
    
    std::cout<<"blocksize = "<<blocksize<<endl<<endl;

    //--------------- Distance between MAP trees ----------//

    cout<<endl;
    for(int i=0;i<tree_dists.size();i++)
      for(int j=0;j<i;j++) {
	cout<<"Distance between MAP trees for "<<i<<" and "<<j<<" = "<<topology_distance(MAP_trees[i],MAP_trees[j])<<endl;
	cout<<"Distance between MAP trees for "<<i<<" and "<<j<<" = "<<branch_distance(MAP_trees[i],MAP_trees[j])<<endl;
      }
    cout<<endl;

    for(int i=0;i<tree_dists.size();i++) 
      cout<<"MAPtree_"<<i<<" = "<<MAP_trees[i]<<endl<<endl;

    //----------  Calculate mask of leaf taxa to ignore in partitions ----------//
    valarray<bool> mask = valarray<bool>(true,MAP_trees[0].leaves());

    vector<string> ignore;
    if (args.set("ignore") and args["ignore"].size() > 0)
      ignore = split(args["ignore"],':');

    for(int i=0;i<ignore.size();i++) {
      int j = find_index(MAP_trees[0].get_sequences(),ignore[i]);
      assert(j != MAP_trees[0].get_sequences().size());
      mask[j] = false;
    }
      
    //------  Compute partitions to analyze -----//
    vector< Partition > partitions;
    vector< vector< int> > branch_to_partitions(tree_dists.size());

    for(int i=0;i<tree_dists.size();i++) {
      branch_to_partitions[i].resize(MAP_trees[i].branches());

      for(int b=MAP_trees[i].leaves();b<MAP_trees[i].branches();b++) {
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
    for(int i=0;topologies.size() < 10 and i<5 ;i++) {

      for(int j=0;j<tree_dists.size();j++) {

	if (i < tree_dists[j].topologies.size()) {
	  string t = tree_dists[j].topologies[tree_dists[j].order[i]];

	  if (not includes(topologies,t))
	    topologies.push_back(t);
	}
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
      
      for(int j=0;j<tree_dists.size();j++) {
	int index = tree_dists[j].get_index(topologies[i]);
	if (index >= 0)
	  cout<<"  "<<i<<"MAPtree"<<j<<" = "<<tree_dists[j].tree_mean[index]<<endl;
	else 
	  cout<<"  Topology "<<i<<" not found in sample "<<j<<"."<<endl;
      }
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


    for(int i=0;i<tree_dists.size();i++) {
      for(int b=0;b<MAP_trees[i].leaves();b++)
	MAP_trees[i].branch(b).length() = 1.0;
      for(int b=MAP_trees[i].leaves();b<MAP_trees[i].branches();b++)
	MAP_trees[i].branch(b).length() = partition_support[ branch_to_partitions[i][b] ][i];
      cout<<"MAPsupport_"<<i<<" = "<<MAP_trees[i]<<endl<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
