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

valarray<double> block_sample(const valarray<bool>& v,int blocksize=1) {
  valarray<double> temp(v.size()/blocksize);
  for(int block=0;block<temp.size();block++) {
    temp[block]=0;
    for(int i=blocksize*block;i<blocksize*(block+1);i++) 
      if (v[i]) temp[block]++;
    
  }
  return temp;
}

unsigned changes(const valarray<bool>& sample,bool value) {
  unsigned count=0;
  for(int i=0;i<sample.size()-1;i++) {
    if (sample[i] == value and sample[i+1] != value)
      count++;
  }
  return count;
}


valarray<bool> add_pseudocount(const valarray<bool>& sample1,int pseudocount) {
  valarray<bool> sample2(sample1.size() + 2*pseudocount);

  int i=0;
  for(int j=0;j<pseudocount;i++,j++)
    sample2[i] = true;

  for(;i<sample1.size();i++)
    sample2[i] = sample1[i];

  for(int j=0;j<pseudocount;i++,j++)
    sample2[i] = false;

  return sample2;
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

bool report_sample(std::ostream& o,const valarray<bool>& sample1_in,const valarray<bool>& sample2_in,int pseudocount,double dx=-1) {
  o.precision(3);
  o.setf(ios::fixed);

  valarray<bool> sample1 = add_pseudocount(sample1_in,pseudocount);
  valarray<bool> sample2 = add_pseudocount(sample2_in,pseudocount);

  /*---------- Basic statistics -------------*/
  const int N1 = sample1.size();
  const int n1 = statistics::count(sample1);
  const double P1 = double(n1)/N1;

  const int N2 = sample2.size();
  const int n2 = statistics::count(sample2);
  const double P2 = double(n2)/N2;

  /*---------- Bootstrap samples of P -------------*/
  int blocksize1 = N1/100+1;
  valarray<double> values1 = bootstrap_apply<bool,double>(sample1,statistics::Pr,10000,blocksize1);

  int blocksize2 = N2/100+1;
  valarray<double> values2 = bootstrap_apply<bool,double>(sample2,statistics::Pr,10000,blocksize2);

  /*---------- Confidence Interval -------------*/
  vector<double> CI1 =  statistics::confidence_interval(values1,0.95);
  vector<double> CI2 =  statistics::confidence_interval(values2,0.95);

  /*------- Numbers of Constant blocks ---------*/
  unsigned nchanges1 = changes(sample1,true) + changes(sample1,false);
  unsigned nchanges2 = changes(sample2,true) + changes(sample2,false);

  unsigned nchanges1_ave = (nchanges1 + 1)/2;
  unsigned nchanges2_ave = (nchanges2 + 1)/2;

  //  double nchanges1_perfect = (N1-1)*P1*(1.0-P1)*2.0;
  //  double nchanges2_perfect = (N2-1)*P2*(1.0-P2)*2.0;
  
  /*----------------- Variances ---------------*/
  double Var1_perfect = P1*(1.0-P1)/N1;
  double Var1_bootstrap = statistics::Var(values1);

  double Var2_perfect = P2*(1.0-P2)/N2;
  double Var2_bootstrap = statistics::Var(values2);

  double stddev1_bootstrap = sqrt( Var1_bootstrap );
  double stddev2_bootstrap = sqrt( Var2_bootstrap );

  double Ne1 = P1*(1.0-P1)/Var1_bootstrap;
  double Ne2 = P2*(1.0-P2)/Var2_bootstrap;

  if (dx > 0 and not separated_by(CI1,CI2,dx))
    return false;

  /*------------- Write things out -------------*/
  o<<"   P1 = "<<P1<<"  in  ("<<CI1[0]<<","<<CI1[1]<<")        (1="<<n1<<"  0="<<N1-n1<<")  ["<<nchanges1_ave;
  if (nchanges1 <= 4) {
    o<<" !!!";
  }
  else if (nchanges1 <= 20) {
    o<<" !!";
  }
  else if (nchanges1 <= 50) {
    o<<" !";
  }
  o<<"]";
  if (nchanges1_ave > 6)
    o<<"     sigma = "<<stddev1_bootstrap;
  o<<endl;


  o<<"   P2 = "<<P2<<"  in  ("<<CI2[0]<<","<<CI2[1]<<")        (1="<<n2<<"  0="<<N2-n2<<")  ["<<nchanges2_ave;
  if (nchanges2 <= 4) {
    o<<" !!!";
  }
  else if (nchanges2 <= 20) {
    o<<" !!";
  }
  else if (nchanges2 <= 50) {
    o<<" !";
  }
  o<<"]";
  if (nchanges2_ave > 6)
    o<<"     sigma = "<<stddev2_bootstrap;
  o<<endl;

  o<<endl;

  o<<"   10s = "<<log10(statistics::odds(P1))<<"  in  ("<<log10(statistics::odds(CI1[0]))<<","<<log10(statistics::odds(CI1[1]))<<")";
  if (nchanges1_ave > 6)
    o<<"    [Var]x = "<<Var1_bootstrap/Var1_perfect<<"          Ne = "<<Ne1;
  o<<endl;
  o<<"   10s = "<<log10(statistics::odds(P2))<<"  in  ("<<log10(statistics::odds(CI2[0]))<<","<<log10(statistics::odds(CI2[1]))<<")";
  if (nchanges2_ave > 6)
    o<<"    [Var]x = "<<Var2_bootstrap/Var2_perfect<<"          Ne = "<<Ne2;
  o<<endl;

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
    
    //-------------- Read in tree distributions --------------//
    if (not args.set("file1"))
      throw myexception()<<"First tree file not specified!";
    ifstream file1(args["file1"].c_str());
    if (not file1)
      throw myexception()<<"Couldn't open file "<<args["file1"];
      
    tree_sample tree_dist1(file1,remove);

    
    if (not args.set("file2"))
      throw myexception()<<"First tree file not specified!";
    ifstream file2(args["file2"].c_str());
    if (not file2)
      throw myexception()<<"Couldn't open file "<<args["file2"];
    tree_sample tree_dist2(file2,remove);
    
    SequenceTree MAP_tree_1 = tree_dist1.tree_mean[tree_dist1.order[0]];
    SequenceTree MAP_tree_2 = tree_dist2.tree_mean[tree_dist2.order[0]];

    //----------  Calculate mask of leaf taxa to ignore in partitions ----------//
    valarray<bool> mask = valarray<bool>(true,MAP_tree_1.leaves());
    vector<string> ignore;
    if (args.set("ignore") and args["ignore"].size() > 0)
      ignore = split(args["ignore"],':');
    for(int i=0;i<ignore.size();i++) {
      int j = find_index(MAP_tree_1.get_sequences(),ignore[i]);
      assert(j != MAP_tree_1.get_sequences().size());
      mask[j] = false;
    }
      
    //------  Compute partitions to analyze -----//
    vector< Partition > partitions;
    for(int b=MAP_tree_1.leaves();b<MAP_tree_1.branches();b++) {
      valarray<bool> p1 = branch_partition(MAP_tree_1,b);

      partitions.push_back( Partition(MAP_tree_1.get_sequences(),p1,mask) );
    }

    for(int b=MAP_tree_2.leaves();b<MAP_tree_2.branches();b++) {
      valarray<bool> p1 = branch_partition(MAP_tree_2,b);

      Partition p(MAP_tree_2.get_sequences(),p1,mask);

      if (not includes(partitions,p))
	partitions.push_back(p);
    }

    //------  Topologies to analyze -----//
    const int maxtrees = 4;

    vector<string> topologies;
    for(int i=0;i<maxtrees and i < tree_dist1.size();i++) {
      topologies.push_back(tree_dist1.topologies[tree_dist1.order[i]]);
    }
    for(int i=0;i<maxtrees and i < tree_dist2.size();i++) {
      string t = tree_dist2.topologies[tree_dist2.order[i]];

      if (not includes(topologies,t))
	topologies.push_back(t);
    }

    //------ Create support bitvectors for the hypotheses (topology,partitions) ----//

    vector< valarray<bool> > topology_series1;
    vector< valarray<bool> > topology_series2;

    for(int i=0;i<topologies.size();i++) {
      topology_series1.push_back( tree_dist1.supports_topology( topologies[i] ) );
      topology_series2.push_back( tree_dist2.supports_topology( topologies[i] ) );
    }

    vector< valarray<bool> > partition_series1;
    vector< valarray<bool> > partition_series2;

    for(int i=0;i<partitions.size();i++) {
      partition_series1.push_back( tree_dist1.supports_partition( partitions[i] ) );
      partition_series2.push_back( tree_dist2.supports_partition( partitions[i] ) );
    }

    //--------------- Distance between MAP trees ----------//

    cout<<endl;
    cout<<"Distance between two MAP trees = "<<topology_distance(MAP_tree_1,MAP_tree_2)<<endl;
    cout<<"Distance between two MAP trees = "<<branch_distance(MAP_tree_1,MAP_tree_2)<<endl;
    cout<<endl;

    cout<<"MAPtree_1 = "<<MAP_tree_1<<endl<<endl;
    cout<<"MAPtree_2 = "<<MAP_tree_2<<endl<<endl;

    //---------------  Summarize best trees ---------------//
    cout<<"Best Topologies: \n";
    for(int i=0;i<topologies.size();i++) {
      std::ostringstream report;
      bool show = report_sample(report,
				topology_series1[i],
				topology_series2[i],
				pseudocount,
				compare_dx);

      if (not show)
	continue;
      cout<<"------------------------------------------------------------------"<<endl;
      cout<<topologies[i]<<endl;
      cout<<endl;
      
      cout<<report.str();
	
      cout<<endl;
      
      int index1 = tree_dist1.get_index(topologies[i]);
      if (index1 >= 0)
	cout<<"  "<<i<<"MAPtree1 = "<<tree_dist1.tree_mean[index1]<<endl;
      else 
	cout<<"  Topology "<<i<<" not found in sample 1."<<endl;
      cout<<endl;
      
      int index2 = tree_dist2.get_index(topologies[i]);
      if (index2 >= 0) 
	cout<<"  "<<i<<"MAPtree2 = "<<tree_dist2.tree_mean[index2]<<endl;
      else 
	cout<<"  Topology "<<i<<" not found in sample 2."<<endl;
      cout<<endl<<endl;
    }
    cout<<endl<<endl;

    //------- Print out support for each partition --------//
    cout<<"Support for the different partitions: \n\n";
    for(int i=0;i<partitions.size();i++) {
      std::ostringstream report;
      bool show = report_sample(report,
				partition_series1[i],
				partition_series2[i],
				pseudocount,
				compare_dx);

      //-------- Determine and print the partition -----------//
      if (not show) continue;

      cout<<partitions[i]<<endl;

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
