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
#include <ext/hash_map>
#include <map>
#include <cmath>

#include "sequencetree.H"
#include "arguments.H"
#include "util.H"
#include "statistics.H"
#include "bootstrap.H"

using namespace std;
using namespace __gnu_cxx;

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

valarray<bool> branch_partition(const tree& T,int b) {
  int parent = T.branch(b).parent();
  int child = T.branch(b).child();

  valarray<bool> temp = T.partition(parent,child);
  valarray<bool> p(T.leaves());
  for(int i=0;i<p.size();i++)
    p[i] = temp[i];

  return p;
}

/// Represents a division of some of the taxa into 2 groups
struct Partition {

  valarray<bool> split;
  valarray<bool> mask;

  bool implied_by(const valarray<bool>& bm) const;

  Partition(const valarray<bool>& v) 
    :split(v),mask(true,v.size()) 
  {}
 
  Partition(const valarray<bool>& v1,const valarray<bool>& v2) 
    :split(v1),mask(v2)
  {
    split &= mask;
    assert(v1.size() == v2.size());
  }
};

bool equal(const valarray<bool>& v1,const valarray<bool>& v2) {
  assert(v1.size() == v2.size());
  bool match = true;
  for(int i=0;i<v1.size() && match;i++)
    if (v1[i] != v2[i])
      match = false;
  return match;
}

/// Does the grouping of all nodes bm, imply *this?
bool Partition::implied_by(const valarray<bool>& bm) const {
  assert(bm.size() == split.size());
  return (equal(split,bm&mask) or  equal(split,(not bm)&mask));
}
// There should be a way to check 'consistent' (might imply),
// not consistent (implies not *this)

/// Does any branch in T imply the partition p?
bool contains_partition(const SequenceTree& T,const Partition& p) {
  bool result = false;
  for(int b=0;b<T.branches() and not result;b++) {
    valarray<bool> bp = branch_partition(T,b);

    if (p.implied_by(bp))
      result = true;
  }
  return result;
}

namespace __gnu_cxx {
  template<> struct hash<string> {
    size_t hash<string>::operator()(const string& s) const {return __stl_hash_string(s.c_str());}
  };

};

struct eqstr {
  bool operator()(const string& s1, const string& s2) const
  {
    return (s1 == s2);
  }
};

struct lstr {
  bool operator()(const string& s1, const string& s2) const {
   return strcmp(s1.c_str(), s2.c_str()) < 0;
  }
};

#define foreach(a,b) for(typeof(b.begin()) a=(b).begin();a != (b).end();a++)

double moment(const vector<double>& v,int n) {
  double total=0.0;
  for(int i=0;i<v.size();i++) {
    double temp = 1.0;
    for(int j=0;j<n;j++)
      temp *= v[i];
    total += temp;
  }
  return total/v.size();
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

void report_sample(std::ostream& o,const valarray<bool>& sample_in,int pseudocount,const string& tag) {
  valarray<bool> sample = add_pseudocount(sample_in,pseudocount);

  /*---------- Basic statistics -------------*/
  const int N = sample.size();
  const int n = statistics::count(sample);
  const double P = double(n)/N;

  /*---------- Bootstrap samples of P -------------*/
  int blocksize = N/100+1;
  valarray<double> values = bootstrap_apply<bool,double>(sample,statistics::Pr,10000,blocksize);

  /*---------- Confidence Interval -------------*/
  vector<double> CI =  statistics::confidence_interval(values,0.99);

  /*------- Numbers of Constant blocks ---------*/
  unsigned nchanges1 = changes(sample,true);
  unsigned nchanges0 = changes(sample,false);
  unsigned nchanges  = nchanges1 + nchanges0;
  double nchanges_ave = (double(nchanges1) + nchanges0 +1)/2.0;

  double nchanges_perfect = (N-1)*P*(1.0-P)*2.0;
  
  /*----------------- Variances ---------------*/
  double Var_perfect = P*(1-P)/N;
  double Var_bootstrap = statistics::Var(values);

  double stddev_bootstrap = sqrt( Var_bootstrap );
  double Ne = P*(1.0-P)/Var_bootstrap;

  /*------------- Write things out -------------*/
  o<<"   P = "<<P<<"  in  ("<<CI[0]<<","<<CI[1]<<")             (1="<<n<<"  0="<<N-n<<")  ["<<nchanges_ave<<"]"<<endl;
  o<<" 10s = "<<log10(statistics::odds(P))<<"  in  ("<<log10(statistics::odds(CI[0]))<<","<<log10(statistics::odds(CI[1]))<<")";
  o<<"           odds = "<<statistics::odds(P)<<endl<<endl;

  o<<"   sigma = "<<stddev_bootstrap;
  if (nchanges > 4) {
    o<<"                     [Var]x = "<<Var_bootstrap/Var_perfect;
    o<<"   Ne = "<<Ne;
  }
  o<<endl;

  o<<"   block switches = "<<nchanges;
  if (nchanges <= 4) {
    o<<" [!!!]";
  }
  else if (nchanges <= 20) {
    o<<" [!!]";
  }
  else if (nchanges <= 50) {
    o<<" [!]";
  }

  if (nchanges > 4) {
    o<<"        [switches]x = "<<nchanges_perfect/nchanges;
    o<<"  Ne = "<<N*nchanges/nchanges_perfect;
  }
  else {
  }
  o<<endl;
}

SequenceTree standardized(const string& t,const vector<string>& remove) {
  SequenceTree T;
  T.parse(t);
  
  for(int i=0;i<remove.size();i++)
    delete_node(T,remove[i]);

  map<string,int,lstr> sequences;

  for(int i=0;i<T.get_sequences().size();i++) {
    sequences.insert(pair<string,int>(T.get_sequences()[i],i));
  }

  vector<int> newnames(T.leaves());

  int i=0;
  foreach(s,sequences) {
    pair<string,int> p = *s;
    newnames[p.second] = i;
    i++;
  }

  //  cerr<<t<<endl;
  //  cerr<<T.write()<<endl;
  //  cerr<<T.write(false)<<endl;
  T.SequenceTree::standardize(newnames);
  //  cerr<<T.write(false)<<endl;
  return T;
}


struct ordering {
  vector<int> count;

  bool operator()(int i,int j) {return count[i] < count[j];}
  
  ordering(const vector<int>& v):count(v) {}
};

const int maxtrees=10;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  std::cout.precision(3);
  std::cout.setf(ios::fixed);

  try {
    /*---------- Initialize random seed -----------*/
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

    list<string> trees;
    
    hash_map<string,int,hash<string>,eqstr> index(10000);
    vector<string> topologies;
    vector<int> count;
    
    string line;
    
    vector<string> remove;
    if (args.set("delete"))
      remove = split(args["delete"],':');
    
    /*--------- Load the trees (as strings) from STDIN ------*/
    while(getline(cin,line)) {
      trees.push_back(line);
    }

    /*--------- Count how many of each topology -----------*/
    foreach(mytree,trees) {
      
      SequenceTree T = standardized(*mytree,remove);
      string t = T.write(false);
      
      typeof(index.begin()) here = index.find(t);
      
      // it if hasn't been seen before, insert it
      if (here == index.end()) {
	topologies.push_back(t);
	count.push_back(0);
	index[t] = topologies.size()-1;
      }
      
      int i = index[t];
      count[i]++;
    }
    
    cout<<"# There were "<<trees.size()<<" trees scanned\n";
    cout<<"#    Different topologies:  "<<topologies.size()<<endl;
    cout<<endl;
    cout<<endl;
    
    /*---------------*  Sort topologies by count  ---------------*/
    vector<int> order(topologies.size());
    for(int i=0;i<order.size();i++)
      order[i] = i;
    
    sort(order.begin(),order.end(),ordering(count));
    
    int numtrees = maxtrees;
    if (topologies.size() < numtrees) numtrees = topologies.size();
    
    /*--------- If called as analyze, show topo vs time -------*/
    vector<int> iorder = invert(order);
    if (args.set("analyze")) {
      int iteration=0;
      foreach(mytree,trees) {
	SequenceTree T = standardized(*mytree,remove);
	string t = T.write(false);
      
	typeof(index.begin()) here = index.find(t);
	assert(here != index.end());

	int i = index[t];
	
	std::cout<<iteration<<"      "<<iorder[i]<<"       "<<count[i]<<"      "<<t<<"      "<<*mytree<<endl;
	iteration++;
      }

      exit(0);
    }

    /*-----------  Get tree examples for best topologies  -----------*/
    vector< SequenceTree > best_trees(numtrees);
    foreach(mytree,trees) {
      SequenceTree T = standardized(*mytree,remove);
      string t = T.write(false);
      
      // check to see if this tree has one of the best topologies
      bool done = true;
      for(int i = 0;i<best_trees.size();i++) {
	// if we've already found a tree for topology i, skip it
	if (best_trees[i].leaves() > 0)
	  continue;

	int j = order[order.size() - 1 - i];
	if (t == topologies[j])
	  best_trees[i] = T;
	else 
	  done = false;
      }
      
      if (done)
	break;
    }

    /*-----------  Choose tree to example splits from  -----------*/
    SequenceTree best;
    if (args.set("tree"))
      best.read(args["tree"]);
    else
      best = best_trees[0];

    /*-------------*  Calculate mask of taxa to ignore  --------------*/
    valarray<bool> mask = valarray<bool>(true,best.leaves());
    vector<string> ignore;
    if (args.set("ignore") and args["ignore"].size() > 0)
      ignore = split(args["ignore"],':');
    for(int i=0;i<ignore.size();i++) {
      int j = find_index(best.get_sequences(),ignore[i]);
      assert(j != best.get_sequences().size());
      mask[j] = false;
    }
      
    const int nleaves = best.leaves();
    const int nbranches = best.branches();
    
    /*-----*  Check branch length and confidence -----*/
    
    // data structure for branch length info
    vector<  vector< double > > branch_m1(best_trees.size(),vector<double>(nbranches));
    vector<  vector< double > > branch_m2(best_trees.size(),vector<double>(nbranches));
    
    // data structure for partition & topology support series
    vector< valarray<bool> > partition_series(best.branches()-best.leaves(),
					      valarray<bool>(false,trees.size())
					      );
    vector< valarray<bool> > topology_series(best_trees.size(),
					     valarray<bool>(false,trees.size())
					     );
    vector<int> branch_count(best.branches(),0);
    
    int iteration=0;
    foreach(mytree,trees) {
      SequenceTree thisone = standardized(*mytree,remove);
      string topo = thisone.write(false);
      
      // collect branch length info
      for(int i=0;i<best_trees.size();i++) {
	int j = order[order.size()-1-i];
	if (topo == topologies[j]) {
	  topology_series[i][iteration] = true;
	  for(int b=0;b<thisone.branches();b++) {
	    double d = thisone.branch(b).length();
	    branch_m1[i][b] += d;
	    branch_m2[i][b] += d*d;
	  }
	}
      }

      // collect branch confidence info
      for(int b=nleaves;b<nbranches;b++) {
	valarray<bool> p1 = branch_partition(best,b);
	if (contains_partition(thisone,Partition(p1,mask))) {
	  partition_series[b-best.leaves()][iteration] = true;
	  branch_count[b]++;
	}
      }
      iteration++;
    }
    
    for(int i=0;i<best_trees.size();i++) {
      int j = order [ order.size() - 1 - i];
      int num = count[j];
      for(int b=0;b<best_trees[i].branches();b++) {
	branch_m1[i][b] /= double(num);
	branch_m2[i][b] /= double(num);
      }
    }
    
    /*---------------  Summarize best trees ---------------*/
    cout<<"Best Trees: \n";
    for(int i=0;i<best_trees.size();i++) {
      int t = order[order.size() - 1 - i];
      cout<<"------------------------------------------------------------------"<<endl;
      cout<<topologies[t]<<endl;

      report_sample(std::cout,
		    topology_series[i],
		    pseudocount,
		    string("topology")+convertToString(i));
      cout<<endl;

      SequenceTree Tmean = best_trees[i];
      SequenceTree Tsignal = best_trees[i];
      for(int b=0;b< best_trees[i].branches();b++) {
	double m1 = branch_m1[i][b];
	double m2 = branch_m2[i][b];
	double sigma = sqrt(m2 - m1*m1);
	
	Tmean.branch(b).length() = m1;
	Tsignal.branch(b).length() = m1/sigma;
      }
      cout<<"  "<<i<<"MAPtree = "<<Tmean<<endl;
      cout<<endl;
      cout<<"  "<<i<<"signal/noise = "<<Tsignal<<endl;
      cout<<endl<<endl<<endl;
    }
    cout<<endl<<endl;
    
    /*------- Print out support for each partition --------*/
    cout<<"Support for the different partitions: \n\n";
    for(int b=best.leaves();b<best.branches();b++) {

      /*-------- Determine and print the partition -----------*/
      valarray<bool> temp = branch_partition(best,b);
      Partition p1(temp,mask);
      
      for(int i=0;i<best.leaves();i++)
	if (p1.split[i] and p1.mask[i])
	  cout<<best.seq(i)<<" ";
      cout<<"| ";
      for(int i=0;i<best.leaves();i++)
	if (not p1.split[i] and p1.mask[i])
	  cout<<best.seq(i)<<" ";
      cout<<endl;

      report_sample(std::cout,
		    partition_series[b-best.leaves()],
		    pseudocount,
		    string("partition")+convertToString(b-best.leaves()));

      std::cout<<endl<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
