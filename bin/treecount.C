#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>
#include <cmath>

#include "tree.H"
#include "arguments.H"
#include "util.H"

using namespace std;
using namespace __gnu_cxx;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

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


void do_analyze() {
}


SequenceTree standardized(const string& t) {
  SequenceTree T;
  T.parse(t);
  
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


string topology(const string& t) {
  SequenceTree T = standardized(t);
  return T.write(false);
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

  try {
    list<string> trees;
    
    hash_map<string,int,hash<string>,eqstr> index(10000);
    vector<string> topologies;
    vector<int> count;
    
    string line;
    
    while(getline(cin,line)) {
      trees.push_back(line);
    }
    
    /*************** Count how many of each type ***************/
    foreach(mytree,trees) {
      
      SequenceTree T = standardized(*mytree);
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
    
    /**************** How good are the best ones? ***************/
    vector<int> order(topologies.size());
    for(int i=0;i<order.size();i++)
      order[i] = i;
    
    sort(order.begin(),order.end(),ordering(count));
    
    int numtrees = maxtrees;
    if (topologies.size() < numtrees) numtrees = topologies.size();
    
    /********** If called as analyze, show topo vs time ********/
    vector<int> iorder = invert(order);
    if (args.set("analyze")) {
      foreach(mytree,trees) {
	SequenceTree T = standardized(*mytree);
	string t = T.write(false);
      
	typeof(index.begin()) here = index.find(t);
	assert(here != index.end());

	int i = index[t];
	
	std::cout<<iorder[i]<<"       "<<count[i]<<"      "<<t<<"      "<<*mytree<<endl;
      }

      exit(0);
    }

    cout<<endl;
    cout<<endl;
    
    /***************  Get examples of best trees ***************/
    vector< SequenceTree > best_trees(numtrees);
    foreach(mytree,trees) {
      SequenceTree T = standardized(*mytree);
      string t = T.write(false);
      
      bool done = true;
      for(int i = 0;i<best_trees.size();i++) {
	int j = order[order.size() - 1 - i];
	
	if (best_trees[i].leaves() > 0)
	  continue;
	if (t == topologies[j])
	  best_trees[i] = T;
	else 
	  done = false;
      }
      
      if (done)
	break;
    }
    
    SequenceTree best;
    if (args.set("tree"))
      best.read(args["tree"]);
    else
      best = best_trees[0];

    valarray<bool> mask = valarray<bool>(true,best.leaves());
    vector<string> ignore;
    if (args.set("ignore") and args["ignore"].size() > 0)
      ignore = split(args["ignore"],':');
    for(int i=0;i<ignore.size();i++) {
      int j = find_index(best.get_sequences(),ignore[i]);
      assert(j != best.get_sequences().size());
      mask[j] = false;
      std::cerr<<"ignore"<<i<<"   "<<ignore[i]<<"    index = "<<j<<"\n";
    }
      
    const int nleaves = best.leaves();
    const int nbranches = best.branches();
    
    /*******  Check branch length and confidence ******/
    
    // data structure for branch length info
    vector<  vector< double > > branch_m1(best_trees.size(),vector<double>(nbranches));
    vector<  vector< double > > branch_m2(best_trees.size(),vector<double>(nbranches));
    
    // data structure for branch count info
    vector<int> branch_count(nbranches,0);
    
    foreach(mytree,trees) {
      SequenceTree thisone = standardized(*mytree);
      
      // collect branch length info
      string topo = thisone.write(false);
      for(int i=0;i<best_trees.size();i++) {
	int j = order[order.size()-1-i];
	if (topo == topologies[j]) 
	  for(int b=0;b<thisone.branches();b++) {
	    double d = thisone.branch(b).length();
	    branch_m1[i][b] += d;
	    branch_m2[i][b] += d*d;
	  }
      }

      // collect branch confidence info
      for(int b=nleaves;b<nbranches;b++) {
	valarray<bool> p1 = branch_partition(best,b);
	if (contains_partition(thisone,Partition(p1,mask)))
	  branch_count[b]++;
      }
    }
    
    for(int i=0;i<best_trees.size();i++) {
      int j = order [ order.size() - 1 - i];
      int num = count[j];
      for(int b=0;b<best_trees[i].branches();b++) {
	branch_m1[i][b] /= double(num);
	branch_m2[i][b] /= double(num);
      }
    }
    
    /****************  Summarize best trees ****************/
    cout<<"Best Trees: \n";
    for(int i=0;i<best_trees.size();i++) {
      int t = order[order.size() - 1 - i];
      cout<<topologies[t]<<endl;
      cout<<double(count[t])/trees.size()<<"            ("<<count[t]<<")"<<endl;
      
      for(int b=0;b< best_trees[i].branches();b++) {
	double m1 = branch_m1[i][b];
	double m2 = branch_m2[i][b];
	double stddev = sqrt(m2 - m1*m1);
	
	best_trees[i].branch(b).length() = m1;
	cout<<b<<"  "<<m1<<"    "<<stddev<<endl;
      }
      cout<<best_trees[i]<<endl;
      cout<<endl<<endl;
    }
    cout<<endl<<endl;
    
    /******** Print out support for each partition *********/
    cout<<"Support for the different partitions: \n";
    for(int b=best.leaves();b<best.branches();b++) {
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
      cout<<double(branch_count[b])/trees.size()<<
	"    ("<<branch_count[b]<<")"<<endl<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
