#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>
#include <cmath>

#include "tree.H"

namespace __gnu_cxx {
  template<> struct hash<string> {
    size_t hash<string>::operator()(const string& s) const {return __stl_hash_string(s.c_str());}
  };

};

using namespace std;
using namespace __gnu_cxx;

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

int contains_partition(const SequenceTree& T,const valarray<bool>& p) {
  int match = -1;
  for(int b=T.leaves();b<T.branches();b++) {
    int parent2 = T.branch(b).parent();
    int child2 = T.branch(b).child();

    valarray<bool> p2 = T.partition(parent2,child2);

    //	cerr<<b<<" ";
    //	for(int i=0;i<p2.size();i++)
    //	  cerr<<p2[i]<<" ";
    //	cerr<<endl;


    if (p[0] != p2[0])
      p2 = !p2;

    assert(p[0] == p2[0]);

    bool m = true;
    for(int i=0;i<T.leaves();i++)
      if (p[i] != p2[i])
	m = false;

    if (m) {
      match = b;
      break;
    }
  }
  return match;
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

int main() {
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
    
    cout<<"There were "<<trees.size()<<" trees scanned\n";
    cout<<"   Different topologies:  "<<topologies.size()<<endl;
    
    /**************** How good are the best ones? ***************/
    vector<int> order(topologies.size());
    for(int i=0;i<order.size();i++)
      order[i] = i;
    
    sort(order.begin(),order.end(),ordering(count));
    
    int numtrees = maxtrees;
    if (topologies.size() < numtrees) numtrees = topologies.size();
    
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
    
    const SequenceTree& best = best_trees[0];
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
	int parent = best.branch(b).parent();
	int child = best.branch(b).child();
	valarray<bool> p1 = best.partition(parent,child);
	
	int match = contains_partition(thisone,p1);
	
	if (match != -1)
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
      int parent = best.branch(b).parent();
      int child = best.branch(b).child();
      valarray<bool> p1 = best.partition(parent,child);
      
      for(int i=0;i<best.leaves();i++)
	if (p1[i])
	  cout<<best.seq(i)<<" ";
      cout<<"| ";
      for(int i=0;i<best.leaves();i++)
	if (not p1[i])
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
