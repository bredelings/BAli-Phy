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


// use magic-squares...
// That is a good way to get pairs.
// It is still random though.

// We can cull to some specific length... e.g. 1000

// Add trees1 vs trees2 arguments to compare distances from different programs

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    bool topology_only = false;
    if (args.set("topology"))
      topology_only = true;

    vector<SequenceTree> trees;
    
    // read in the trees
    string line;
    while(getline(cin,line)) {
      SequenceTree T = standardized(line); 
      if (topology_only) 
	for(int b=0;b<T.branches();b++)
	  T.branch(b).length() = 1.0;

      trees.push_back( T );
    }
    cout<<"# There were "<<trees.size()<<" trees scanned\n";

    // set the window size
    int window = trees.size()/20;
    if (args.set("window"))
      window = convertTo<int>(args["window"]);

    // bound the window
    if (window >= trees.size()/2)
      window = trees.size()/2;
    cout<<"# window size = "<<window<<endl;

    // calculate the distances
    valarray<double> distances(0.0,window);
    const int passes = trees.size()-window;
    for(int i=0;i<passes;i++) {
      const SequenceTree& T1 = trees[i];
      for(int j=0;j<window;j++) {
	const SequenceTree& T2 = trees[i+j];
	distances[j] += distance1(T1,T2);
      }
    }
    distances /= passes;

    // write out the average distances
    for(int i=0;i<distances.size();i++)
      cout<<i<<"   "<<distances[i]<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
