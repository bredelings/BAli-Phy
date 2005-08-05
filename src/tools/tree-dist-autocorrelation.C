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
#include "tree-dist.H"

using namespace std;

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
    const int maxsize = int( double(trees.size()/20.0 + 1.0 ) );
    int window = args.loadvalue("window", maxsize);

    // bound the window
    if (window >= trees.size()/2)
      window = trees.size()/2;
    cout<<"# window size = "<<window<<endl;

    // calculate the distances
    valarray<double> distances(0.0,window);
    for(int i=0;i<trees.size();i++) {
      const SequenceTree& T1 = trees[i];
      for(int j=i;j<trees.size() and j-i < distances.size();j++) {
	const SequenceTree& T2 = trees[j];
	distances[j-i] += branch_distance(T1,T2);
      }
    }
    for(int i=0;i<distances.size();i++)
      distances[i] /= (trees.size()-i);

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
