#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>
#include <cmath>

#include "myexception.H"
#include "sequencetree.H"
#include "arguments.H"

using namespace std;

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

SequenceTree standardized(const SequenceTree& T1) {
  SequenceTree T = T1;
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

SequenceTree standardized(const string& t) {
  SequenceTree T;
  T.parse(t);
  
  return standardized(T);
}

string topology(const string& t) {
  SequenceTree T = standardized(t);
  return T.write(false);
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    // Load the target tree
    SequenceTree target;
    string target_string;
    if (args.set("tree")) {
      target.read(args["tree"]);
      target_string = target.write(false);
    }
    else 
      throw myexception("Tree not specified! (tree=<filename> )");

    
    string line;
    while(getline(cin,line)) {
      SequenceTree T = standardized(line);
      if (T.write(false) == target_string)
	cout<<"1\n";
      else
	cout<<"0\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
