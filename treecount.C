#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>

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

string topology(string t) {
  SequenceTree T(t);
  
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

  std::cerr<<t<<std::endl;
  std::cerr<<T.write()<<std::endl;
  std::cerr<<T.write(false)<<std::endl;
  T.SequenceTree::standardize(newnames);
  std::cerr<<T.write(false)<<std::endl;

  return T.write(false);
}

int main() {
  list<string> trees;

  //FIXME - need to write a class that does strings!
  hash_map<string,int,hash<string>,eqstr> index(1000);
  vector<string> topologies;
  vector<int> count;

  string line;

  while(getline(cin,line)) {
    trees.push_back(line);
  }
  
  foreach(mytree,trees) {

    string t = topology(*mytree);

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

  /*************** Count how many of each type ***************/
  int total = 0;
  int most = 0;
  int withmost = 0;
  for(int i=0;i<count.size();i++) {
    if (count[i] > most) {
      most = count[i];
      withmost = i;
    }
    total += count[i];
  }

  std::cout<<"There were "<<total<<" trees scanned\n";
  std::cout<<"   Different topologies:  "<<topologies.size()<<std::endl;

  std::cout<<"\nMost frequent topology had "<<most<<" counts."<<std::endl;
  std::cout<<topologies[withmost]<<std::endl;

  for(int i=0;i<topologies.size();i++) {
    std::cout<<i<<"   "<<count[i]<<std::endl;
    std::cout<<"  "<<topologies[i]<<std::endl;
  }
  //put all the trees in a hash?

  //find which tree is the most common - print %

  //for each split, find which % of trees have it

  //all trees with the best topolgy, get the mean,stddef of branch lengths
}
