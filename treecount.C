#include <iostream>
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


SequenceTree standardized(const string& t) {
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

  //  std::cerr<<t<<std::endl;
  //  std::cerr<<T.write()<<std::endl;
  //  std::cerr<<T.write(false)<<std::endl;
  T.SequenceTree::standardize(newnames);
  //  std::cerr<<T.write(false)<<std::endl;
  return T;
}


string topology(const string& t) {
  SequenceTree T = standardized(t);
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

  //  for(int i=0;i<topologies.size();i++) {
  //    std::cout<<i<<"   "<<count[i]<<"   ("<<double(count[i])/total<<")"<<std::endl;
  //    std::cout<<"  "<<topologies[i]<<std::endl;
  //  }

  std::cout<<std::endl;
  std::cout<<std::endl;

  /*******  Check confidence of internal branches ******/
  SequenceTree best;
  foreach(mytree,trees) {
    if (topology(*mytree) == topologies[withmost]) {
      best = standardized(*mytree);
      break;
    }
  }

  std::cout<<best.write(false)<<std::endl;

  std::cout<<std::endl;
  std::cout<<std::endl;
  for(int b=best.leaves();b<best.branches();b++) {

    int count = 0;
    int parent = best.branch(b).parent();
    int child = best.branch(b).child();
    std::valarray<bool> p1 = best.partition(parent,child);

    for(int i=0;i<best.leaves();i++)
      if (p1[i])
	std::cout<<best.seq(i)<<" ";
    std::cout<<"| ";
    for(int i=0;i<best.leaves();i++)
      if (not p1[i])
	std::cout<<best.seq(i)<<" ";
    std::cout<<std::endl;

    foreach(mytree,trees) {
      SequenceTree thisone = standardized(*mytree);
      int match = -1;
      for(int b2=thisone.leaves();b2<thisone.branches();b2++) {
	int parent2 = thisone.branch(b2).parent();
	int child2 = thisone.branch(b2).child();

	std::valarray<bool> p2 = thisone.partition(parent2,child2);

	//	std::cerr<<b2<<" ";
	//	for(int i=0;i<p2.size();i++)
	//	  std::cerr<<p2[i]<<" ";
	//	std::cerr<<endl;


	if (p1[0] != p2[0])
	  p2 = !p2;

	assert(p1[0] == p2[0]);

	bool m = true;
	for(int i=0;i<thisone.leaves();i++)
	  if (p1[i] != p2[i])
	    m = false;

	if (m) {
	  match = b2;
	  break;
	}
      }
      if (match != -1)
	count++;
      if (b == 14)
	assert(match != -1);
    }  
    std::cout<<"  "<<count<<"     "<<double(count)/trees.size()<<std::endl;
  }

  /************ Get Branch Length Distribution ************/
  vector< vector<double> > branch_data(best.branches());

  foreach(mytree,trees) {
    if (topology(*mytree) != topologies[withmost]) continue;

    SequenceTree thisone = standardized(*mytree);

    for(int b=0;b<thisone.branches();b++)
      branch_data[b].push_back(thisone.branch(b).length());
  }

  for(int b=0;b< best.branches();b++) {
    double m1 = moment(branch_data[b],1);
    double m2 = moment(branch_data[b],2);
    double stddev = sqrt(m2 - m1*m1);

    best.branch(b).length() = m1;
    std::cout<<b<<"  "<<m1<<"    "<<stddev<<std::endl;
  }

  std::cout<<best<<endl;
}
