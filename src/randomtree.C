#include "sequencetree.H"
#include "rng.H"

using std::vector;
using std::string;

vector<int> permutation(int n) {
  vector<int> p;
  if (n==1)
    p.push_back(0);
  else {
    p = permutation(n-1);
    int i = myrandom(n);
    p.insert(p.begin()+i,n-1);
  }
  return p;
}

SequenceTree RandomTree(const vector<string>& s,double branch_mean) {
  vector<RootedSequenceTree> trees;
  for(int i=0;i<s.size();i++) {
    RootedSequenceTree RT(s[i]);
    int node = RT.add_node(RT.root());
    RT.reroot(node);
    trees.push_back(RT);
  }

  while(trees.size() > 1) {
    int i1 = myrandom(trees.size());
    RootedSequenceTree T1 = trees[i1];trees.erase(trees.begin()+i1);

    int i2 = myrandom(trees.size());
    RootedSequenceTree T2 = trees[i2];

    trees[i2] = (T1+T2);
  }

  SequenceTree ST = remove_root_branch(trees[0]);

  for(int i=0;i<ST.n_branches();i++) 
    ST.branch(i).set_length( exponential(branch_mean) );

  return ST;
}
