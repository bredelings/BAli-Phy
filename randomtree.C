#include "tree.H"
#include "rng.H"

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
  vector<SequenceTree> trees;
  for(int i=0;i<s.size();i++)
    trees.push_back(SequenceTree(s[i]));

  while(trees.size() > 1) {
    int i1 = myrandom(trees.size());
    SequenceTree T1 = trees[i1];trees.erase(trees.begin()+i1);

    int i2 = myrandom(trees.size());
    SequenceTree T2 = trees[i2];

    trees[i2] = (T1+T2);
  }

  for(int i=0;i<trees[0].branches();i++) 
    trees[0].branch(i).length() = exponential(branch_mean);

  return trees[0];
}
