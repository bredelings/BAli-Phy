#include "tree.H"
#include "rng.H"

vector<int> permutation(int n) {
  vector<int> p;
  if (n==1)
    p.push_back(0);
  else {
    p = permutation(n-1);
    int i = myrandom(n);
    p.insert(p.begin()+i,n);
  }
  return p;
}

//FIXME - structure isn't random!
SequenceTree RandomTree(const vector<string>& s,double branch_mean) {
  vector<SequenceTree> trees;
  vector<int> pi = permutation(s.size());
  for(int i=0;i<s.size();i++)
    trees.push_back(SequenceTree(s[pi[i]]));

  while(trees.size() > 1) {
    SequenceTree T1 = trees.back();trees.pop_back();
    SequenceTree T2 = trees.back();trees.pop_back();
    SequenceTree T3 = (T1+T2);
    trees.push_back(T3);
  }

  for(int i=0;i<trees[0].branches();i++) 
    trees[0].branch(i).length() = exponential(branch_mean);

  return trees[0];
}
