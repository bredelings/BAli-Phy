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

  nodeview r1 = trees[0].root();
  nodeview r2 = *(trees[0].root().neighbors());
  trees[0].reroot(0);

  r2 = trees[0].prune_subtree(trees[0].directed_branch(r2,r1));
  trees[0].remove_node_from_branch(r2);

  for(int i=0;i<trees[0].n_branches();i++) 
    trees[0].branch(i).length() = exponential(branch_mean);

  return SequenceTree(trees[0]);
}
