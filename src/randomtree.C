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

Tree remove_root_branch(RootedTree RT) {
  nodeview r1 = RT.root();
  assert(r1.neighbors().size() == 1);
  nodeview r2 = *(RT.root().neighbors());
  RT.reroot(0);

  r2 = RT.prune_subtree(RT.directed_branch(r2,r1));
  if (not r2.is_leaf_node())
    RT.remove_node_from_branch(r2);

  return Tree(RT);
}

Tree RandomTree(int n,double branch_mean) 
{
  // construct the base root tree
  RootedTree singleton;
  singleton.add_first_node();
  {
    int node = singleton.add_node(singleton.root());
    singleton.reroot(node);
  }
  
  vector<RootedTree> trees(n,singleton);
  

  while(trees.size() > 1) {
    int i1 = myrandom(trees.size());
    RootedTree T1 = trees[i1];trees.erase(trees.begin()+i1);

    int i2 = myrandom(trees.size());
    RootedTree T2 = trees[i2];

    RootedTree T3(T1,T2);
    int new_root = T3.add_node(T3.root());
    T3.reroot(new_root);

    trees[i2] = T3;
  }

  Tree T = remove_root_branch(trees[0]);

  for(int i=0;i<T.n_branches();i++) 
    T.branch(i).set_length( exponential(branch_mean) );

  return T;
}

SequenceTree RandomTree(const vector<string>& s,double branch_mean) {
  return SequenceTree(RandomTree(s.size(),branch_mean),s);
}
