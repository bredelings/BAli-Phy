#include "substitution.H"

#include <cmath>
#include <valarray>
using std::valarray;


valarray<double> peel(const alphabet& a, const vector<int>& residues,const tree& T,
		      int node1, int node2) {
  
  // we are spending too much work creating AND destroying this thing
  TreeFunc< valarray<double> > distributions(T);

  valarray<bool> group = T.partition(node1,node2);
  if (node1 != T.parent(node2)) group[node2] = false; // don't propagate from node2

  // what nodes to we need to work in the second pass
  vector<int> work;
  work.reserve(T.num_nodes());

  // declare a temporary for use in the loop
  valarray<double> dist(a.size());

  // peel up as far as possible
  for(int n=0;n<T.num_nodes()-1;n++) {
    if (!group[n]) continue;
    
    if (node1 != T.parent(node2) and T.ancestor(n,node2)) {
      work.push_back(n);
      continue;
    }
    else if (!distributions(n).size() and n>=T.leaves())
      continue;

    if (n<T.leaves()) {
      if (residues[n] == alphabet::gap)
	continue;
      for(int i=0;i<a.size();i++)
	dist[i] = a.substitution(i,residues[n]);
    }
    else {
      dist = 0.0;
      for(int i=0;i<a.size();i++)
	for(int j=0;j<a.size();j++)
	  dist[i] += a.substitution(i,j)*distributions(n)[j];
    }

    int parent = T.parent(n);
    if (!distributions(parent).size()) {
      distributions(parent).resize(a.size());
      distributions(parent) = dist;
    }
    else 
      distributions(parent) *= dist;
  }
  
  if (work.size())
    assert(node1 != T.parent(node2));

  // peel down to node2
  for(int i=work.size()-1;i>=0;i--) {
    int n = work[i];
    if (!distributions(n).size()) continue;

    int parent = T[n].left->name;
    if (!T.ancestor(parent,node2))
      parent = T[n].right->name;

    dist = 0.0;
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	dist[i] += a.substitution(i,j)*distributions(n)[j];
    
    if (!distributions(parent).size()) {
      distributions(parent).resize(a.size());
      distributions(parent) = dist;
    }
    else 
      distributions(parent) *= dist;
  }

  int root = std::max(node1,node2);

  /*
  std::cerr<<"nodes: "<<node1<<" "<<node2<<endl;
  for(int i=0;i<residues.size();i++)
    if (group[i])
      std::cerr<<a.lookup(residues[i])<<" ";
    else
      std::cerr<<"* ";
  std::cerr<<distributions(root).sum()<<" "<<log(distributions(root).sum())<<endl;
  */

  if (!distributions(root).size()) {
    bool allgaps=true;
    for(int i=0;i<residues.size();i++)
      if (group[i] && residues[i] != alphabet::gap)
	allgaps=false;
    assert(allgaps);
    dist = 1.0;
    return dist;
    //assert(distributions(node2).size());
  }

  assert(distributions(root).size());

  return distributions(root);
}

double substitution(const alphabet& a,const vector<int>& residues,const tree& T,
		    TreeFunc< valarray<double> >& distributions) {
  assert(residues.size() == T.leaves());

  int root = T.num_nodes()-1;
  int left = T[root].left->name;
  int right = T[root].right->name;
      
  valarray<double> leftD = peel(a,residues,T,right,left);
  valarray<double> rightD = peel(a,residues,T,left,right);
  
  valarray<double> rootD = leftD * a.frequency * rightD;

  double p = rootD.sum();

  /*
  left = 13;
  right = T.parent(left);
  leftD = peel(a,residues,T,right,left);
  rightD = peel(a,residues,T,left,right);
  rootD = leftD * a.frequency * rightD;
  double p2 = rootD.sum();

  for(int i=0;i<residues.size();i++)
    std::cerr<<a.lookup(residues[i])<<" ";
  std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
  */
  assert(p<= 1.0);
  return log(p);
}

double substitution(const alignment& A,const tree& T) {
  double P = 0.0;
  
  TreeFunc< valarray<double> > distributions(T);

  vector<int> residues(A.num_sequences());

  // Do each node before its parent
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
    P += substitution(A.get_alphabet(),residues,T,distributions);
  }

  return P;
}


 
