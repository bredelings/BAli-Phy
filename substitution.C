#include "substitution.H"
#include "myrandom.H"
#include <cmath>
#include <valarray>
using std::valarray;

//FIXME - should be some way to avoid duplication matrix multiply?
//how to deal with directionality of top edge?  sometime peel down,
// sometimes peel up
valarray<double> peel(const vector<int>& residues,const Parameters& Theta,
		      int node1, int node2) {
  const alphabet& a = Theta.get_alphabet();
  const tree& T = Theta.T;
  
  // we are spending too much work creating AND destroying this thing
  TreeFunc< valarray<double> > distributions(T);

  valarray<bool> group = T.partition(node1,node2);
  if (node1 != T.parent(node2)) group[node2] = false; // don't propagate from node2
  assert(0);//FIXME - the above test doesn't work for top branch

  // what nodes do we need to work in the second pass
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

    int b = T.branch_up(n);
    const Matrix& Q = Theta.substitution(b);

    if (T[n].leaf()) {
      if (residues[n] == alphabet::gap)
	continue;
      for(int i=0;i<a.size();i++)
	dist[i] = Q(i,residues[n]);
    }
    else {
      dist = 0.0;
      for(int i=0;i<a.size();i++)
	for(int j=0;j<a.size();j++)
	  dist[i] += Q(i,j)*distributions(n)[j];
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

    int parent = T[n].left();
    if (!T.ancestor(parent,node2))
      parent = T[n].right();

    int b = T.branch_up(parent);
    const Matrix& Q = Theta.substitution(b);
    dist = 0.0;
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	dist[i] += Q(i,j)*distributions(n)[j];
    
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

double substitution(const vector<int>& residues,const Parameters& Theta,
		    TreeFunc< valarray<double> >& distributions) {
  const tree& T = Theta.T;
  assert(residues.size() == T.leaves());

  int root = T.num_nodes()-1;
  int left = T[root].left();
  int right = T[root].right();
      
  valarray<double> leftD = peel(residues,Theta,right,left);
  valarray<double> rightD = peel(residues,Theta,left,right);
  
  valarray<double> rootD = leftD * Theta.frequencies() * rightD;

  double p = rootD.sum();


  /*
  left = myrandom(0,T.num_nodes()-2);
  right = T.parent(left);
  leftD = peel(residues,Theta,right,left);
  rightD = peel(residues,Theta,left,right);
  rootD = leftD * Theta.frequency * rightD;
  double p2 = rootD.sum();

  for(int i=0;i<residues.size();i++)
    std::cerr<<Theta.get_alphabet().lookup(residues[i])<<" ";
  std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
  */

  assert(p<= 1.0);
  return log(p);
}

double substitution(const alignment& A,const Parameters& Theta) {
  double P = 0.0;
  
  TreeFunc< valarray<double> > distributions(Theta.T);

  vector<int> residues(A.num_sequences());

  // Do each node before its parent
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
    P += substitution(residues,Theta,distributions);
  }

  return P;
}


 
