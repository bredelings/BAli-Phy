#include "substitution.H"

#include <cmath>
#include <valarray>
using std::valarray;


// Compute the contribution of one child node to this node
valarray<double> peel_branch(const alphabet& a,int residue,const tree& T,
			     const valarray<double>& prior) {
  valarray<double> dist(a.size());

  if (residue != -2) {
    if (residue == alphabet::gap) 
      dist = 1.0; // doesn't sum to 1, but has no effect
    else
      for(int i=0;i<a.size();i++)  // store matrix per-branch?
	dist[i] = a.substitution(i,residue);
  } 
  else {
    dist = 0.0;
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++) 
	dist[i] += a.substitution(i,j)*prior[j];
//	std::cerr<<i<<"  "<<j<<"  "<<dist[i]<<"  "<<prior[j]<<endl;
  }

  return dist;
}


void peel(const alphabet& a,const vector<int>& residues,const tree& T,
	  int i,TreeFunc< valarray<double> >& distributions) {
  distributions(i).resize(a.size());

  valarray<double> left(a.size());
  valarray<double> right(a.size());

  int residue = -2;

  if (!T[i].left) {
    int child = T[i].right->name;
    if (child<T.leaves())
      residue = residues[child];
    distributions(i) = peel_branch(a,residue,T,distributions(child));
  }
  else if (!T[i].right) {
    int child = T[i].left->name;
    if (child<T.leaves())
      residue = residues[child];
    distributions(i) = peel_branch(a,residue,T,distributions(child));
  }
  else if (T[i].left and T[i].right) {
    int lchild = T[i].left->name;
    if (lchild<T.leaves())
      residue = residues[lchild];
    left =  peel_branch(a,residue,T,distributions(lchild));
    
    residue = -2;
    int rchild = T[i].right->name;
    if (rchild<T.leaves())
      residue = residues[rchild];
    right = peel_branch(a,residue,T,distributions(rchild));

    distributions(i) = left * right;
  }
  else 
    assert(0);
}


valarray<double> peel(const alphabet& a, const vector<int>& residues,const tree& T,
		      int node1, int node2) {
  
  TreeFunc< valarray<double> > distributions(T);

  valarray<bool> group = T.partition(node1,node2);
  group[node2] = false;

  // what nodes to we need to work in the second pass
  vector<int> work;
  work.reserve(T.num_nodes());

  // declare a temporary for use in the loop
  valarray<double> dist(a.size());

  // peel up as far as possible
  for(int n=0;n<T.num_nodes()-1;n++) {
    if (!group[n] or (!distributions(n).size() and n>=T.leaves()) ) continue;
    
    if (T.ancestor(n,node2)) {
      work.push_back(n);
      continue;
    }

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

  // peel down to node2
  for(int i=work.size()-1;i>0;i--) {
    int n = work[i];
    if (!distributions(n).size()) continue;

    int parent = T[n].left->name;
    if (T.ancestor(parent,node2))
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

  if (!distributions(node2).size()) {
    dist = 1.0;
    return dist;
    //assert(distributions(node2).size());
  }

  int root = std::max(node1,node2);
  if (root != node2) {
    distributions(node1).resize(a.size());
    distributions(node1) = 0.0;
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	distributions(node1)[i] += a.substitution(i,j)*distributions(node2)[j];
  }
  return distributions(root);
}

double substitution(const alphabet& a,const vector<int>& residues,const tree& T,
		    TreeFunc< valarray<double> >& distributions) {
  assert(residues.size() == T.leaves());

  // Propagate info up from leaves
  for(int i=T.leaves();i<T.num_nodes();i++) 
    peel(a,residues,T,i,distributions);
  
  // Sum over probabilities at the root node
  const valarray<double>& frequency = a.frequency;
  valarray<double> dist = distributions(T[T.num_nodes()-1]);

  dist *= frequency;
  
  double p = dist.sum();

  double p2 = 1; 
  {
    int root = T.num_nodes()-1;
    int left = T[root].left->name;
    int right = T[root].right->name;
    if (right> left) {
      
      valarray<double> leftD = peel(a,residues,T,right,left);
      valarray<double> rightD = peel(a,residues,T,left,right);
      valarray<double> rootD(0.0,a.size());
      
      // if  right> left
      for(int i=0;i<a.size();i++)
	for(int j=0;j<a.size();j++)
	  rootD[i] += a.substitution(i,j)*rightD[j];
      
      rootD *= leftD;
      rootD *= frequency;
      p2 = rootD.sum();

      for(int i=0;i<residues.size();i++)
	std::cerr<<a.lookup(residues[i])<<" ";
      std::cerr<<p<<" "<<log(p)<<"    "<<p2<<"   "<<log(p2)<<endl;
    }
  }


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


 
