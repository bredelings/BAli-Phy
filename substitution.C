#include "substitution.H"
#include "rng.H"
#include <cmath>
#include <valarray>
using std::valarray;

// This file assumes that 
// * the matrix is reversible.  This means that we evaluate
//   frequencies at the root - even for insertions, where they actually
//   apply somewhere down the tree.
//
// * we don't need to work in log space for a single column
//
// * 

inline valarray<double> peel(int letter,const Matrix& P,const valarray<double>& dist1) {
  valarray<double> dist2(0.0,P.size1());
  
  //This is a speedup in this case
  if (alphabet::letter(letter)) {
    for(int i=0;i<dist2.size();i++)
      dist2[i] = P(i,letter)*dist1[letter];
  }
  // But this still works in all cases
  else {
    for(int i=0;i<dist2.size();i++)
      for(int j=0;j<dist2.size();j++)
	dist2[i] += P(i,j)*dist1[j];
  }

  return dist2;
}


// (dist.size()==0) does not mean "gap", but means no info so far.

valarray<double> peel(const vector<int>& residues,const Parameters& Theta,
		      int b,bool up) {
  const alphabet& a = Theta.get_alphabet();
  const tree& T = Theta.T;

  /**************** Find our branch, and orientation *****************/
  int root = T.branch(b).parent();      //this is an arbitrary choice

  int node1 = T.branch(b).child();
  int node2 = T.branch(b).parent();
  if (not up) std::swap(node1,node2);

  valarray<bool> group = T.partition(node1,node2);

  /******* Put the info from the letters into the distribution *******/
  bool any_letters = false;
  TreeFunc< valarray<double> > distributions(T);  //doing a lot of work here
  for(int i=0;i<residues.size();i++) {
    if (alphabet::letter(residues[i])) {
      distributions(i).resize(a.size(),0.0);
      distributions(i)[residues[i]] = 1.0;

      //are there ANY letters at all?
      if (group[i]) any_letters = true;
    }
  }

  /******** Compute the ordered list of branches to process *********/
  vector<int> branches1;
  branches1.reserve(T.num_nodes());
  vector<int> branches2;
  branches2.reserve(T.num_nodes());

  for(int i=0;i<T.num_nodes()-1;i++) {
    int child = T.get_nth(i);
    if (not group[child] or child == root) continue;

    if (child != node2 and T.ancestor(child,node2)) {
      int parent = T[child].left();
      if (not T.ancestor(parent,node2))
	parent = T[child].right();
      int b = T.branch_up(parent);

      branches2.push_back(b);
    }
    else {
      int b = T.branch_up(child);
      branches1.push_back(b);
    }
  }
  std::reverse(branches2.begin(),branches2.end());
  branches1.insert(branches1.end(),branches2.begin(),branches2.end());
  if (branches2.size()) assert(up);

  /**************** Propogate info along branches ******************/
  valarray<double> dist(a.size());   // declare a temporary for use in the loop
                                     // profile this w/ modern
                                     // compiler and with code in
                                     // separate routine
  

  for(int i=0;i<branches1.size();i++) {
    int b = branches1[i];
    int child = T.branch(b).child();
    int parent = T.branch(b).parent();

    // Are we going up or down the branch?
    if (T.ancestor(child,root))
      std::swap(child,parent);

    // Skip if no info going out
    if (not distributions(child).size()) continue;

    // Propogate info along branch
    const Matrix& Q = Theta.substitution(b);
    dist = peel(residues[child],Q,distributions(child));

    // Update info at parent
    if (not distributions(parent).size()) {
      distributions(parent).resize(a.size());
      distributions(parent) = dist;
    }
    else 
      distributions(parent) *= dist;
  }

  /***************** If no info, return vector(1.0) ***************/
  if (not distributions(root).size()) {
    distributions(root).resize(a.size());
    distributions(root) = 1.0;

    //This can only happen if none of our nodes has info
    assert(not any_letters);
  }

  /*
  std::cerr<<"nodes: "<<node1<<" "<<node2<<endl;
  for(int i=0;i<residues.size();i++)
    if (group[i])
      std::cerr<<a.lookup(residues[i])<<" ";
    else
      std::cerr<<". ";
  std::cerr<<distributions(root).sum()<<" "<<log(distributions(root).sum())<<endl;

  if (not up)
    assert(node1 == T.branch(b).parent() and node2 == T.branch(b).child());
  else
    assert(node2 == T.branch(b).parent() and node1 == T.branch(b).child());
  */

  return distributions(root);
}

double substitution(const vector<int>& residues,const Parameters& Theta,int b) {
  const tree& T = Theta.T;
  assert(residues.size() == T.num_nodes()-1);

  valarray<double> leftD = peel(residues,Theta,b,false);
  valarray<double> rightD = peel(residues,Theta,b,true);
  
  valarray<double> rootD = leftD * Theta.frequencies() * rightD;

  double p = rootD.sum();

  return p;
}

double substitution(const vector<int>& residues,const Parameters& Theta) {
  const tree& T = Theta.T;

  int root = T.get_nth(T.num_nodes()-2);
  root = T.branch_up(root).parent();

  int b = T.branch_up(root);
  double p = substitution(residues,Theta,b);

  /*
  int b2 = myrandom(0,T.branches());
  double p2 = substitution(residues,Theta,b2);

  if (std::abs((p2-p)/p) > 1.0e-7) {
    for(int i=0;i<T.leaves();i++)
      std::cerr<<Theta.get_alphabet().lookup(residues[i])<<" ";
    std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
    assert(0); //FIXME - try this check!
  }

  */
  assert(0.0 < p and p<= 1.0);
  //  std::cerr<<" substitution: p="<<p<<"      log(p)="<<log(p)<<std::endl;
  return log(p);
}

double substitution(const alignment& A,const Parameters& Theta) {
  double P = 0.0;
  
  vector<int> residues(A.size2());

  // Do each node before its parent
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
    P += substitution(residues,Theta);
    //    std::cerr<<" substitution: P="<<P<<std::endl;
  }

  return P;
}


 
