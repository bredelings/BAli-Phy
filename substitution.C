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
namespace substitution {

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

// If we are going to return distributions(root), then we must make sure
//  that there is no information there from the WRONG side of the tree

valarray<double> peel(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
		      const vector<Matrix>& transition_P,int node1, int node2, int root) {
  const alphabet& a = SModel.Alphabet();

  /**************** Find our branch, and orientation *****************/
  assert(root == node1 or root == node2);
  valarray<bool> group = T.partition(node1,node2);

  /******* Put the info from the letters into the distribution *******/
  bool any_letters = false;
  TreeFunc< valarray<double> > distributions(T);  //doing a lot of work here
  for(int i=0;i<residues.size();i++) {
    // we DO use distributions() for wrong side of tree, so can't put 
    // info about letters from wrong side into them

    if (alphabet::letter(residues[i]) and group[i]) {
      distributions(i).resize(a.size(),0.0);
      distributions(i)[residues[i]] = 1.0;

      //are there ANY letters at all?
      any_letters = true;
    }
  }

  /******** Compute the ordered list of branches to process *********/
  vector<int> branches1;
  branches1.reserve(T.num_nodes());
  vector<int> branches2;
  branches2.reserve(T.num_nodes());

  for(int i=0;i<T.num_nodes()-1;i++) {
    int child = T.get_nth(i);

    // don't propogate from the root, or non-members
    if (not group[child] or child == root) continue;

    // if we are above the root, then propagate down to it
    if (child != root and T.ancestor(child,root)) {
      int parent = T[child].left();
      if (not T.ancestor(parent,root))
	parent = T[child].right();
      int b = T.branch_up(parent);

      branches2.push_back(b);
    }
    // otherwise propagate up to a node that is >= the root
    else {
      int b = T.branch_up(child);
      branches1.push_back(b);
    }
  }
  std::reverse(branches2.begin(),branches2.end());
  branches1.insert(branches1.end(),branches2.begin(),branches2.end());
  if (branches2.size()) assert(T.ancestor(node2,node1));

  /**************** Propogate info along branches ******************/
  valarray<double> dist(a.size());   // declare a temporary for use in the loop.
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
    const Matrix& Q = transition_P[b];
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

valarray<double> peel(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
		      const vector<Matrix>& transition_P,int b,bool up) {
  /**************** Find our branch, and orientation *****************/
  int root = T.branch(b).parent();      //this is an arbitrary choice

  int node1 = T.branch(b).child();
  int node2 = T.branch(b).parent();
  if (not up) std::swap(node1,node2);

  return peel(residues,T,SModel,transition_P,node1,node2,root);
}

double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P,int b) {
  assert(residues.size() == T.num_nodes()-1);

  valarray<double> leftD  = peel(residues,T,SModel,transition_P,b,false);
  valarray<double> rightD = peel(residues,T,SModel,transition_P,b,true);
  
  valarray<double> rootD = leftD * SModel.frequencies() * rightD;

  double p = rootD.sum();

  return p;
}

double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P) {

  int root = T.get_nth(T.num_nodes()-2);
  root = T.branch_up(root).parent();

  int b = T.branch_up(root);
  double p = Pr(residues,T,SModel,transition_P,b);

  int b2 = myrandom(0,T.branches());
  double p2 = Pr(residues,T,SModel,transition_P,b2);

  if (std::abs(p2-p) > 1.0e-9) {
    for(int i=0;i<T.leaves();i++)
      std::cerr<<SModel.Alphabet().lookup(residues[i])<<" ";
    std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
    assert(0); //FIXME - try this check!
  }

  //  std::cerr<<" Pr: p="<<p<<"      log(p)="<<log(p)<<std::endl;
  assert(0.0 <= p and p <= 1.0);
  return p;
}

double Pr(const alignment& A,const Parameters& P,int column) {
  const MultiRateModel& MRModel = P.SModel();

  vector<int> residues(A.size2());
  for(int i=0;i<residues.size();i++)
    residues[i] = A(column,i);
    
  
  double total=0;
  for(int r=0;r<MRModel.nrates();r++) 
    total += MRModel.distribution()[r] * Pr(residues,
					    P.T,
					    MRModel.BaseModel(),
					    P.transition_P(r)
					    );
  assert(0 < total and total <= 1);
  return log(total);
}

double Pr(const alignment& A,const Parameters& P) {
  double p = 0.0;

  // Do each node before its parent
  for(int column=0;column<A.length();column++) 
    p += Pr(A,P,column);

    //    std::cerr<<" substitution: P="<<P<<std::endl;
  return p;
}

double Pr_star(const vector<int>& column,const tree& T,const ReversibleModel& SModel,
	       const vector<Matrix>& transition_P) {
  const alphabet& a = SModel.Alphabet();

  double p=0;
  for(int lroot=0;lroot<a.size();lroot++) {
    double temp=SModel.frequencies()[lroot];
    for(int b=0;b<T.branches();b++) {
      const Matrix& Q = transition_P[b];

      int lleaf = column[b];
      if (a.letter(lleaf))
	temp *= Q(lleaf,lroot);
    }
    p += temp;
  }
  return p;
}

double Pr_star(const alignment& A,const Parameters& P) {

  double p = 0.0;
  const MultiRateModel& MRModel = P.SModel();
  
  vector<int> residues(A.size2());

  // Do each node before its parent
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);

    double total=0;
    for(int r=0;r<MRModel.nrates();r++)
      total += MRModel.distribution()[r] * Pr_star(residues,
						   P.T,
						   MRModel.BaseModel(),
						   P. transition_P(r)
						   );

    assert(0.0 < total and total <= 1.0);
    p += log(total);
  }

  return p;
}

double Pr_star_constant(const alignment& A,const Parameters& P) {
  const tree& T1 = P.T;
  Parameters P2 = P;

  //----------- Get Distance Matrix --------------//
  Matrix D(T1.leaves(),T1.leaves());
  for(int i=0;i<T1.leaves();i++) 
    for(int j=0;j<T1.leaves();j++) 
      D(i,j) = T1.distance(i,j);

  //----------- Get Average Distance -------------//
  double sum=0;
  for(int i=0;i<T1.leaves();i++) 
    for(int j=0;j<i;j++) 
      sum += D(i,j);
  const int n = (T1.leaves()*(T1.leaves()+1))/2;
  double ave = sum/n;

  //-------- Set branch lengths to ave/2  ----------//
  for(int b=0;b<T1.leaves();b++)
    P2.setlength(b,ave/2.0);


  //----------- Get log L w/ new tree  -------------//
  return Pr_star(A,P2);
}

double Pr_star_estimate(const alignment& A,const Parameters& P) {
  const tree& T1 = P.T;
  Parameters P2 = P;

  //----------- Get Distance Matrix --------------//
  Matrix D(T1.leaves(),T1.leaves());
  for(int i=0;i<T1.leaves();i++) 
    for(int j=0;j<T1.leaves();j++) 
      D(i,j) = T1.distance(i,j);


  //---- Set branch lengths to ave/2 per branch ----//
  for(int i=0;i<T1.leaves();i++) {
    double ave=0;
    for(int j=0;j<T1.leaves();j++) {
      if (i==j) continue;
      ave += log(D(i,j));
    }
    ave /= (T1.leaves()-1);
    P2.setlength(i,exp(ave)/2.0);
  }

  //----------- Get log L w/ new tree  -------------//
  return Pr_star(A,P2);
}

}
