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

  /*
  int b2 = myrandom(0,T.branches());
  double p2 = Pr(residues,Theta,b2);

  if (std::abs((p2-p)/p) > 1.0e-7) {
    for(int i=0;i<T.leaves();i++)
      std::cerr<<Theta.get_alphabet().lookup(residues[i])<<" ";
    std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
    assert(0); //FIXME - try this check!
  }
  */

  assert(0.0 < p and p<= 1.0);
  //std::cerr<<" Pr: p="<<p<<"      log(p)="<<log(p)<<std::endl;
  return log(p);
}

double Pr(const alignment& A,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P) {
  double P = 0.0;
  
  vector<int> residues(A.size2());

  // Do each node before its parent
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
    P += Pr(residues,T,SModel,transition_P);
    //    std::cerr<<" substitution: P="<<P<<std::endl;
  }

  return P;
}


 
double Pr_star(const vector<int>& column,const tree& T,const ReversibleModel& SModel,
	       const vector<Matrix>& transition_P) {
  const alphabet& a = SModel.Alphabet();

  double sum=0;
  for(int lroot=0;lroot<a.size();lroot++) {
    double temp=SModel.frequencies()[lroot];
    for(int b=0;b<T.branches();b++) {
      const Matrix& Q = transition_P[b];

      int lleaf = column[b];
      if (a.letter(lleaf))
	temp *= Q(lleaf,lroot);
    }
    sum += temp;
  }
  return log(sum);
}

double Pr_star(const alignment& A,const tree& T,const ReversibleModel& SModel,
	       const vector<Matrix>& transition_P) {
  double Pr = 0.0;
  
  vector<int> residues(A.size2());

  // Do each node before its parent
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
    Pr += Pr_star(residues,T,SModel,transition_P);
    //    std::cerr<<" substitution: P="<<P<<std::endl;
  }

  return Pr;
}

double Pr_star_constant(const alignment& A,const tree& T1,const ReversibleModel& SModel,
			const vector<Matrix>& transition_P) {
  tree T2 = T1;

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
  for(int i=0;i<T2.leaves();i++)
    T2.branch(i).length() = ave/2.0;


  //----------- Get log L w/ new tree  -------------//
  return Pr_star(A,T2,SModel,transition_P);
}

double Pr_star_estimate(const alignment& A,const tree& T1,const ReversibleModel& SModel,
			const vector<Matrix>& transition_P) {
  tree T2 = T1;

  //----------- Get Distance Matrix --------------//
  Matrix D(T1.leaves(),T1.leaves());
  for(int i=0;i<T1.leaves();i++) 
    for(int j=0;j<T1.leaves();j++) 
      D(i,j) = T1.distance(i,j);


  //---- Set branch lengths to ave/2 per branch ----//
  for(int i=0;i<T2.leaves();i++) {
    double ave=0;
    for(int j=0;j<T2.leaves();j++) {
      if (i==j) continue;
      ave += log(D(i,j));
    }
    ave /= (T2.leaves()-1);
    T2.branch(i).length() = exp(ave)/2.0;
  }
  for(int i=T2.leaves();i<T2.branches();i++) {
    T2.branch(i).length() = 0;
  }

  //----------- Get log L w/ new tree  -------------//
  return Pr_star(A,T2,SModel,transition_P);
}

double Pr(const alignment& A, const Parameters& P) {
  const MultiRateModel& SModel = P.SModel();
  const tree& T = P.T;

  double sum = 0;
  for(int r=0;r<SModel.nrates();r++)
    sum += Pr(A,T,SModel.BaseModel(),P.transition_P(r))*SModel.distribution()[r];

  return sum;
}

double Pr_star(const alignment& A, const Parameters& P) {
  const MultiRateModel& SModel = P.SModel();
  const tree& T = P.T;

  double sum = 0;
  for(int r=0;r<SModel.nrates();r++)
    sum += Pr_star(A,T,SModel.BaseModel(),P.transition_P(r))*SModel.distribution()[r];

  return sum;
}


double Pr_star_constant(const alignment& A, const Parameters& P) {
  const MultiRateModel& SModel = P.SModel();
  const tree& T = P.T;

  double sum = 0;
  for(int r=0;r<SModel.nrates();r++)
    sum += Pr_star_constant(A,T,SModel.BaseModel(),P.transition_P(r))*SModel.distribution()[r];

  return sum;
}


double Pr_star_estimate(const alignment& A, const Parameters& P) {
  const MultiRateModel& SModel = P.SModel();
  const tree& T = P.T;

  double sum = 0;
  for(int r=0;r<SModel.nrates();r++)
    sum += Pr_star_estimate(A,T,SModel.BaseModel(),P.transition_P(r))*SModel.distribution()[r];

  return sum;
}




}
