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

// 1. make an inner 'peel' that <distributions> as a parameter
//   a. this contains the info from 'residues' and 'group'
// 2. alter 'peel' to go over the whole tree
//   a. to do half the tree, only send in half the info

// (dist.size()==0) does not mean "gap", but means no info so far.

// If we are going to return distributions(root), then we must make sure
//  that there is no information there from the WRONG side of the tree


// things to do:  (look at Sampler Todo.sxw)
// 1. Make the distributions(i) never change size - use a vector<int> or vector<bool>
//    to store whether or not things have information.
// 2. Then separate out part of the routine as an inline function, to which 'distributions'
//    is passed.   (change 'distributions' into a matrix?)
// 3. Make the 'branches' array not depend on 'group'...
//    In the common case (doing the full tree) this might be a speedup...

  /// Actually propogate info along branches
												       
												       
  inline void peel(const vector<int>& branches, valarray<bool>& used,
		   Matrix& distributions,
		   const vector<int>& residues, const tree& T, 
		   const vector<Matrix>& transition_P,int root) 
  {
    const int asize = distributions.size2();

    for(int i=0;i<branches.size();i++) {
      int b = branches[i];
      int child = T.branch(b).child();
      int parent = T.branch(b).parent();
      
      // Are we going up or down the branch?
      if (T.ancestor(child,root))
	std::swap(child,parent);

      // Skip if no info going out
      if (not used[child]) continue;

      // Propogate info along branch
      const Matrix& Q = transition_P[b];
      if (alphabet::letter(residues[child])) 
	for(int i=0;i<asize;i++)
	  distributions(parent,i) *= Q(i,residues[child])*distributions(child,residues[child]);
      else {
	for(int i=0;i<asize;i++) {
	  double temp=0;
	  for(int j=0;j<asize;j++)
	    temp += Q(i,j)*distributions(child,j);
	  distributions(parent,i) *= temp;
	}
      }

      // Update info at parent
      used[parent] = true;
    }
  }

  //  inline vector<int> get_branches(const tree& T, int root) __attribute((always_inline))

  /// Compute an ordered list of branches to process
  inline vector<int> get_branches(const tree& T, int root) {

    //FIXME - walk up the tree from peeling 'root' to the tree root, 
    // instead of computing branches2 and using 'reverse'
    vector<int> branches1;
    branches1.reserve(T.num_nodes());
    vector<int> branches2;
    branches2.reserve(T.num_nodes());

    for(int i=0;i<T.num_nodes()-1;i++) {
      int child = T.get_nth(i);

      // don't propogate from the root, or non-members
      if (child == root) continue;

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
  
    return branches1;
  }


  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'

  /*!
    \param residues The letters/gaps/non-gaps at each node
    \param T The tree
    \param SModel The substitution Model
    \param transition_P The transition matrices for each branch
    \param root The node at which we are assessing the probabilities
    \param group The nodes from which to consider info
   */
valarray<double> peel(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
		      const vector<Matrix>& transition_P,int root, const valarray<bool>& group) {
  const alphabet& a = SModel.Alphabet();

  /******* Put the info from the letters into the distribution *******/
  bool any_letters = false;

  valarray<bool> used(false,T.num_nodes()-1);
  Matrix distributions(T.num_nodes()-1,a.size());

  for(int i=0;i<T.num_nodes()-1;i++) {
    if (alphabet::letter(residues[i]) and group[i]) {
      //are there ANY letters at all?
      any_letters = true;

      used[i] = true;

      for(int j=0;j<a.size();j++)
      	distributions(i,j) = 0;
      distributions(i,residues[i]) = 1;
    }
    // If we always replaced unused nodes instead of multiplying, we could remove this
    else {
      for(int j=0;j<a.size();j++)
      	distributions(i,j) = 1;
    }
  }

  vector<int> branches = get_branches(T,root);

  peel(branches,used,distributions,residues,T,transition_P,root);

  //This can only happen if none of our nodes has info
  assert(used[root] or not any_letters);

  /*----------- return the result ------------------*/
  //FIXME - filling this valarray is costing us 2% of the CPU time!
  valarray<double> result(a.size());
  for(int i=0;i<a.size();i++)
    result[i] = distributions(root,i);
  return result;
}

valarray<double> peel(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
		      const vector<Matrix>& transition_P,int b,bool up) {
  /**************** Find our branch, and orientation *****************/
  int root = T.branch(b).parent();      //this is an arbitrary choice

  int node1 = T.branch(b).child();
  int node2 = T.branch(b).parent();
  if (not up) std::swap(node1,node2);

  valarray<bool> group = T.partition(node1,node2);

  return peel(residues,T,SModel,transition_P,root,group); 
}


double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P,int root) {
  assert(residues.size() == T.num_nodes()-1);

  valarray<double> rootD = peel(residues,T,SModel,transition_P,
				root,valarray<bool>(true,T.num_nodes())
				);

  rootD *= SModel.frequencies();

  return rootD.sum();
}

double Pr(const vector<int>& residues,const tree& T,const ReversibleModel& SModel,
	  const vector<Matrix>& transition_P) {

  int root = T.get_nth(T.num_nodes()-2);
  double p = Pr(residues,T,SModel,transition_P,root);

#ifndef NDEBUG  
  int node = myrandom(0,T.num_nodes()-1);
  double p2 = Pr(residues,T,SModel,transition_P,node);

  if (std::abs(p2-p) > 1.0e-9) {
    for(int i=0;i<T.leaves();i++)
      std::cerr<<SModel.Alphabet().lookup(residues[i])<<" ";
    std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
    assert(0); //FIXME - try this check!
  }
#endif

  // we don't get too close to zero, normally
  assert(0 <= p and p <= 1.00000000001);

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

  // we don't get too close to zero, normally
  assert(0 < total and total <= 1.00000000001);

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
    for(int b=0;b<T.leaves();b++) {
      const Matrix& Q = transition_P[b];

      int lleaf = column[b];
      if (a.letter(lleaf))
	temp *= Q(lleaf,lroot);
    }
    p += temp;
  }

  // we don't get too close to zero, normally
  assert(0 <= p and p <= 1.00000000001);

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

    // we don't get too close to zero, normally
    assert(0 < total and total <= 1.00000000001);

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
