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
    // if (branches2.size()) {}
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
      std::abort(); //FIXME - try this check!
    }
#endif

    // we don't get too close to zero, normally
    assert(0 <= p and p <= 1.00000000001);

    return p;
  }


  double Pr(const alignment& A, const tree& T, const MultiModel& MModel, const MatCache& MC,int column) {
    
    vector<int> residues(A.size2());
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
  
    double total=0;
    for(int m=0;m<MModel.n_base_models();m++) {
      double p=0;
      p = Pr(residues,
	     T,
	     MModel.base_model(m),
	     MC.transition_P(m)
	     );
      std::cerr<<"    p("<<m<<") = "<<p;
      p *= MModel.distribution()[m];
      std::cerr<<"    f(m)*p("<<m<<") = "<<p<<std::endl;

      total += p;
    }

    // we don't get too close to zero, normally
    assert(0 < total and total <= 1.00000000001);
    
    return log(total);
  }

  double Pr(const alignment& A, const tree& T, const MultiModel& MModel, const MatCache& SM) {
    double p = 0.0;

    // Do each node before its parent
    for(int column=0;column<A.length();column++) {
      double P = Pr(A,T,MModel,SM,column);
      p += P;
      std::cerr<<column<<"  ln P = "<<P<<"     ln total = "<<p<<std::endl;
    }

    //    std::cerr<<" substitution: P="<<P<<std::endl;
    return p;
  }

  double Pr(const alignment& A,const Parameters& P) {
    double result = Pr(A,P.T,P.SModel(),P);
#ifndef NDEBUG
    Parameters P2 = P;
    P2.recalc();
    double result2 = Pr(A,P.T,P.SModel(),P2);
    assert(std::abs(result - result2) < 1.0e-9);
#endif
    return result;
  }


  double Pr_star(const vector<int>& column,const tree& T,const ReversibleModel& SModel,
		 const vector<Matrix>& transition_P) {
    const alphabet& a = SModel.Alphabet();

    if (T.leaves() == 2)
      return Pr(column,T,SModel,transition_P);

    double p=0;
    for(int lroot=0;lroot<a.size();lroot++) {
      double temp=SModel.frequencies()[lroot];
      for(int b=0;b<T.leaves();b++) {
	const Matrix& Q = transition_P[b];

	int lleaf = column[b];
	if (a.letter(lleaf))
	  temp *= Q(lroot,lleaf);
      }
      p += temp;
    }

    // we don't get too close to zero, normally
    assert(0 <= p and p <= 1.00000000001);

    return p;
  }

  double Pr_star(const alignment& A, const tree& T, const MultiModel& MModel, const MatCache& MC) {

    double p = 0.0;
  
    vector<int> residues(A.size2());

    // Do each node before its parent
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);

      double total=0;
      for(int m=0;m<MModel.n_base_models();m++)
	total += MModel.distribution()[m] * Pr_star(residues,
						    T,
						    MModel.base_model(m),
						    MC.transition_P(m)
						    );

      // we don't get too close to zero, normally
      assert(0 < total and total <= 1.00000000001);

      p += log(total);
    }

    return p;
  }

  double Pr_star(const alignment& A,const Parameters& P) {
    return Pr_star(A, P.T, P.SModel(), P);
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
    const int n = (T1.leaves()*(T1.leaves()-1))/2;
    double ave = sum/n;

    //-------- Set branch lengths to ave/2  ----------//
    for(int b=0;b<T1.leafbranches();b++)
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
   
      int b = i;
      if (T1.leaves() == 2) b=0;

      P2.setlength(b,exp(ave)/2.0);
    }
    
    //----------- Get log L w/ new tree  -------------//
    return Pr_star(A,P2);
  }

  double Pr_unaligned(const alignment& A,const Parameters& P) {
    const alphabet& a = A.get_alphabet();

    vector<double> count(a.size(),0);

    for(int i=0;i<A.num_sequences();i++) {
      for(int column=0;column<A.length();column++) {
	int l = A(column,i);
	if (a.letter(l))
	  count[l]++;
      }
    }
    
    double total=0;
    for(int l=0;l<count.size();l++)
      total += log(P.SModel().frequencies()[l])*count[l];
    return total;
  }
}
