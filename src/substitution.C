#include "substitution.H"
#include "rng.H"
#include <cmath>
#include <valarray>

// recalculate a likelihood immediate afterwards, and see if we get the same answer...
// perhaps move the collection root node one branch away?
// then we have to do re-validation...

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


  // Structure for cached likelihoods:
  // LikelihoodCache[column][rate](branch,letter)
  //  Question: should have the matrix be per-rate, or per-branch? (?per-rate?)

  typedef vector< Matrix > Column_Likelihood_Cache_t;
  typedef vector< ColumnLikelihoodCache_t > Likelihood_Cache_t;

  //  The Matrix will be twice the size, since it will have results for each branch in each direction

  // directed branch b_ which goes from n1->n2 will have index 'b' if n2 > n1, otherwise 'b+B'

  // If we are going to return distributions(root), then we must make sure
  //  that there is no information there from the WRONG side of the tree

  // things to do:  (look at Sampler Todo.sxw)
  // 2. Then separate out part of the routine as an inline function, to which 'distributions'
  //    is passed.   (change 'distributions' into a matrix?)
  // 3. Make the 'branches' array not depend on 'group'...
  //    In the common case (doing the full tree) this might be a speedup...




  // Changes:
  // 1. no longer using used[] array to keep track of .... ???
  // 2. we pass in info about the direction of the branch -> b+B means reversed b.
  // 3. when we compute the branch-end distribution, 
  //    a) we don't multiply.
  //    b) Instead we multiply to find the START, if necessary.
  //    c) To get the root distribution, we merge THREE branch distributions or ONE
  //    d) And if there is a letter at the root, the we condition on that, as well


  struct peeling_info {
    int b_;
    int b1_;
    int b2_;
    int child;
    peeling_info(int x, int y, int z, int w): b_(x),b1_(y), b2_(z_), child(w) {}
  };


  /// Peel along each branch in work-list @branches 
  inline void peel(const vector<peeling_info>& branches,
		   Matrix& distributions,
		   const vector<int>& residues,
		   const vector<Matrix>& transition_P)
  {
    // The number of directed branches is twice the number of undirected branches
    const int B     = distributions.size1()/2;
    const int asize = distributions.size2();

    for(int i=0;i<branches.size();i++) {
      // Get info 
      int b_     = branches[i].b_;      // directed branch from source -> target
      int b1_    = branches[i].b1_;     // directed branch from n1     -> source, -1 if leaf(source)
      int b2_    = branches[i].b2_;     // directed branch from n2     -> source, -1 if leaf(source)
      int source = branches[i].child_;  // = T.directed_branch(b_).child();

      // Propogate info along branch - doesn't depend on direction of b
      const Matrix& Q = transition_P[b_%B];

      if (b1 < 0) {
	// compute the distribution at the target (parent) node - single letter
	if (alphabet::letter(residues[source]))
	  for(int i=0;i<asize;i++)
	    distributions(b_,i) = Q(i,residues[source]);
	// compute the distribution at the target (parent) node - wildcard
	else
	  for(int i=0;i<asize;i++)
	    distributions(b_,i) = 1.0;
      }
      else {
	// cache the source distribution, or not?
	const int scratch = distributions.size1()-1;
	for(int j=0;j<asize;j++)
	  distributions(scratch,j) = distributions(b1_,j)*distributions(b2_,j);

	// compute the distribution at the target (parent) node - multiple letters
	for(int i=0;i<asize;i++) {
	  double temp=0;
	  for(int j=0;j<asize;j++)
	    temp += Q(i,j)*distributions(scratch,j);  // cache, or no cache?
	  distributions(b_,i) = temp;
	}
      }

    }
  }

  /// Compute an ordered list of branches to processn
  inline vector<peeling_info> get_branches(const tree& T, int root, const valarray<bool>& up_to_date,
					   const valarray<bool>& group) {

    //FIXME - walk up the tree from peeling 'root' to the tree root, 
    // instead of computing branches2 and using 'reverse'
    vector<peeling_info> branches1;
    branches1.reserve(T.num_nodes());
    vector<peeling_info> branches2;
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
	int b_  = T.directed_branch(child,parent);
	int b1_,b2_;
	T.get_branches_in(b_,b1_,b2_);

	if (not up_to_date[b_])
	  branches2.push_back(peeling_info(b_,b1_,b2_,child));
      }
      // otherwise propagate up to a node that is >= the root
      else {
	int parent = T[child].parent();
	int b_ = T.directed_branch(child,parent);
	int b_  = T.directed_branch(child,parent);
	int b1_,b2_;
	T.get_branches_in(b_,b1_,b2_);

	if (not up_to_date[b_])
	  branches1.push_back(peeling_info(b_,b1_,b2_,child));
      }
    }
    // if (branches2.size()) {}
    std::reverse(branches2.begin(),branches2.end());
    branches1.insert(branches1.end(),branches2.begin(),branches2.end());
  
    return branches1;
  }

  valarray<double> get_root_distribution(const vector<int>& residues, const Matrix& distributions,
					 const tree& T,int root) {
    const int asize = distributions.size2();

    valarray<double> distribution(asize);
    int b1_;
    int b2_;
    int b3_;

    // get the branches;

    for(int i=0;i<asize;i++)
      distribution[i] = distributions(b1_,i);

    if (b2_ >= 0)
      for(int i=0;i<asize;i++)
	distribution[i] *= distributions(b2_,i);

    if (b3_ >= 0)
      for(int i=0;i<asize;i++)
	distribution[i] *= distributions(b3_,i);

    if (alphabet::letter(residues[root]))
      for(int i=0;i<asize;i++)
	if (i!=root)
	  distribution[i] == 0;

    return distribution;
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

    // Allocate Matrix and mark all branches out of date
    Matrix distributions(2*T.n_branches()+1,a.size());
    vector<bool> up_to_date(2*T.n_branches(),false);

    // how to deal with letters outside the group?

    // how to deal with gaps?

    //------- Put the info from the letters into the distribution -------//
    bool any_letters = false;

    valarray<bool> used(false,T.num_nodes()-1);

    for(int i=0;i<T.num_nodes()-1;i++) {
      if (alphabet::letter(residues[i]) and group[i]) {
	//are there ANY letters at all?
	any_letters = true;

	used[i] = true;

	for(int j=0;j<a.size();j++)
	  distributions(i,j) = 0;
	distributions(i,residues[i]) = 1;
      }
    }

    vector<peeling_info> branches = get_branches(T,root,up_to_date,group);

    peel(branches,distributions,residues,transition_);

    //This can only happen if none of our nodes has info
    assert(used[root] or not any_letters);

    //----------- return the result ------------------//
    return get_root_distribution(residues,distributions,T,root);
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

  double Pr_INV(const vector<int>& residues, const ReversibleModel& SModel) {
    int letter = alphabet::gap;

    for(int i=0;i<residues.size();i++) {
      if (alphabet::letter(residues[i]))
	if (letter == alphabet::gap)
	  letter=residues[i];
	else if (letter != residues[i])
	  return 0.0;
      ;
    }
    if (letter == alphabet::gap)
      return 1.0;
    else
      return SModel.frequencies()[letter];
  }

  double Pr(const alignment& A, const tree& T, const MultiModel& MModel, const MatCache& MC,int column) {
    
    vector<int> residues(A.size2());
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
  
    double total=0;
    for(int m=0;m<MModel.nmodels();m++) {
      double p=0;
      double model_rate = MModel.rates()[m]*MModel.get_model(m).rate();
      if (model_rate == 0.0) 
	p = MModel.distribution()[m] * Pr_INV(residues,
					      MModel.get_model(m)
					      );      
      else
	p = MModel.distribution()[m] * Pr(residues,
					  T,
					  MModel.get_model(m),
					  MC.transition_P(m)
					  );
      total += p;
    }

    // we don't get too close to zero, normally
    assert(0 < total and total <= 1.00000000001);
    
    return log(total);
  }

  double Pr(const alignment& A, const tree& T, const MultiModel& MModel, const MatCache& SM) {
    double p = 0.0;

    // Do each node before its parent
    for(int column=0;column<A.length();column++) 
      p += Pr(A,T,MModel,SM,column);

    //    std::cerr<<" substitution: P="<<P<<std::endl;
    return p;
  }

  double Pr(const alignment& A,const Parameters& P) {
    double result = Pr(A,P.T,P.SModel(),P);
#ifndef NDEBUG
    Parameters P2 = P;
    P2.recalc();
    double result2 = Pr(A,P.T,P.SModel(),P2);
    assert(result == result2);
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
      for(int m=0;m<MModel.nmodels();m++)
	total += MModel.distribution()[m] * Pr_star(residues,
						    T,
						    MModel.get_model(m),
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
