#include "substitution.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>

// recalculate a likelihood immediate afterwards, and see if we get the same answer...
// perhaps move the collection root node one branch away?
// then we have to do re-validation...

using std::valarray;
using std::vector;

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
  //  Question: should have the matrix be per-rate, or per-branch?
  //   - per-rate means that all branches in one matrix - rates are done separately
  //   - per-branch means that you do all branches for one rate at the same time
  //     which allows weight-change models...

  typedef vector< Matrix > Column_Likelihood_Cache_t;
  typedef vector< Column_Likelihood_Cache_t > Likelihood_Cache_t;

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



  /// A structure which holds all topology info to peel along directed branch 'db'
  struct peeling_branch_info {
    int b;
    int b1;
    int b2;
    int source;

    peeling_branch_info(const const_branchview& db,const Tree& T)
      :b(db),
       b1(-1),
       b2(-1),
       source(db.source())
    {
      const_in_edges_iterator i = db.branches_before();
      if (i) {
	b1 = *i;
	i++;
      }

      if (i) {
	b2 = *i;
	i++;
      }

      assert(not i);
    }
  };

  struct peeling_info: public vector<peeling_branch_info> {
    int rb1;
    int rb2;
    int rb3;

    int root;
    peeling_info(const Tree&T, int r):rb1(-1),rb2(-1),rb3(-1),root(r) {
      const_in_edges_iterator i = T[root].branches_in();

      assert(i); // We had better have at least one neighbor!

      rb1 = *i;
      i++;

      if (i) {
	rb2 = *i;
	i++;
      }

      if (i) {
	rb3 = *i;
	i++;
      }

      assert(not i);

      reserve(T.n_branches());
    }
  };


  /// Compute the letter likelihoods at the root
  void calc_root_likelihoods(const vector<int>& residues, Matrix& distributions,
			     const peeling_info& pi) 
  {
    const int scratch = distributions.size1()-1;

    const int asize = distributions.size2();

    //-------------- Propagate and collect information at 'root' -----------//
    for(int l=0;l<asize;l++)
      distributions(scratch,l) = distributions(pi.rb1,l);

    if (pi.rb2 != -1)
      for(int l=0;l<asize;l++)
	distributions(scratch,l) *= distributions(pi.rb2,l);

    if (pi.rb3 != -1)
      for(int l=0;l<asize;l++)
	distributions(scratch,l) *= distributions(pi.rb3,l);

    //-------------- Take into account letters at 'root' -------------//
    //FIXME - we could avoid calculations for 
    if (alphabet::letter(residues[pi.root]))
      for(int l=0;l<asize;l++)
	if (l != residues[pi.root])
	  distributions(scratch,l) = 0;
  }


  /// Peel along each branch in work-list @branches 
  inline void peel(const peeling_info& branches,
		   Matrix& distributions,
		   const vector<int>& residues,
		   const vector<Matrix>& transition_P)
  {
    

    // The number of directed branches is twice the number of undirected branches
    const int B     = distributions.size1()/2;
    const int asize = distributions.size2();

    // record if this distribution is just '*'
    valarray<bool> uninformative(false,2*B);  // how much speedup does this give?

    for(int i=0;i<branches.size();i++) {

      // Get info 
      int b     = branches[i].b;     // directed branch from source -> target
      int b1    = branches[i].b1;    // directed branch from n1     -> source, -1 if leaf(source)
      int b2    = branches[i].b2;    // directed branch from n2     -> source, -1 if leaf(source)
      int source = branches[i].source;   // = T.directed_branch(b).source();

      // Propogate info along branch - doesn't depend on direction of b
      const Matrix& Q = transition_P[b%B];

      // compute the distribution at the target (parent) node - single letter
      if (b1 < 0 and alphabet::letter(residues[source])) 
	for(int i=0;i<asize;i++)
	  distributions(b,i) = Q(i,residues[source]);

      // compute the distribution at the target (parent) node - wildcard
      else if (b1 < 0 or (uninformative[b1] and uninformative[b2])) {
	for(int i=0;i<asize;i++)
	  distributions(b,i) = 1.0;
	uninformative[b] = true;
      }

      // cache the source distribution, or not?
      else {
	const int scratch = distributions.size1()-1;
	for(int j=0;j<asize;j++)
	  distributions(scratch,j) = distributions(b1,j)*distributions(b2,j);

	// compute the distribution at the target (parent) node - multiple letters
	for(int i=0;i<asize;i++) {
	  double temp=0;
	  for(int j=0;j<asize;j++)
	    temp += Q(i,j)*distributions(scratch,j);  // cache, or no cache?
	  distributions(b,i) = temp;
	}
      }

    }

    //-------------- collect at 'root' ---------------//
    calc_root_likelihoods(residues,distributions,branches);
  }


  /// Compute an ordered list of branches to process
  inline peeling_info get_branches(const Tree& T, int root, const vector<bool>& up_to_date) 
  {
    //------- Get ordered list of not up_to_date branches ----------///
    peeling_info peeling_operations(T,root);

    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[root].branches_in(),branches);

    for(int i=0;i<branches.size();i++) {
	const const_branchview& db = branches[i];
	if (not up_to_date[db]) {
	  append(db.branches_before(),branches);
	  peeling_operations.push_back(peeling_branch_info(db,T));
	}
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  valarray<double> 
  get_column_likelihoods(const vector<int>& residues,const Tree& T,const ReversibleModel& SModel,
			 const vector<Matrix>& transition_P,int root) 
  {
    const alphabet& a = SModel.Alphabet();

    //------ Allocate space and mark all branches out of date -------//
    Matrix distributions(2*T.n_branches()+1,a.size());
    vector<bool> up_to_date(2*T.n_branches(),false);

    //----------- determine the operations to perform -----------------//
    peeling_info branches = get_branches(T,root,up_to_date);
    
    //-------- propagate info along branches ---------//
    peel(branches,distributions,residues,transition_P);

    //----------- return the result ------------------//
    const int scratch = distributions.size1()-1;
    valarray<double> likelihoods(distributions.size2());
    for(int i=0;i<likelihoods.size();i++)
      likelihoods[i] = distributions(scratch,i);
    
    return likelihoods;
  }

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  valarray<double> 
  get_column_likelihoods(vector<int> residues,const Tree& T,const ReversibleModel& SModel,
			 const vector<Matrix>& transition_P,int root, const valarray<bool>& group) 
  {
    for(int i=0;i<residues.size();i++)
      if (not group[i]) residues[i] = alphabet::not_gap;

    return get_column_likelihoods(residues,T,SModel,transition_P,root);
  }



  double Pr(const vector<int>& residues, const peeling_info& branches, const ReversibleModel& SModel,
	    const vector<Matrix>& transition_P, Matrix& distributions)
  {
    const alphabet& a = SModel.Alphabet();

    //-------- propagate info along branches ---------//
    peel(branches,distributions,residues,transition_P);

    const int scratch = distributions.size1()-1;

    double p = 0;
    for(int l=0;l<a.size();l++)
      p += distributions(scratch,l)*SModel.frequencies()[l];

    // we don't get too close to zero, normally
    assert(0 <= p and p <= 1.00000000001);

    return p;
  }

  double Pr(const vector<int>& residues,const Tree& T,const ReversibleModel& SModel,
	    const vector<Matrix>& transition_P,int root) {
    assert(residues.size() == T.n_nodes());

    const alphabet& a = SModel.Alphabet();
    Matrix distributions(2*T.n_branches()+1,a.size());

    //------ Allocate space and mark all branches out of date -------//
    vector<bool> up_to_date(2*T.n_branches(),false);

    //----------- determine the operations to perform -----------------//
    peeling_info branches = get_branches(T,root,up_to_date);
    
    return Pr(residues,branches,SModel,transition_P,distributions);
  }

  double Pr(const vector<int>& residues,const Tree& T,const ReversibleModel& SModel,
	    const vector<Matrix>& transition_P) 
  {
    const alphabet& a = SModel.Alphabet();
    Matrix distributions(2*T.n_branches()+1,a.size());
    //------ Allocate space and mark all branches out of date -------//
    vector<bool> up_to_date(2*T.n_branches(),false);

    //----------- determine the operations to perform -----------------//
    int root = T.n_nodes()-1;
    peeling_info branches = get_branches(T,root,up_to_date);
    return Pr(residues,branches,SModel,transition_P,distributions);
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

  double Pr(const alignment& A, const peeling_info& operations, const MultiModel& MModel, 
	    const MatCache& MC,int column,Matrix& distributions) 
  {

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
					  operations,
					  MModel.get_model(m),
					  MC.transition_P(m),
					  distributions
					  );

#ifdef DEBUG_BROKEN
      {
	int node = myrandom(0,T.n_nodes());
	vector<bool> up_to_date(2*T.n_branches(),false);
	peeling_info branches2 = get_branches(T,root,up_to_date);
	double p2 = Pr(residues,branches2,SModel,transition_P,distributions);
	
	if (std::abs(p2-p) > 1.0e-9) {
	  for(int i=0;i<T.n_leaves();i++)
	    std::cerr<<SModel.Alphabet().lookup(residues[i])<<" ";
	  std::cerr<<p<<" "<<log(p)<<"     "<<p2<<"      "<<log(p2)<<endl;
	  std::abort(); //FIXME - try this check!
	}
      }
#endif

      total += p;
    }

    // we don't get too close to zero, normally
    assert(0 < total and total <= 1.00000000001);
    
    return log(total);
  }

  double Pr(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC,int column) {
    const alphabet& a = MModel.Alphabet();
    //------ Allocate space and mark all branches out of date -------//
    Matrix distributions(2*T.n_branches()+1,a.size());

    //---------- determine the operations to perform ----------------//
    vector<bool> up_to_date(2*T.n_branches(),false);
    int root = T.n_nodes()-1;
    peeling_info operations = get_branches(T,root,up_to_date);
    
    //---------------- sum the column likelihoods -------------------//
    return Pr(A,operations,MModel,MC,column,distributions);
  }

  double Pr(const alignment& A, const Parameters& P,Conditional_Likelihoods& L) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    int root = L.root;
    peeling_info operations = get_branches(T,root,L.up_to_date);
    
    //---------------- sum the column likelihoods -------------------//
    double p = 0.0;
    for(int column=0;column<A.length();column++) 
      p += Pr(A,operations,MModel,MC,column,L[column]);

    //    std::cerr<<" substitution: P="<<P<<std::endl;
    return p;
  }

  double Pr(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC) {
    const alphabet& a = MModel.Alphabet();
    //------ Allocate space and mark all branches out of date -------//
    Matrix distributions(2*T.n_branches()+1,a.size());

    //---------- determine the operations to perform ----------------//
    vector<bool> up_to_date(2*T.n_branches(),false);
    int root = T.n_nodes()-1;
    peeling_info operations = get_branches(T,root,up_to_date);
    
    //---------------- sum the column likelihoods -------------------//
    double p = 0.0;
    for(int column=0;column<A.length();column++) 
      p += Pr(A,operations,MModel,MC,column,distributions);

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


  double Pr_star(const vector<int>& column,const Tree& T,const ReversibleModel& SModel,
		 const vector<Matrix>& transition_P) {
    const alphabet& a = SModel.Alphabet();

    if (T.n_leaves() == 2)
      return Pr(column,T,SModel,transition_P);

    double p=0;
    for(int lroot=0;lroot<a.size();lroot++) {
      double temp=SModel.frequencies()[lroot];
      for(int b=0;b<T.n_leaves();b++) {
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

  double Pr_star(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC) {

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
    const Tree& T1 = P.T;
    Parameters P2 = P;

    //----------- Get Distance Matrix --------------//
    Matrix D(T1.n_leaves(),T1.n_leaves());
    for(int i=0;i<T1.n_leaves();i++) 
      for(int j=0;j<T1.n_leaves();j++) 
	D(i,j) = T1.distance(i,j);

    //----------- Get Average Distance -------------//
    double sum=0;
    for(int i=0;i<T1.n_leaves();i++) 
      for(int j=0;j<i;j++) 
	sum += D(i,j);
    const int n = (T1.n_leaves()*(T1.n_leaves()-1))/2;
    double ave = sum/n;

    //-------- Set branch lengths to ave/2  ----------//
    for(int b=0;b<T1.n_leafbranches();b++)
      P2.setlength(b,ave/2.0);


    //----------- Get log L w/ new tree  -------------//
    return Pr_star(A,P2);
  }

  double Pr_star_estimate(const alignment& A,const Parameters& P) {
    const Tree& T1 = P.T;
    Parameters P2 = P;
    
    //----------- Get Distance Matrix --------------//
    Matrix D(T1.n_leaves(),T1.n_leaves());
    for(int i=0;i<T1.n_leaves();i++) 
      for(int j=0;j<T1.n_leaves();j++) 
	D(i,j) = T1.distance(i,j);
    
    
    //---- Set branch lengths to ave/2 per branch ----//
    for(int i=0;i<T1.n_leaves();i++) {
      double ave=0;
      for(int j=0;j<T1.n_leaves();j++) {
	if (i==j) continue;
	ave += log(D(i,j));
      }
      ave /= (T1.n_leaves()-1);
   
      int b = i;
      if (T1.n_leaves() == 2) b=0;

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
