#include "substitution.H"
#include "substitution-index.H"
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

  struct peeling_info: public vector<int> {
    peeling_info(const Tree&T) { reserve(T.n_branches()); }
  };

  /// compute log(probability) from conditional likelihoods (S) and equilibrium frequencies as in MModel
  efloat_t Pr(const Matrix& S,const MultiModel& MModel) {
    const alphabet& a = MModel.Alphabet();

    double total = 0;
    for(int m=0;m<MModel.n_base_models();m++) {
      double p = 0;

      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int l=0;l<a.size();l++)
	p += S(m,l) * f[l];

      // A specific model (e.g. the INV model) could be impossible
      assert(0 <= p and p <= 1.00000000001);
      total += p * MModel.distribution()[m];
    }

    // SOME model must be possible
    assert(0 < total and total <= 1.00000000001);

    return total;
  }

  efloat_t calc_root_probability(const alignment& A,const Tree& T,Likelihood_Cache& cache,
			       const MultiModel& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    const alphabet& a = A.get_alphabet();

    const int root = cache.root;

    if (T[root].is_leaf_node())
      throw myexception()<<"Trying to accumulate conditional likelihoods at a root node is not allowed.";

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    // cache matrix of frequencies
    Matrix F(n_models,asize);
    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int l=0;l<asize;l++) 
	F(m,l) = f[l]*p;
    }

    efloat_t total = 1;
    for(int i=0;i<index.size1();i++) {

      double p_col=0;
      for(int m=0;m<n_models;m++) {
	double p_model = 0;

	//-------------- Set letter & model prior probabilities  ---------------//
	for(int l=0;l<asize;l++) 
	  S(m,l) = F(m,l);

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<rb.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= cache(i0,rb[j])(m,l);
	}

	//--------- If there is a letter at the root, condition on it ---------//
	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (a.is_letter_class(rl))
	    for(int l=0;l<asize;l++)
	      if (not a.matches(l,rl))
		S(m,l) = 0;
	}

	//--------- If there is a letter at the root, condition on it ---------//
	for(int l=0;l<asize;l++)
	  p_model += S(m,l);

	// A specific model (e.g. the INV model) could be impossible
	assert(0 <= p_model and p_model <= 1.00000000001);

	p_col += p_model;
      }

      // SOME model must be possible
      assert(0 < p_col and p_col <= 1.00000000001);

      total *= p_col;
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }

    return total;
  }

  efloat_t calc_root_probability(const data_partition& P,const vector<int>& rb,
			       const ublas::matrix<int>& index) 
  {
    return calc_root_probability(P.A, P.T, P.LC, P.SModel(), rb, index);
  }

  void peel_branch(int b0,Likelihood_Cache& cache, const alignment& A, const Tree& T, 
		   const MatCache& transition_P,const MultiModel& MModel)
  {
    const alphabet& a = A.get_alphabet();

    // compute branches-in
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    // get the relationships with the sub-alignments
    b.push_back(b0);
    ublas::matrix<int> index = subA_index_select(b,A,T);
    assert(index.size1() == subA_length(A,b0));
    assert(subA_index_valid(A,b0));
    b.pop_back();

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    for(int i=0;i<subA_length(A,b0);i++) {

      // compute the distribution at the parent node - single letter
      if (not b.size()) {
	int l2 = A.note(0,i+1,b0);
	if (a.is_letter(l2))
	  for(int m=0;m<n_models;m++) {
	    const Matrix& Q = transition_P[m][b0%B];
	    for(int l1=0;l1<asize;l1++)
	      cache(i,b0)(m,l1) = Q(l1,l2);
	  }
	else if (a.is_letter_class(l2)) {
	  for(int m=0;m<n_models;m++) {
	    const Matrix& Q = transition_P[m][b0%B];
	    for(int l1=0;l1<asize;l1++)
	      cache(i,b0)(m,l1) = sum(Q,l1,l2,a);
	  }	  
	}
	else
	  for(int m=0;m<n_models;m++) 
	    for(int l=0;l<asize;l++)
	      cache(i,b0)(m,l) = 1;
      }
      // compute the distribution at the target (parent) node - 2 branch distributions
      else if (b.size() == 1 and false)
	; // compute the source distribution from model-switching matrix
      else if (b.size() == 2) 
      {
	// compute the source distribution from 2 branch distributions
	int i0 = index(i,0);
	int i1 = index(i,1);
	if (i0 != alphabet::gap and i1 != alphabet::gap)
	  for(int m=0;m<n_models;m++) 
	    for(int j=0;j<asize;j++)
	      S(m,j) = cache(i0,b[0])(m,j)* cache(i1,b[1])(m,j);
	else if (i0 != alphabet::gap)
	  S = cache(i0,b[0]);
	else if (i1 != alphabet::gap)
	  S = cache(i1,b[1]);
	else
	  std::abort();

	// propagate from the source distribution
	Matrix& R = cache(i,b0);            //name result matrix
	for(int m=0;m<n_models;m++) {

	  // FIXME!!! - switch order of MatCache to be MC[b][m]
	  const Matrix& Q = transition_P[m][b0%B];

	  // compute the distribution at the target (parent) node - multiple letters
	  for(int l=0;l<asize;l++) {
	    double temp=0;
	    for(int j=0;j<asize;j++)
	      temp += Q(l,j)*S(m,j);
	    R(m,l) = temp;
	  }
	}
#ifndef NDEBUG
	Pr(R,MModel);
#endif
      }
      else
	std::abort();

    }
    cache.validate_branch(b0);
  }


  /// Compute an ordered list of branches to process
  inline peeling_info get_branches(const Tree& T, const Likelihood_Cache& LC) 
  {
    //------- Get ordered list of not up_to_date branches ----------///
    peeling_info peeling_operations(T);

    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[LC.root].branches_in(),branches);

    for(int i=0;i<branches.size();i++) {
	const const_branchview& db = branches[i];
	if (not LC.up_to_date(db)) {
	  append(db.branches_before(),branches);
	  peeling_operations.push_back(db);
	}
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  static 
  int calculate_caches(const alignment& A, const MatCache& MC, const Tree& T,Likelihood_Cache& cache,
		       const MultiModel& MModel) {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T, cache);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],cache,A,T,MC,MModel);

    return ops.size();
  }

  int calculate_caches(const data_partition& P) {
    return calculate_caches(P.A, P.MC, P.T, P.LC, P.SModel());
  }

  Matrix get_rate_probabilities(const alignment& A,const MatCache& MC,const Tree& T,
				Likelihood_Cache& cache,const MultiModel& MModel)
  {
    const alphabet& a = A.get_alphabet();

    const int root = cache.root;
    
    // make sure that we are up-to-date
    calculate_caches(A,MC,T,cache,MModel);

    // declare a matrix to store our results in
    Matrix probs(A.length(),MModel.n_base_models());

    // initialize the entries to prior probability of each sub-model
    for(int m=0;m<probs.size2();m++)
      for(int c=0;c<probs.size1();c++)
	probs(c,m) = MModel.distribution()[m];

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the index
    ublas::matrix<int> index = subA_index(root,A,T);

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    // cache matrix of frequencies
    Matrix F(n_models,asize);
    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int l=0;l<asize;l++) 
	F(m,l) = f[l]*p;
    }

    for(int i=0;i<index.size1();i++) {
      double p_col = 0;
      for(int m=0;m<n_models;m++) {

	//-------------- Set letter & model prior probabilities  ---------------//
	for(int l=0;l<asize;l++) 
	  S(m,l) = F(m,l);

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<rb.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= cache(i0,rb[j])(m,l);
	}

	//--------- If there is a letter at the root, condition on it ---------//
	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (a.is_letter_class(rl))
	    for(int l=0;l<asize;l++)
	      if (not a.matches(l,rl))
		S(m,l) = 0;
	}

	//--------- If there is a letter at the root, condition on it ---------//
	probs(i,m) = 0;
	for(int l=0;l<asize;l++)
	  probs(i,m) += S(m,l);

	// A specific model (e.g. the INV model) could be impossible
	assert(0 <= probs(i,m) and probs(i,m) <= 1.00000000001);

	p_col += probs(i,m);
      }

      // SOME model must be possible
      assert(0 < p_col and p_col <= 1.00000000001);
      for(int m=0;m<n_models;m++)
	probs(i,m) /= p_col;
    }
    return probs;
  }


  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const data_partition& P, const vector<int>& b,
			 const vector<int>& req,const vector<int>& seq,int delta)
  {
    const alphabet& a = P.get_alphabet();

    const alignment& A = P.A;
    const Tree& T = P.T;
    Likelihood_Cache& LC = P.LC;

#ifndef NDEBUG
    subA_index_check_footprint(A,T);
    subA_index_check_regenerate(A,T);
#endif

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    LC.root = root;

    ublas::matrix<int> index = subA_index_any(b,A,T,req,seq);

    int n_br = calculate_caches(P);
#ifndef NDEBUG
    std::clog<<"get_column_likelihoods: Peeled on "<<n_br<<" branches.\n";
#endif

    vector<Matrix> L;
    L.reserve(A.length()+2);

    Matrix& S = LC.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    //Add the padding matrices
    {
      for(int i=0;i<S.size1();i++)
	for(int j=0;j<S.size2();j++)
	  S(i,j) = 0;

      for(int i=0;i<delta;i++)
	L.push_back(S);
    }

    for(int i=0;i<index.size1();i++) {

      for(int m=0;m<n_models;m++) {
	for(int l=0;l<asize;l++) 
	  S(m,l) = 1;

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<b.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= LC(i0,b[j])(m,l);
	}

	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (a.is_letter_class(rl))
	    for(int l=0;l<asize;l++)
	      if (not a.matches(l,rl))
		S(m,l) = 0;
	}
      }
      L.push_back(S);
    }
    return L;
  }

  efloat_t other_subst(const data_partition& P, const vector<int>& nodes) 
  {
    const alignment& A = P.A;
    const Tree& T = P.T;
    Likelihood_Cache& LC = P.LC;

    int n_br = calculate_caches(P);
#ifndef NDEBUG
    std::clog<<"other_subst: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index1 = subA_index_none(rb,A,T,nodes);
    efloat_t Pr1 = calc_root_probability(P,rb,index1);

#ifndef NDEBUG
    ublas::matrix<int> index2 = subA_index_any(rb,A,T,nodes);
    ublas::matrix<int> index  = subA_index(rb,A,T);

    efloat_t Pr2 = calc_root_probability(P,rb,index2);
    efloat_t Pr  = calc_root_probability(P,rb,index);

    assert(std::abs(log(Pr1 * Pr2) - log(Pr) ) < 1.0e-9);
#endif

    return Pr1;
  }

  efloat_t Pr(const alignment& A,const MatCache& MC,const Tree& T,Likelihood_Cache& LC,
	    const MultiModel& MModel)
  {
#ifndef NDEBUG
    subA_index_check_footprint(A,T);
    subA_index_check_regenerate(A,T);
#endif

#ifndef DEBUG_CACHING
    if (LC.cv_up_to_date()) {
#ifndef NDEBUG
      std::clog<<"Pr: Using cached value "<<log(LC.cached_value)<<"\n";
#endif
      return LC.cached_value;
    }
#endif

    int n_br = calculate_caches(A,MC,T,LC,MModel);
#ifndef NDEBUG
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = subA_index(rb,A,T);

    // get the probability
    efloat_t Pr = calc_root_probability(A,T,LC,MModel,rb,index);

    LC.cached_value = Pr;
    LC.cv_up_to_date() = true;

    return Pr;
  }

  efloat_t Pr(const data_partition& P,Likelihood_Cache& LC) {
    return Pr(P.A, P.MC, P.T, LC, P.SModel());
  }

  efloat_t Pr(const data_partition& P) {
    efloat_t result = Pr(P, P.LC);

#ifdef DEBUG_CACHING
    data_partition P2 = P;
    P2.LC.invalidate_all();
    invalidate_subA_index_all(A);
    efloat_t result2 = Pr(A, P2, P2.LC);
    if (std::abs(log(result) - log(result2))  > 1.0e-9) {
      std::cerr<<"Pr: diff = "<<log(result)-log(result2)<<std::endl;
      std::abort();
    }
#endif

    return result;
  }
}
