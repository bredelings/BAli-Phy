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

  struct peeling_info: public vector<int> {
    peeling_info(const Tree&T) { reserve(T.n_branches()); }
  };

  typedef Likelihood_Cache& column_cache_t;

  ublas::matrix<int> leaf_index(int b,const alignment& A) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),1);
    int l=0;
    for(int c=0;c<A.length();c++) {
      if (not A.gap(c,b))
	subA(l++,0) = A(c,b);
    }

    assert(l == A.seqlength(b));

    //resize the matrix, and send it back...
    ublas::matrix<int> temp(l,1);
    for(int i=0;i<temp.size1();i++)
      temp(i,0) = subA(i,0);
    
    return temp;
  }

  static vector<vector<int> > get_subtree_leaves(const vector<int>& b,const Tree& T) {
    // get criteria for being in a sub-A
    vector<vector<int> > leaves(b.size());
    for(int i=0;i<b.size();i++) {
      leaves[i].reserve(T.n_leaves());
      valarray<bool> p = T.partition(T.directed_branch(b[i]).reverse());
      for(int j=0;j<T.n_leaves();j++)
	if (p[j]) leaves[i].push_back(j);
    }

    return leaves;
  }

  /// increment and return the index if any nodes in 'mask' are present in column 'c'
  inline int inc(int& index,const vector<int>& mask, 
		 const alignment& A,int c) 
  {
    for(int i=0;i<mask.size();i++)
      if (not A.gap(c,mask[i])) {
	return ++index;
      }
    return alphabet::gap;
  }

  static ublas::matrix<int> subA_index_simple(const alignment& A, 
					      const vector<vector<int> >& leaves) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),leaves.size()+1);

    // declare the index for the nodes
    vector<int> index(leaves.size(),-1);

    // calculate the index of each column
    for(int c=0;c<A.length();c++) {
      for(int j=0;j<leaves.size();j++) 
	subA(c,j) = inc(index[j],leaves[j],A,c);
      subA(c,leaves.size()) = c;
    }

    return subA;
  }

  static bool column_empty(const ublas::matrix<int>& M,int c) {
    const int I = M.size2()-1;
    for(int i=0;i<I;i++)
      if (M(c,i) != alphabet::gap)
	return false;
    return true;
  }

  ublas::matrix<int> subA_select(const ublas::matrix<int>& subA1) {
    const int I = subA1.size2()-1;

    // count the number of columns to keep
    int L=0;
    for(int c=0;c<subA1.size1();c++)
      if (subA1(c,I) != alphabet::gap) L++;

    ublas::matrix<int> subA2(L,I);

    for(int c=0;c<subA1.size1();c++) {
      int c2 = subA1(c,I);
      if (c2 == alphabet::gap) continue;

      for(int j=0;j<subA2.size2();j++)
	subA2(c2,j) = subA1(c,j);
    }

    return subA2;
  }

  ublas::matrix<int> subA_index(const vector<int>& b,const alignment& A,const Tree& T) 
  {
    // get ordered ist of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_simple(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (column_empty(subA,c))
	subA(c,I) = alphabet::gap;
      else
	subA(c,I) = l++;

    // return processed indices
    return subA_select(subA);
  }


  bool any_present(const alignment& A, int c, const vector<int>& nodes) {
    for(int j=0;j<nodes.size();j++)
      if (not A.gap(c,nodes[j]))
	return true;
    return false;
  }

  ublas::matrix<int> subA_index_any(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& nodes) 
  {
    // get ordered list of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_simple(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,I) = l++;
      else
	subA(c,I) = alphabet::gap;

    // return processed indices
    return subA_select(subA);
  }


  ublas::matrix<int> subA_index_any(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& nodes, const vector<int>& seq) 
  {
    // get ordered list of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_simple(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size();
    for(int c=0;c<subA.size1();c++) 
      subA(c,I) = alphabet::gap;

    for(int i=0;i<seq.size();i++) 
      subA(seq[i],I) = i;

#ifndef NDEBUG
    // check reqs...
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	assert(subA(c,I)!=alphabet::gap);
      else
	assert(subA(c,I)==alphabet::gap);
#endif

    return subA_select(subA);
  }


  ublas::matrix<int> subA_index_none(const vector<int>& b,const alignment& A,const Tree& T,
				     const vector<int>& nodes) 
  {
    // get ordered list of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_simple(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,I) = alphabet::gap;
      else
	subA(c,I) = l++;

    // return processed indices
    return subA_select(subA);
  }


  /// compute log(probability) from conditional likelihoods (S) and equilibrium frequencies as in MModel
  double Pr(const Matrix& S,const MultiModel& MModel) {
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

    return log(total);
  }


  double calc_root_probability(const alignment& A, const Parameters& P,const vector<int>& rb,
			       const ublas::matrix<int>& index) 
  {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();
    Likelihood_Cache& cache = P.LC;

    const int root = cache.root;

    if (P.T[root].is_leaf_node())
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

    double total = 0;
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
	  if (alphabet::letter(rl))
	    for(int l=0;l<asize;l++)
	      if (l != rl) 
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

      total += log(p_col);
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }

    return total;
  }

  void peel_branch(int b0,column_cache_t cache, const alignment& A, const Tree& T, 
		   const MatCache& transition_P,const MultiModel& MModel)
  {
    // compute branches-in
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = (b.size())?subA_index(b,A,T):leaf_index(b0,A);

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    const int length = index.size1();

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    for(int i=0;i<length;i++) {

      // compute the distribution at the parent node - single letter
      if (not b.size()) {
	int l2 = index(i,0);
	if (alphabet::letter(l2))
	  for(int m=0;m<n_models;m++) {
	    const Matrix& Q = transition_P[m][b0%B];
	    for(int l1=0;l1<asize;l1++)
	      cache(i,b0)(m,l1) = Q(l1,l2);
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

  int calculate_caches(const alignment& A, const Parameters& P,column_cache_t cache) {
    const Tree& T = P.T;
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T, cache);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],cache,A,T,MC,P.SModel());

    return ops.size();
  }

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const alignment& A,const Parameters& P, const vector<int>& b,
			 const vector<int>& req,const vector<int>& seq)
  {
    const Tree& T = P.T;
    Likelihood_Cache& cache = P.LC;

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    cache.root = root;

    ublas::matrix<int> index = subA_index_any(b,A,T,req,seq);

    int n_br = calculate_caches(A,P,cache);
#ifndef NDEBUG
    std::clog<<"get_column_likelihoods: Peeled on "<<n_br<<" branches.\n";
#endif


    vector<Matrix> L;
    L.reserve(A.length());

    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    for(int i=0;i<index.size1();i++) {

      for(int m=0;m<n_models;m++) {
	for(int l=0;l<asize;l++) 
	  S(m,l) = 1;

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<b.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= cache(i0,b[j])(m,l);
	}

	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (alphabet::letter(rl))
	    for(int l=0;l<asize;l++)
	      if (l != rl) 
		S(m,l) = 0;
	}
      }
      L.push_back(S);
    }
    return L;
  }

  double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes) 
  {
    const Tree& T = P.T;
    Likelihood_Cache& cache = P.LC;

    int n_br = calculate_caches(A,P,cache);
#ifndef NDEBUG
    std::clog<<"other_subst: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[cache.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index1 = subA_index_none(rb,A,T,nodes);
    double Pr1 = calc_root_probability(A,P,rb,index1);

#ifndef NDEBUG
    ublas::matrix<int> index2 = subA_index_any(rb,A,T,nodes);
    ublas::matrix<int> index  = subA_index(rb,A,T);

    double Pr2 = calc_root_probability(A,P,rb,index2);
    double Pr  = calc_root_probability(A,P,rb,index);

    assert(std::abs(Pr1 + Pr2 - Pr) < 1.0e-9);
#endif

    return Pr1;
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& cache) {
    const Tree& T = P.T;

    if (cache.cv_up_to_date()) {
#ifndef NDEBUG
      std::clog<<"Pr: Using cached value "<<cache.cached_value<<"\n";
#endif
      return cache.cached_value;
    }

    int n_br = calculate_caches(A,P,cache);
#ifndef NDEBUG
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[cache.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = subA_index(rb,A,T);

    // get the probability
    double Pr = calc_root_probability(A,P,rb,index);

    cache.cached_value = Pr;
    cache.cv_up_to_date() = true;

    return Pr;
  }

  double Pr(const alignment& A,const Parameters& P) {
    double result = Pr(A, P, P.LC);

#ifdef DEBUG_CACHING
    Parameters P2 = P;
    P2.LC.invalidate_all();
    double result2 = Pr(A, P2, P2.LC);
    if (std::abs(result - result2)  > 1.0e-9) {
      std::cerr<<"Pr: diff = "<<result-result2<<std::endl;
      std::abort();
    }
#endif

    return result;
  }
}
