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

  typedef alignment::column_t column_t;
  typedef Likelihood_Cache& column_cache_t;

  /// move increment indexes for nodes 'source' in column 'c' of 'A'
  inline int inc(int& index,const vector<int>& mask, 
		 const alignment& A,int c) 
  {
    int ret = alphabet::gap;
    for(int i=0;i<mask.size();i++)
      if (not A.gap(c,mask[i])) {
	index++;
	ret = index;
	break;
      }
    return ret;
  }

  ublas::matrix<int> subA_index(const vector<int>& b,const alignment& A,const Tree& T) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),b.size());

    // get criteria for being in a sub-A
    vector<vector<int> > leaves;
    for(int i=0;i<b.size();i++) {
      leaves.push_back(vector<int>());
      leaves.back().reserve(A.size2());
      valarray<bool> p = T.partition(T.directed_branch(b[i]).reverse());
      for(int i=0;i<T.n_leaves();i++)
	if (p[i]) leaves.back().push_back(i);
    }
      
    // declare the index for the nodes
    vector<int> index(b.size(),-1);
    int l=0;
    for(int c=0;c<A.length();c++) {
      bool empty=true;
      for(int j=0;j<b.size();j++) {
	subA(l,j) = inc(index[j],leaves[j],A,c);
	if (subA(l,j) != alphabet::gap) empty=false;
      }
      if (not empty) l++;
    }

    //resize the matrix, and send it back...
    ublas::matrix<int> temp(l,b.size());
    for(int i=0;i<temp.size1();i++)
      for(int j=0;j<temp.size2();j++)
	temp(i,j) = subA(i,j);
    
    return temp;
  }


  ublas::matrix<int> subA_index_req(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& req) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),b.size());

    // get criteria for being in a sub-A
    vector<vector<int> > leaves;
    for(int i=0;i<b.size();i++) {
      leaves.push_back(vector<int>());
      leaves.back().reserve(A.size2());
      valarray<bool> p = T.partition(T.directed_branch(b[i]).reverse());
      for(int i=0;i<T.n_leaves();i++)
	if (p[i]) leaves.back().push_back(i);
    }
      
    // declare the index for the nodes
    vector<int> index(b.size(),-1);
    int l=0;
    for(int c=0;c<A.length();c++) {

      // write the indices for sub-alignments into the current column
      for(int j=0;j<b.size();j++)
	subA(l,j) = inc(index[j],leaves[j],A,c);

      // check to see if we have a REQUIRED node here.
      bool empty=true;
      for(int j=0;j<req.size();j++) {
	if (not A.gap(c,req[j])) {
	  empty = false;
	  break;
	}
      }
      if (not empty) l++;
    }

    //resize the matrix, and send it back...
    ublas::matrix<int> temp(l,b.size());
    for(int i=0;i<temp.size1();i++)
      for(int j=0;j<temp.size2();j++)
	temp(i,j) = subA(i,j);
    
    return temp;
  }


  ublas::matrix<int> subA_index_req(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& req, const vector<int>& seq) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),b.size());

    // get criteria for being in a sub-A
    vector<vector<int> > leaves;
    for(int i=0;i<b.size();i++) {
      leaves.push_back(vector<int>());
      leaves.back().reserve(A.size2());
      valarray<bool> p = T.partition(T.directed_branch(b[i]).reverse());
      for(int i=0;i<T.n_leaves();i++)
	if (p[i]) leaves.back().push_back(i);
    }
      
    // declare the index for the nodes
    vector<int> index(b.size(),-1);
    int l=0;
    for(int c=0;c<A.length();c++) {

      // write the indices for sub-alignments into the current column
      for(int j=0;j<b.size();j++)
	subA(c,j) = inc(index[j],leaves[j],A,c);

      // check to see if we have a REQUIRED node here.
      bool empty=true;
      for(int j=0;j<req.size();j++) {
	if (not A.gap(c,req[j])) {
	  empty = false;
	  break;
	}
      }

      if (not empty) l++;
    }
    assert(l == seq.size());

    //resize the matrix, and send it back...
    ublas::matrix<int> temp(l,b.size());
    for(int i=0;i<temp.size1();i++)
      for(int j=0;j<temp.size2();j++) {
	temp(i,j) = subA(seq[i],j);
      }
    return temp;
  }


  ublas::matrix<int> subA_index_other(const vector<int>& b,const alignment& A,const Tree& T,
				      const vector<int>& exclude) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),b.size());

    // get criteria for being in a sub-A
    vector<vector<int> > leaves;
    for(int i=0;i<b.size();i++) {
      leaves.push_back(vector<int>());
      leaves.back().reserve(A.size2());
      valarray<bool> p = T.partition(T.directed_branch(b[i]).reverse());
      for(int i=0;i<T.n_leaves();i++)
	if (p[i]) leaves.back().push_back(i);
    }
      
    // declare the index for the nodes
    vector<int> index(b.size(),-1);
    int l=0;
    for(int c=0;c<A.length();c++) {

      // write the indices for sub-alignments into the current column
      for(int j=0;j<b.size();j++)
	subA(l,j) = inc(index[j],leaves[j],A,c);

      // check to see if we have a REQUIRED node here.
      bool do_exclude=false;
      for(int j=0;j<exclude.size();j++) {
	if (not A.gap(c,exclude[j])) {
	  do_exclude = true;
	  break;
	}
      }
      if (not do_exclude) l++;
    }

    //resize the matrix, and send it back...
    ublas::matrix<int> temp(l,b.size());
    for(int i=0;i<temp.size1();i++)
      for(int j=0;j<temp.size2();j++)
	temp(i,j) = subA(i,j);
    
    return temp;
  }


  //FIXME - cache the frequencies in a matrix, for quick access.

  /// compute log(probability) from conditional likelihoods (S) and equilibrium frequencies as in MModel
  double Pr(const Matrix& S,const MultiModel& MModel) {
    const alphabet& a = MModel.Alphabet();

    double total = 0;
    for(int m=0;m<MModel.nmodels();m++) {
      double p = 0;

      const valarray<double>& f = MModel.get_model(m).frequencies();
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

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    // cache matrix of frequencies
    Matrix F(n_models,asize);
    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.get_model(m).frequencies();
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
      //      std::cerr<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<std::endl;
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
    ublas::matrix<int> index = subA_index(b,A,T);

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int asize    = S.size2();

    const int length = index.size1()?index.size1():A.seqlength(b0);

    //    std::cerr<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    for(int i=0;i<length;i++) {

      // compute the distribution at the parent node - single letter
      if (not b.size()) {
	int l2 = A.seq(b0)[i];
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

  void calculate_caches(const alignment& A, const Parameters& P,column_cache_t cache) {
    const Tree& T = P.T;
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T, cache);
    //std::cerr<<"Peeled on "<<ops.size()<<" branches.\n";

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],cache,A,T,MC,P.SModel());
  }

  /// I could make peel_branch a generate routine which takes 
  //  a) a number of branches, which point to -> vector<Matrix>
  //  b) optionally peels along vector<Matrix> which is the transition matrices for each model
  //  c) writes the result into a vector<Matrix>& which is passed in
  //     c.1) if a list of nodes specified in a vector<int> are present in that column
  //     c.2) otherwise accumulates the column probability to a double& which is passed in....

  // in this case, then I could use peel_branch for
  // a) get_column_likelihoods
  // b) calc_root_likelihoods...

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const alignment& A,const Parameters& P, const vector<int>& b,
			 const vector<int>& req,const vector<int>& seq)
  {
    const Tree& T = P.T;
    Likelihood_Cache& cache = P.LC;

    calculate_caches(A,P,cache);

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    cache.root = root;

    ublas::matrix<int> index = subA_index_req(b,A,T,req,seq);

    calculate_caches(A,P,cache);

    vector<Matrix> L;
    L.reserve(A.length());

    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    for(int i=0;i<index.size1();i++) {

      Matrix S(n_models,asize);

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
    int root = cache.root;

    calculate_caches(A,P,cache);

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index1 = subA_index_other(rb,A,T,nodes);
#ifndef NDEBUG
    ublas::matrix<int> index2 = subA_index_req(rb,A,T,nodes);
    ublas::matrix<int> index  = subA_index(rb,A,T);

    double Pr1 = calc_root_probability(A,P,rb,index1);
    double Pr2 = calc_root_probability(A,P,rb,index2);
    double Pr  = calc_root_probability(A,P,rb,index);

    assert(std::abs(Pr1 + Pr2 - Pr) < 1.0e-9);
#endif

    return Pr1;
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& cache) {
    const Tree& T = P.T;

    //    std::cerr<<" substitution: root = "<<cache.root<<std::endl;
    int root = cache.root;

    calculate_caches(A,P,cache);

    assert(get_branches(T, P.LC).size() == 0);

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = subA_index(rb,A,T);

    //    std::cerr<<"length of subA for root "<<cache.root<<" is "<<index.size1()<<"\n";

    // get the probability
    double Pr = calc_root_probability(A,P,rb,index);

    // cache the value
    cache.old_value = Pr;

    //std::cerr<<" substitution: P="<<Pr<<std::endl;
    return Pr;
  }

  double Pr(const alignment& A,const Parameters& P) {
    double result = Pr(A, P, P.LC);
#ifndef NDEBUG
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
