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
  inline bool inc(vector<int>& index,const vector<int>& source, const alignment& A,int c) {
    assert(index.size() == source.size());
    bool all_gaps = true;
    for(int i=0;i<index.size();i++) {
      if (not A.gap(c,source[i])) {
	all_gaps = false;
	index[i]++;
      }
    }
    return not all_gaps;
  }

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

  ublas::matrix<int> get_subA_relationships(const vector<int>& b,const alignment& A,const Tree& T) 
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


  ublas::matrix<int> get_subA_relationships2(const vector<int>& b,const vector<int>& req,
					     const alignment& A,const Tree& T) 
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

  // FIXME - divide into two versions
  //  a) one version which saves the likelihoods in scratch(column)
  //  b) one version which just compute the probability
  //  c) when we are fast, time which one is faster!


  /// Compute the letter likelihoods at the root
  void calc_root_likelihoods(const alignment& A, const Tree& T,column_cache_t cache) 
  {
    int root = cache.root;

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = get_subA_relationships(rb,A,T);

    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    for(int i=0;i<index.size1();i++) {

      Matrix & S = cache.scratch(i);

      for(int m=0;m<n_models;m++) {
	for(int l=0;l<asize;l++) 
	  S(m,l) = 1;

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<rb.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= cache(i0,rb[j])(m,l);
	}

	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (alphabet::letter(rl))
	    for(int l=0;l<asize;l++)
	      if (l != rl) 
		S(m,l) = 0;
	}
      }
    }
  }


  /// Compute the letter likelihoods at the root
  double calc_root_probability(const alignment& A, const Tree& T,column_cache_t cache,
			       const MultiModel& MModel) 
  {
    int root = cache.root;

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = get_subA_relationships(rb,A,T);

    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    // scratch matrix 
    Matrix S(n_models,asize);

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
	for(int l=0;l<asize;l++) 
	  S(m,l) = F(m,l);

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<rb.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= cache(i0,rb[j])(m,l);
	}

	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (alphabet::letter(rl))
	    for(int l=0;l<asize;l++)
	      if (l != rl) 
		S(m,l) = 0;
	}

	for(int l=0;l<asize;l++)
	  p_model += S(m,l);

	// A specific model (e.g. the INV model) could be impossible
	assert(0 <= p_model and p_model <= 1.00000000001);
	p_col += p_model;
      }

      // SOME model must be possible
      assert(0 < p_col and p_col <= 1.00000000001);

      total += log(p_col);
    }
    return total;
  }


  double calc_root_probability2(const alignment& A, const Tree& T,column_cache_t cache,
			       const MultiModel& MModel) 
  {
    calc_root_likelihoods(A,T,cache);

    int root = cache.root;

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = get_subA_relationships(rb,A,T);

    double total = 0;
    for(int i=0;i<index.size1();i++) {

      Matrix & S = cache.scratch(i);

      total += Pr(S,MModel);
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
    ublas::matrix<int> index = get_subA_relationships(b,A,T);

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();
    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    const int length = index.size1()?index.size1():A.seqlength(b0);

    std::cerr<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
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
	Matrix& S = cache.scratch(i);
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
	Pr(R,MModel);
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

  double sum_caches(const alignment& A, const Parameters& P,column_cache_t cache) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();

    assert(get_branches(T, P.LC).size() == 0);

    //---------------- Compute the root likelihoods -----------------//
    double total = calc_root_probability(A,T,cache,MModel);

    cache.old_value = total;
    return total;
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
  get_column_likelihoods(const alignment& A,const Parameters& P, const vector<int>& b,const vector<int>& req)
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

    ublas::matrix<int> index = get_subA_relationships2(b,req,A,T);

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

  double Pr(const alignment& A, const Parameters& P, int column) {
    const column_cache_t cache = P.LC;
    int root = cache.root;

    calculate_caches(A,P,cache);
    sum_caches(A,P,cache);

    vector<int> source;
    for(int i=0;i<A.size2();i++)
      source.push_back(i);

    vector<int> index(source.size(),-1);
    for(int c=0;c <= column;c++)
      inc(index,source,A,c);

    if (not A.gap(column,root)) 
      return Pr(cache.scratch(index[root]),P.SModel());

    vector<const_branchview> branches = branches_toward_node(P.T, P.LC.root);
    std::reverse(branches.begin(),branches.end());
    for(int i=0;i<branches.size();i++) {
      int b = branches[i].source();
      if (not A.gap(column,b))
	return Pr(cache(index[b],b),P.SModel());
    }

    std::abort();
  }

  double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();
    Likelihood_Cache& cache = P.LC;

    calc_root_likelihoods(A,T,cache);

    int root = cache.root;

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = get_subA_relationships(rb,A,T);

    double total = 0;
    for(int i=0;i<index.size1();i++) {

      Matrix & S = cache.scratch(i);

      total += Pr(S,MModel);
    }

    return total;
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& cache) {
    calculate_caches(A,P,cache);
    double total = sum_caches(A,P,cache);

    //std::cerr<<" substitution: P="<<total<<std::endl;
    return total;
  }

  double Pr(const alignment& A,const Parameters& P) {
    //P.LC.invalidate_all();
    double result = Pr(A,P,P.LC);
    return result;
  }
}
