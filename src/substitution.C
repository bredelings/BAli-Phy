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
      total += p;
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

    // compute source node for root branches
    vector<int> source;
    for(int i=0;i<rb.size();i++)
      source.push_back(T.directed_branch(rb[i]).source());
    source.push_back(root);

    // declare the index for the nodes
    vector<int> index(source.size(),-1);

    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    for(int c=0;c<A.length();c++) {

      // compute indices, and skip column if no relevant nodes present
      if (not inc(index,source,A,c)) continue;

      Matrix & S = cache.scratch(index.back());

      for(int m=0;m<n_models;m++) {
	for(int l=0;l<asize;l++) 
	  S(m,l) = 1;

	//-------------- Propagate and collect information at 'root' -----------//
	for(int i=0;i<rb.size();i++) 
	  if (not A.gap(c,source[i]))
	    for(int l=0;l<asize;l++) 
	      S(m,l) *= cache(index[i],rb[i])(m,l);

	//-------------- Take into account letters at 'root' -------------//
	//FIXME - we could avoid calculations for other letters...
	if (alphabet::letter(A(c,root)))
	  for(int l=0;l<asize;l++)
	    if (l != A(c,root))
	      S(m,l) = 0;
      }
    }
  }

  double calc_root_probability(const alignment& A, const Tree& T,column_cache_t cache,
			       const MultiModel& MModel) 
  {
    calc_root_likelihoods(A,T,cache);

    int root = cache.root;

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // compute source node for root branches
    vector<int> source;
    for(int i=0;i<rb.size();i++)
      source.push_back(T.directed_branch(rb[i]).source());
    source.push_back(root);

    // declare the index for the nodes
    vector<int> index(source.size(),-1);

    double total = 0;
    for(int c=0;c<A.length();c++) {

      // compute indices, and skip column if no relevant nodes present
      if (not inc(index,source,A,c)) continue;

      total += Pr(cache.scratch(index.back()),MModel);
    }
    for(int i=0;i<rb.size();i++)
      total += cache.sum(rb[i]);

    return total;
  } 


  void peel_branch(int b0,column_cache_t cache, const alignment& A, const Tree& T, 
		   const MatCache& transition_P,const MultiModel& MModel)
  {
    double total = 0;
    int s0 = T.directed_branch(b0).source();

    // compute branches-in
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    // compute source nodes for branches-in
    vector<int> source;
    for(int i=0;i<b.size();i++)
      source.push_back(T.directed_branch(b[i]).source());
    source.push_back(T.directed_branch(b0).source());

    // declare the index for the nodes
    vector<int> index(source.size(),-1);

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();
    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    for(int c=0;c<A.length();c++) {

      // increment indices and bail if nothing is present in this column
      if (not inc(index,source,A,c)) continue;

      // if the target not is '-', then also bail
      if (A.gap(c,s0)) {
	for(int i=0;i<source.size()-1;i++)
	  if (not A.gap(c,source[i])) {
	    total += Pr(cache(index[i],b[i]),MModel);
	    break;
	    // there had BETTER not be more than one + connected to a -!
	  }
	continue;
      }

      // compute the distribution at the parent node - single letter
      if (not b.size() and alphabet::letter(A(c,s0))) 
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  for(int i=0;i<asize;i++)
	    cache(index.back(),b0)(m,i) = Q(i,A(c,source.back()));
	}

      // compute the distribution at the target (parent) node - wildcard
      else if (b.size() == 0) {
	for(int m=0;m<n_models;m++) 
	  for(int i=0;i<asize;i++)
	    cache(index.back(),b0)(m,i) = 1.0;
      }

      // compute the distribution at the target (parent) node - 2 branch distributions
      else if (b.size() == 1 and false)
	; // compute the source distribution from model-switching matrix
      else if (b.size() == 2) 
      {
	// compute the source distribution from 2 branch distributions
	Matrix& S = cache.scratch(index.back());
	if (not A.gap(c,source[0]) and not A.gap(c,source[1]))
	  for(int m=0;m<n_models;m++) 
	    for(int j=0;j<asize;j++)
	      S(m,j) = cache(index[0],b[0])(m,j)* cache(index[1],b[1])(m,j);
	else if (not A.gap(c,source[0]))
	  S = cache(index[0],b[0]);
	else if (not A.gap(c,source[1]))
	  S = cache(index[1],b[1]);
	else
	  for(int m=0;m<n_models;m++) 
	    for(int j=0;j<asize;j++)
	      S(m,j) = 1.0;

	// propagate from the source distribution
	Matrix& R = cache(index.back(),b0);            //name result matrix
	for(int m=0;m<n_models;m++) {

	  // FIXME!!! - switch order of MatCache to be MC[b][m]
	  const Matrix& Q = transition_P[m][b0%B];

	  // compute the distribution at the target (parent) node - multiple letters
	  for(int i=0;i<asize;i++) {
	    double temp=0;
	    for(int j=0;j<asize;j++)
	      temp += Q(i,j)*S(m,j);
	    R(m,i) = temp;
	  }
	}
	
      }
      else
	std::abort();

    }
    for(int i=0;i<b.size();i++)
      total += cache.sum(b[i]);
    cache.sum(b0) = total;
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
  get_column_likelihoods(const alignment& A,const Parameters& P, const vector<int>& b,bool flag)
  {
    const Tree& T = P.T;
    const MatCache& MC = P;
    const MultiModel& MModel = P.SModel();
    Likelihood_Cache& cache = P.LC;

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    cache.root = root;

    // get sources
    vector<int> source(b.size());
    for(int i=0;i<source.size();i++) 
      source[i] = T.directed_branch(b[i]).source();
    if (flag) source.push_back(root);

    vector<int> index(source.size(),-1);

    //----------- determine the operations to perform -----------------//
    peeling_info ops = get_branches(T,cache);
    
    //-------- propagate info along branches ---------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],cache,A,T,MC,MModel);

    vector<Matrix> L;
    L.reserve(A.length());

    const int M = cache.scratch(0).size1();
    const int asize = cache.scratch(0).size2();

    for(int c=0;c<A.length();c++) {
      if (not inc(index,source,A,c)) continue;

      // if the ROOT not is the criterion, then bail when its missing
      if (flag and A.gap(c,root)) continue;

      Matrix S(M,asize);
      for(int i=0;i<S.size1();i++)
	for(int j=0;j<S.size2();j++)
	  S(i,j) = 1;

      for(int i=0;i<b.size();i++)
	if (not A.gap(c,source[i]))
	  for(int j=0;j<S.size1();j++)
	    for(int k=0;k<S.size2();k++)
	      S(j,j) *= cache(index[i],b[i])(j,k);


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
    const column_cache_t cache = P.LC;
    calculate_caches(A,P,cache);

    double p1 = 0;

    // compute the leaf branches for the subtree
    vector<int> branches;
    for(int i=0;i<nodes.size();i++) {
      vector<int> neighbors;
      append(T[nodes[i]].neighbors(),neighbors);

      int outside = -1;
      int inside = -1;
      for(int j=0;j<neighbors.size();j++)
	if (nodes[neighbors[j]])
	  inside = neighbors[j];
	else
	  outside = neighbors[j];
      if (outside != -1 and inside != -1)
	branches.push_back(T.directed_branch(nodes[i],inside));
    }

    for(int i=0;i<branches.size();i++)
      p1 += cache.sum(branches[i]);

    double p2 = 0;
    for(int column=0;column < A.length();column++) {
      bool present = false;
      for(int i=0;i<nodes.size();i++) {
	if (not A.gap(column,nodes[i]))
	  present = true;
      }
      if (present) continue;
      
      p2 += Pr(A, P, column);
    }

    assert(std::abs(p1 - p2) < 1.0e-9);
    return p1;
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
