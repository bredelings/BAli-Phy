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


  /// A structure which holds all topology info to peel along directed branch 'db'
  struct peeling_branch_info {
    int b;
    int b1;
    int b2;
    int source;

    peeling_branch_info(const const_branchview& db)
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
    vector<int> rb;

    int root;

    int B;
    int M;
    int A;

    peeling_info(const Tree&T, const Likelihood_Cache& LC)
      :root(LC.root),
       B(LC.n_branches()),
       M(LC.n_models()),
       A(LC.n_letters())
    {
      for(const_in_edges_iterator i = T[root].branches_in();i;i++)
	rb.push_back(*i);

      assert(rb.size()); // We had better have at least one neighbor!

      reserve(T.n_branches());
    }
  };

  typedef alignment::column_t column_t;
  typedef Likelihood_Cache& column_cache_t;

  // FIXME - divide into two versions
  //  a) one version which saves the likelihoods in scratch(column)
  //  b) one version which just compute the probability
  //  c) when we are fast, time which one is faster!

  /// Compute the letter likelihoods at the root
  void calc_root_likelihoods(int root_letter,
			     int c, column_cache_t distributions,
			     const vector<int>& rb) 
  {
    Matrix & RL = distributions.scratch(c);
    const int n_models = RL.size1();
    const int asize    = RL.size2();

    for(int m=0;m<n_models;m++) {
      for(int l=0;l<asize;l++) 
	RL(m,l) = 1;

      //-------------- Propagate and collect information at 'root' -----------//
      for(int i=0;i<rb.size();i++) 
	if (distributions.informative(c,rb[i]))
	  for(int l=0;l<asize;l++) 
	    RL(m,l) *= distributions(c,rb[i])(m,l);

      //-------------- Take into account letters at 'root' -------------//
      //FIXME - we could avoid calculations for other letters...
      if (alphabet::letter(root_letter))
	for(int l=0;l<asize;l++)
	  if (l != root_letter)
	    RL(m,l) = 0;
    }
  }

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
    return total;
  }

  double calc_root_probability(int root_letter, int c, column_cache_t distributions,
			       const vector<int>& rb,const MultiModel& MModel) 
  {
    calc_root_likelihoods(root_letter,c,distributions,rb);
    return Pr(distributions.scratch(c),MModel);
  } 


  void peel_branch(int b0,column_cache_t cache, const alignment& A, const Tree& T, 
		   const MatCache& transition_P,const MultiModel& MModel)
  {
    // Get info 
    const int s0 = T.directed_branch(b0).source();
    int b1=-1,b2=-1,s1=-1,s2=-1;
    const_in_edges_iterator i = T.directed_branch(b0).branches_before();
    if (i) {
      b1 = *i;
      s1 = T.directed_branch(b1).source();
      i++;
    }
    
    if (i) {
      b2 = *i;
      s2 = T.directed_branch(b2).source();
      i++;
    }


    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();
    const int n_models = cache.scratch(0).size1();
    const int asize    = cache.scratch(0).size2();

    int i0=-1,i1=-1,i2=-1;

    vector<int> vtemp(1);
    for(int c=0;c<A.length();c++) {

      if (A.gap(c,s0)) {
	if (s1 != -1 and not A.gap(c,s1)) {
	  i1++;
	  vtemp[0] = b1;
	  double Pr = calc_root_probability(alphabet::gap,i1,cache,vtemp,MModel);
	}
	if (s2 != -1 and not A.gap(c,s2)) {
	  i2++;
	  vtemp[0] = b2;
	  double Pr = calc_root_probability(alphabet::gap,i2,cache,vtemp,MModel);
	}
	continue;
      }
      i0++;
      if (s1 != -1 and not A.gap(c,s1)) i1++;
      if (s2 != -1 and not A.gap(c,s2)) i2++;

      // compute the distribution at the target (parent) node - single letter
      if (b1 == -1 and alphabet::letter(A(c,s0))) 
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  for(int i=0;i<asize;i++)
	    cache(i0,b0)(m,i) = Q(i,A(c,s0));
	}

      // compute the distribution at the target (parent) node - wildcard
      else if (b1 == -1) {
	for(int m=0;m<n_models;m++) {
	  for(int i=0;i<asize;i++)
	    cache(i0,b0)(m,i) = 1.0;
	}
      }

      // compute the distribution at the target (parent) node - 2 branch distributions
      else {
	
	if (b2 == -1 and false)
	  // compute the source distribution from model-switching matrix
	;
	else {
	  // compute the source distribution from 2 branch distributions

	  if (b1 != -1 and b2 == -1)
	    cache.scratch(c) = cache(i1,b1);
	  else if (b1 != -1 and b2 != -1)
	    for(int m=0;m<n_models;m++) {
	      for(int j=0;j<asize;j++)
		cache.scratch(c)(m,j) = cache(i1,b1)(m,j)* cache(i2,b2)(m,j);
	    }
	  else
	    assert(b2 == -1);
	}
	
	// propagate from the source distribution
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  // compute the distribution at the target (parent) node - multiple letters
	  for(int i=0;i<asize;i++) {
	    double temp=0;
	    for(int j=0;j<asize;j++)
	      temp += Q(i,j)*cache.scratch(c)(m,j);
	    cache(i0,b0)(m,i) = temp;
	  }
	}
	
      }


    }
  }


  /// Compute an ordered list of branches to process
  inline peeling_info get_branches(const Tree& T, const Likelihood_Cache& LC) 
  {
    //------- Get ordered list of not up_to_date branches ----------///
    peeling_info peeling_operations(T,LC);

    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[LC.root].branches_in(),branches);

    for(int i=0;i<branches.size();i++) {
	const const_branchview& db = branches[i];
	if (not LC.up_to_date(db)) {
	  append(db.branches_before(),branches);
	  peeling_operations.push_back(peeling_branch_info(db));
	}
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  Matrix
  get_column_likelihoods(const column_t& residues,const Tree& T,const MultiModel& MModel,
			 const MatCache& transition_P,int root) 
  {
    //------ Allocate space and mark all branches out of date -------//
    Likelihood_Cache LC(T,MModel,1);
    LC.root = root;

    //----------- determine the operations to perform -----------------//
    peeling_info ops = get_branches(T,LC);
    
    //-------- propagate info along branches ---------//
    //    peel(ops,0,LC,residues,transition_P);
    //    for(int i=0;i<ops.size();i++)
    //      peel_branch(ops,ops[i],0,LC,residues,transition_P);
    //    calc_root_likelihoods(residues[ops.root],0,LC,ops.rb);
    //----------- return the result ------------------//
    return LC.scratch(0);
  }

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  Matrix
  get_column_likelihoods(vector<int> residues,const Tree& T,const MultiModel& MModel,
			 const MatCache& transition_P,int root, const valarray<bool>& group) 
  {
    ublas::matrix<int> M(1,residues.size());
    for(int i=0;i<residues.size();i++)
      if (group[i]) 
	M(0,i) = residues[i];
      else
	M(0,i) = alphabet::not_gap;

    column_t column(M,0);

    return get_column_likelihoods(column,T,MModel,transition_P,root);
  }


  void calculate_caches(const alignment& A, const Parameters& P,column_cache_t cache) {
    const Tree& T = P.T;
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T, cache);

    //-------------- Compute the branch likelihoods -----------------//
    //      peel(ops,column,cache,residues,MC);
    for(int i=0;i<ops.size();i++) {
      peel_branch(ops[i].b,cache,A,T,MC,P.SModel());
      cache.validate_branch(ops[i].b);
    }

    //std::cerr<<"Peeled on "<<ops.size()<<" branches.\n";
  }

  double sum_caches(const alignment& A, const Parameters& P,column_cache_t cache) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();

    peeling_info ops(T,cache); // = get_branches(T, P.LC);
    assert(ops.size() == 0);

    //---------------- Compute the root likelihoods -----------------//
    double total = 0.0;
    for(int column=0;column<A.length();column++) {
      alignment::column_t residues = A.get_column(column);

      double p = calc_root_probability(residues[ops.root],column,cache,ops.rb,MModel);

      // SOME model must be possible
      assert(0 < p and p <= 1.00000000001);
      total += log(p);
    }

    cache.old_value = total;
    return total;
  }

  double Pr(const alignment& A, const Parameters& P, int column) {
    calculate_caches(A,P,P.LC);
    sum_caches(A,P,P.LC);

    //---------------- sum the column likelihoods -------------------//
    return Pr(P.LC.scratch(column),P.SModel());
    //A.get_column(column),column,ops,MModel,MC,LC);
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
