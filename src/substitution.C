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

  /// Compute the letter likelihoods at the root
  void calc_root_likelihoods(const column_t& residues,
			     int c, column_cache_t distributions,
			     const peeling_info& ops) 
  {
    const int n_models = ops.M;
    const int asize    = ops.A;

    for(int m=0;m<n_models;m++) {
      for(int l=0;l<asize;l++) 
	distributions.scratch(c)(m,l) = 1;

      //-------------- Propagate and collect information at 'root' -----------//
      for(int i=0;i<ops.rb.size();i++) 
	if (distributions.informative(c,ops.rb[i]))
	  for(int l=0;l<asize;l++) 
	    distributions.scratch(c)(m,l) *= distributions(c,ops.rb[i])(m,l);

      //-------------- Take into account letters at 'root' -------------//
      //FIXME - we could avoid calculations for other letters...
      if (alphabet::letter(residues[ops.root]))
	for(int l=0;l<asize;l++)
	  if (l != residues[ops.root])
	    distributions.scratch(c)(m,l) = 0;
    }
  }


  /// Peel along each branch in work-list @branches 
  void peel(const peeling_info& ops,
	    int c,column_cache_t cache,
	    const column_t& residues,
	    const MatCache& transition_P)
  {
    
    // The number of directed branches is twice the number of undirected branches
    const int B        = ops.B;
    const int n_models = ops.M;
    const int asize    = ops.A;

    for(int i=0;i<ops.size();i++) {

      // Get info 
      const int b      = ops[i].b;      // directed branch from source -> target
      int b1     = ops[i].b1;     // directed branch from n1     -> source, -1 if leaf(source)
      int b2     = ops[i].b2;     // directed branch from n2     -> source, -1 if leaf(source)
      const int source = ops[i].source; // = T.directed_branch(b).source();


      if (not cache.informative(c,b)) continue;

      // compute the distribution at the target (parent) node - single letter
      if (b1 < 0 and alphabet::letter(residues[source])) 
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][ops[i].b%B];
	  for(int i=0;i<asize;i++)
	    cache(c,b)(m,i) = Q(i,residues[source]);
	}

      // compute the distribution at the target (parent) node - wildcard
      else if (b1 < 0) {
	for(int m=0;m<n_models;m++) {
	  for(int i=0;i<asize;i++)
	    cache(c,b)(m,i) = 1.0;
	}
      }

      // compute the distribution at the target (parent) node - 2 branch distributions
      else {

	if (b2<0 and false)
	  // compute the source distribution from model-switching matrix
	  ;
	/*
	  Matrix& M;
	  for(int l=0;l<asize;l++)
	    for(int m1=0;m1<n_models;m1++)
	      for(int m2=0;m2<n_models;m2++)
	        distributions[scratch](m2,l) = cache[b1](m1,l) * M(m1,m2);
	*/
	else {
	  // compute the source distribution from 2 branch distributions
	  assert( cache.informative(c,b1) or cache.informative(c,b2));
	  if (not cache.informative(c,b1)) std::swap(b1,b2);

	  cache.scratch(c) = cache(c,b1);
	  if (cache.informative(c,b2))
	    for(int m=0;m<n_models;m++) {
	      for(int j=0;j<asize;j++)
		cache.scratch(c)(m,j) *= cache(c,b2)(m,j);
	    }
	}

	// propagate from the source distribution
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][ops[i].b%B];
	  // compute the distribution at the target (parent) node - multiple letters
	  for(int i=0;i<asize;i++) {
	    double temp=0;
	    for(int j=0;j<asize;j++)
	      temp += Q(i,j)*cache.scratch(c)(m,j);
	    cache(c,b)(m,i) = temp;
	  }
	}


      }

    }

    //-------------- collect at 'root' ---------------//
    calc_root_likelihoods(residues,c,cache,ops);
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
    peel(ops,0,LC,residues,transition_P);

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



  double Pr(const column_t& residues, int column, const peeling_info& ops, const MultiModel& MModel,
	    const MatCache& transition_P, column_cache_t cache)
  {
    const alphabet& a = MModel.Alphabet();

    //-------- propagate info along branches ---------//
    peel(ops,column,cache,residues,transition_P);

    double total = 0;
    for(int m=0;m<MModel.nmodels();m++) {
      double p = 0;

      const valarray<double>& f = MModel.get_model(m).frequencies();
      for(int l=0;l<a.size();l++)
	p += cache.scratch(column)(m,l) * f[l];

      // A specific model (e.g. the INV model) could be impossible
      assert(0 <= p and p <= 1.00000000001);
      total += p;
    }

    // SOME model must be possible, though
    assert(0 < total and total <= 1.00000000001);

    return log(total);
  }

  double Pr(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC,int column) {
    //------ Allocate space and mark all branches out of date -------//
    Likelihood_Cache LC(T,MModel,1);
    LC.root = T.n_nodes()-1;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T,LC);
    
    //---------------- sum the column likelihoods -------------------//
    return Pr(A.get_column(column),column,ops,MModel,MC,LC);
  }

  vector<const_branchview> branches_toward_from_node(const Tree& T,int n) {
    vector<const_branchview> branches;
    branches.reserve(2*T.n_branches());

    branches = branches_from_node(T,n);
    std::reverse(branches.begin(),branches.end());
    for(int i=0;i<T.n_branches();i++)
      branches.push_back(branches[i]);

    for(int i=0;i<T.n_branches();i++)
      branches[i] = branches[branches.size()-1-i].reverse();

    return branches; 
  }

  ublas::matrix<int> get_SM(const alignment& A,const Tree& T) {
    ublas::matrix<int> SM(A.length(),2*T.n_branches());
    
    vector<const_branchview> branches = branches_toward_from_node(T,T.n_nodes()-1);

    // Compute the sub-alignments
    vector<const_branchview> temp;temp.reserve(2);
    for(int i=0;i<branches.size();i++) {
      int b = branches[i];


      int l=0;
      for(int c=0;c<SM.size1();c++) {
	SM(c,b) = alphabet::gap;

	// for leaf branches fill from the alignment
	if (branches[i].source().is_leaf_node()) {
	  if (not A.gap(c,b))
	    SM(c,b) = l++;
	}

	// for internal branches fill from the previous branches
	else {
	  temp.clear();
	  append(T.directed_branch(b).branches_before(),temp);
	  assert(temp.size() == 2);

	  if (SM(c,temp[0]) != -1 or SM(c,temp[1]) != -1)
	    SM(c,b) = l++;
	}

      }
    }

    return SM;
  }

  ublas::matrix<int> get_fake_SM(int c,const ublas::matrix<int>& SM1) {
    ublas::matrix<int> SM(1,SM1.size2());
    for(int i=0;i<SM.size2();i++) {
      SM(0,i) = SM1(c,i);
      if (SM(0,i) != alphabet::gap)
	SM(0,i) = 0;
    }
    return SM;
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& L) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T,L);
    
    if (not ops.size() and false) {
      //std::cerr<<"Peeled on 0 branches. (cached result)\n";
      return L.old_value;
    }
    ublas::matrix<int> SM = get_SM(A,T);
    L.SM = &SM;

    //---------------- sum the column likelihoods -------------------//
    double total = 0.0;
    for(int column=0;column<A.length();column++) {

      alignment::column_t col = A.get_column(column);
      double p = Pr(col,column,ops,MModel,MC,L);

#ifndef NDEBUG
      // how can we check that a L(column,b) actually goes with column b?

      {
	Likelihood_Cache LC(T,MModel,1);
	LC.root = L.root;//myrandom(0,T.n_nodes());
	ublas::matrix<int> fSM = get_fake_SM(column,SM);
	LC.SM = &fSM;

	peeling_info ops2 = get_branches(T,LC);
	double p2 = Pr(A.get_column(column),0,ops2,MModel,MC,LC);
	
	if (std::abs(p2-p) > 1.0e-9) {
	  for(int i=0;i<T.n_leaves();i++)
	    std::cerr<<MModel.Alphabet().lookup(A(column,i))<<" ";
	  std::cerr<<p<<" "<<p2<<endl;
	  std::abort(); //FIXME - try this check!
	}
      }
#endif
      total += p;

    }

    for(int i=0;i<ops.size();i++)
      L.validate_branch(ops[i].b);
    L.old_value = total;
    L.SM = NULL;

    //std::cerr<<"Peeled on "<<ops.size()<<" branches.\n";
    //std::cerr<<" substitution: P="<<total<<std::endl;
    return total;
  }

  double Pr(const alignment& A,const Parameters& P) {
    //P.LC.invalidate_all();
    double result = Pr(A,P,P.LC);
    return result;
  }


}
