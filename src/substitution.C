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
    int b_loc;
    int b1_loc;
    int b2_loc;
    int source;

    peeling_branch_info(const const_branchview& db,const Likelihood_Cache& LC)
      :b(db),
       b_loc(LC.location(b)),
       b1_loc(-1),
       b2_loc(-1),
       source(db.source())
    {
      b %= LC.n_branches();
      const_in_edges_iterator i = db.branches_before();
      if (i) {
	b1_loc = LC.location(*i);
	i++;
      }

      if (i) {
	b2_loc = LC.location(*i);
	i++;
      }

      assert(not i);
    }
  };

  struct peeling_info: public vector<peeling_branch_info> {
    int rb1_loc;
    int rb2_loc;
    int rb3_loc;

    int root;

    int B;
    int M;
    int A;
    int scratch;

    peeling_info(const Tree&T, const Likelihood_Cache& LC)
      :rb1_loc(-1),rb2_loc(-1),rb3_loc(-1),
       root(LC.root),
       B(LC.n_branches()),
       M(LC.n_models()),
       A(LC.n_letters()),
       scratch(LC.scratch())
    {
      const_in_edges_iterator i = T[root].branches_in();

      assert(i); // We had better have at least one neighbor!

      rb1_loc = LC.location(*i);
      i++;

      if (i) {
	rb2_loc = LC.location(*i);
	i++;
      }

      if (i) {
	rb3_loc = LC.location(*i);
	i++;
      }

      assert(not i);

      reserve(T.n_branches());
    }
  };


  /// Compute the letter likelihoods at the root
  void calc_root_likelihoods(const vector<int>& residues, 
			     vector<Matrix>& distributions,
			     const peeling_info& ops) 
  {
    const int scratch  = ops.scratch;
    const int n_models = ops.M;
    const int asize    = ops.A;

    for(int m=0;m<n_models;m++) {

      //-------------- Propagate and collect information at 'root' -----------//
      for(int l=0;l<asize;l++)
	distributions[scratch](m,l) = distributions[ops.rb1_loc](m,l);

      if (ops.rb2_loc != -1)
	for(int l=0;l<asize;l++)
	  distributions[scratch](m,l) *= distributions[ops.rb2_loc](m,l);

      if (ops.rb3_loc != -1)
	for(int l=0;l<asize;l++)
	  distributions[scratch](m,l) *= distributions[ops.rb3_loc](m,l);

      //-------------- Take into account letters at 'root' -------------//
      //FIXME - we could avoid calculations for other letters...
      if (alphabet::letter(residues[ops.root]))
	for(int l=0;l<asize;l++)
	  if (l != residues[ops.root])
	    distributions[scratch](m,l) = 0;
    }
  }


  /// Peel along each branch in work-list @branches 
  inline void peel(const peeling_info& ops,
		   vector<Matrix>& distributions,
		   const vector<int>& residues,
		   const MatCache& transition_P)
  {
    
    // The number of directed branches is twice the number of undirected branches
    const int n_models = ops.M;
    const int asize    = ops.A;
    const int scratch  = ops.scratch;

    for(int i=0;i<ops.size();i++) {

      // Get info 
      const int b_loc  = ops[i].b_loc;   // directed branch from source -> target
      const int b1     = ops[i].b1_loc;  // directed branch from n1     -> source, -1 if leaf(source)
      const int b2     = ops[i].b2_loc;  // directed branch from n2     -> source, -1 if leaf(source)
      const int source = ops[i].source; // = T.directed_branch(b).source();

      Matrix& DB = distributions[b_loc];

      // compute the distribution at the target (parent) node - single letter
      if (b1 < 0 and alphabet::letter(residues[source])) 
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][ops[i].b];
	  for(int i=0;i<asize;i++)
	    DB(m,i) = Q(i,residues[source]);
	}

      // compute the distribution at the target (parent) node - wildcard
      else if (b1 < 0) {
	for(int m=0;m<n_models;m++) 
	  for(int i=0;i<asize;i++)
	    DB(m,i) = 1.0;
      }

      // compute the distribution at the target (parent) node - 2 branch distributions
      else {

	// compute the source distribution from 2 branch distributions
	for(int m=0;m<n_models;m++) {
	  
	  /*
	  if (b2 < 0) {
	    Matrix& M;
	    for(int l=0;l<asize;l++)
	      for(int m1=0;m1<n_models;m1++)
		for(int m2=0;m2<n_models;m2++)
		  distributions[scratch](m2,l) = distributions[b1](m1,l) * M(m1,m2);
	  }
	  else */

	  Matrix& DS = distributions[scratch];

	  for(int j=0;j<asize;j++)
	    DS(m,j) = distributions[b1](m,j)*distributions[b2](m,j);

	}

	// propagate from the source distribution
	for(int m=0;m<n_models;m++) {
	  Matrix& DS = distributions[scratch];
	  const Matrix& Q = transition_P[m][ops[i].b];
	  // compute the distribution at the target (parent) node - multiple letters
	  for(int i=0;i<asize;i++) {
	    double temp=0;
	    for(int j=0;j<asize;j++)
	      temp += Q(i,j)*DS(m,j);
	    DB(m,i) = temp;
	  }
	}


      }

    }

    //-------------- collect at 'root' ---------------//
    calc_root_likelihoods(residues,distributions,ops);
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
	  peeling_operations.push_back(peeling_branch_info(db,LC));
	}
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  Matrix
  get_column_likelihoods(const vector<int>& residues,const Tree& T,const MultiModel& MModel,
			 const MatCache& transition_P,int root) 
  {
    //------ Allocate space and mark all branches out of date -------//
    Likelihood_Cache LC(T,MModel,1);
    LC.root = root;

    //----------- determine the operations to perform -----------------//
    peeling_info ops = get_branches(T,LC);
    
    //-------- propagate info along branches ---------//
    peel(ops,LC[0],residues,transition_P);

    //----------- return the result ------------------//
    return LC[0][ops.scratch];
  }

  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  Matrix
  get_column_likelihoods(vector<int> residues,const Tree& T,const MultiModel& MModel,
			 const MatCache& transition_P,int root, const valarray<bool>& group) 
  {
    for(int i=0;i<residues.size();i++)
      if (not group[i]) residues[i] = alphabet::not_gap;

    return get_column_likelihoods(residues,T,MModel,transition_P,root);
  }



  double Pr(const vector<int>& residues, const peeling_info& ops, const MultiModel& MModel,
	    const MatCache& transition_P, vector<Matrix>& distributions)
  {
    const alphabet& a = MModel.Alphabet();

    //-------- propagate info along branches ---------//
    peel(ops,distributions,residues,transition_P);

    const int scratch = ops.scratch;

    double total = 0;
    for(int m=0;m<MModel.nmodels();m++) {
      double p = 0;

      const valarray<double>& f = MModel.get_model(m).frequencies();
      for(int l=0;l<a.size();l++)
	p += distributions[scratch](m,l) * f[l];

      // A specific model (e.g. the INV model) could be impossible
      assert(0 <= p and p <= 1.00000000001);
      total += p;
    }

    // SOME model must be possible, though
    assert(0 < total and total <= 1.00000000001);

    return log(total);
  }

  double Pr(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC,int column) {
    vector<int> residues(A.size2());
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
  
    //------ Allocate space and mark all branches out of date -------//
    Likelihood_Cache LC(T,MModel,1);
    LC.root = T.n_nodes()-1;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T,LC);
    
    //---------------- sum the column likelihoods -------------------//
    return Pr(residues,ops,MModel,MC,LC[0]);
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& L) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T,L);
    
    //---------------- sum the column likelihoods -------------------//
    vector<int> residues(A.size2());
  
    double total = 0.0;
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);
      double p = Pr(residues,ops,MModel,MC,L[column]);

#ifndef NDEBUG
      {
	Likelihood_Cache LC(T,MModel,1);
	LC.root = L.root;//myrandom(0,T.n_nodes());

	peeling_info ops2 = get_branches(T,LC);
	double p2 = Pr(residues,ops2,MModel,MC,LC[0]);
	
	if (std::abs(p2-p) > 1.0e-9) {
	  for(int i=0;i<T.n_leaves();i++)
	    std::cerr<<MModel.Alphabet().lookup(residues[i])<<" ";
	  std::cerr<<p<<" "<<p2<<endl;
	  std::abort(); //FIXME - try this check!
	}
      }
#endif
      total += p;

    }

    for(int i=0;i<ops.size();i++)
      L.mark_location_up_to_date(ops[i].b_loc);

    std::cerr<<"Peeled on "<<ops.size()<<" branches.\n";
    //std::cerr<<" substitution: P="<<P<<std::endl;
    return total;
  }

  double Pr(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC) {
    //------ Allocate space and mark all branches out of date -------//
    Likelihood_Cache LC(T,MModel,1);
    LC.root = T.n_nodes()-1;

    //---------- determine the operations to perform ----------------//
    peeling_info operations = get_branches(T,LC);
    
    //---------------- sum the column likelihoods -------------------//
    vector<int> residues(A.size2());

    double p = 0.0;
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);
      p += Pr(residues,operations,MModel,MC,LC[0]);
    }

    //    std::cerr<<" substitution: P="<<P<<std::endl;
    return p;
  }

  double Pr(const alignment& A,const Parameters& P) {
    P.LC.invalidate_all();
    double result = Pr(A,P,P.LC);
#ifndef NDEBUG
    Parameters P2 = P;
    P2.recalc();
    double result2 = Pr(A,P.T,P.SModel(),P2);
    assert(std::abs(result - result2) < 1.0e-9);
#endif
    return result;
  }


}
