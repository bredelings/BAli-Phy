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
  void calc_root_likelihoods(const vector<int>& residues, 
			     vector<RefMatrix>& distributions,
			     const peeling_info& pi) 
  {
    const int scratch = distributions.size()-1;

    const int n_models = distributions[scratch].size1();

    const int asize = distributions[scratch].size2();

    for(int m=0;m<n_models;m++) {

      //-------------- Propagate and collect information at 'root' -----------//
      for(int l=0;l<asize;l++)
	distributions[scratch](m,l) = distributions[pi.rb1](m,l);

      if (pi.rb2 != -1)
	for(int l=0;l<asize;l++)
	  distributions[scratch](m,l) *= distributions[pi.rb2](m,l);

      if (pi.rb3 != -1)
	for(int l=0;l<asize;l++)
	  distributions[scratch](m,l) *= distributions[pi.rb3](m,l);

      //-------------- Take into account letters at 'root' -------------//
      //FIXME - we could avoid calculations for other letters...
      if (alphabet::letter(residues[pi.root]))
	for(int l=0;l<asize;l++)
	  if (l != residues[pi.root])
	    distributions[scratch](m,l) = 0;
    }
  }


  /// Peel along each branch in work-list @branches 
  inline void peel(const peeling_info& branches,
		   vector< RefMatrix> & distributions,
		   const vector<int>& residues,
		   const MatCache& transition_P)
  {
    

    // The number of directed branches is twice the number of undirected branches
    const int B        = distributions.size()/2;
    const int n_models = distributions[0].size1();
    const int asize    = distributions[0].size2();

    // record if this distribution is just '*'
    valarray<bool> uninformative(false,2*B);  // how much speedup does this give?

    for(int i=0;i<branches.size();i++) {

      // Get info 
      int b     = branches[i].b;     // directed branch from source -> target
      int b1    = branches[i].b1;    // directed branch from n1     -> source, -1 if leaf(source)
      int b2    = branches[i].b2;    // directed branch from n2     -> source, -1 if leaf(source)
      int source = branches[i].source;   // = T.directed_branch(b).source();


      // compute the distribution at the target (parent) node - single letter
      if (b1 < 0 and alphabet::letter(residues[source])) 
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b%B];
	  for(int i=0;i<asize;i++)
	    distributions[b](m,i) = Q(i,residues[source]);
	}

      // compute the distribution at the target (parent) node - wildcard
      else if (b1 < 0 or (uninformative[b1] and uninformative[b2])) {
	for(int m=0;m<n_models;m++) 
	  for(int i=0;i<asize;i++)
	    distributions[b](m,i) = 1.0;
	uninformative[b] = true;
      }

      // cache the source distribution, or not?
      else {
	const int scratch = distributions.size()-1;

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

	  for(int j=0;j<asize;j++)
	    distributions[scratch](m,j) = distributions[b1](m,j)*distributions[b2](m,j);

	  const Matrix& Q = transition_P[m][b%B];

	  // compute the distribution at the target (parent) node - multiple letters
	  for(int i=0;i<asize;i++) {
	    double temp=0;
	    for(int j=0;j<asize;j++)
	      temp += Q(i,j)*distributions[scratch](m,j);  // cache, or no cache?
	    distributions[b](m,i) = temp;
	  }
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
  Matrix
  get_column_likelihoods(const vector<int>& residues,const Tree& T,const MultiModel& MModel,
			 const MatCache& transition_P,int root) 
  {
    //------ Allocate space and mark all branches out of date -------//
    vector<RefMatrix> distributions = Likelihood_Cache(T,MModel).unshareable_column();

    vector<bool> up_to_date(2*T.n_branches(),false);

    //----------- determine the operations to perform -----------------//
    peeling_info branches = get_branches(T,root,up_to_date);
    
    //-------- propagate info along branches ---------//
    peel(branches,distributions,residues,transition_P);

    //----------- return the result ------------------//
    const int scratch = distributions.size()-1;

    return distributions[scratch];
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



  double Pr(const vector<int>& residues, const peeling_info& branches, const MultiModel& MModel,
	    const MatCache& transition_P, vector<RefMatrix> distributions)
  {
    const alphabet& a = MModel.Alphabet();

    //-------- propagate info along branches ---------//
    peel(branches,distributions,residues,transition_P);

    const int scratch = distributions.size()-1;

    double total = 0;
    for(int m=0;m<MModel.nmodels();m++) {
      double p = 0;
      for(int l=0;l<a.size();l++)
	p += distributions[scratch](m,l) * MModel.get_model(m).frequencies()[l];

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
    vector<RefMatrix> distributions = Likelihood_Cache(T,MModel).unshareable_column();

    //---------- determine the operations to perform ----------------//
    vector<bool> up_to_date(2*T.n_branches(),false);
    int root = T.n_nodes()-1;
    peeling_info operations = get_branches(T,root,up_to_date);
    
    //---------------- sum the column likelihoods -------------------//
    return Pr(residues,operations,MModel,MC,distributions);
  }

  double Pr(const alignment& A, const Parameters& P,Likelihood_Cache& L) {
    const Tree& T = P.T;
    const MultiModel& MModel = P.SModel();
    const MatCache& MC = P;

    //---------- determine the operations to perform ----------------//
    int root = L.root;
    peeling_info operations = get_branches(T,root,L.up_to_date);
    
    //---------------- sum the column likelihoods -------------------//
    vector<int> residues(A.size2());
  
    double total = 0.0;
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);
      double p = Pr(residues,operations,MModel,MC,L[column]);

#ifndef NDEBUG
      {
	vector<RefMatrix> distributions = Likelihood_Cache(T,MModel).unshareable_column();

	int node = myrandom(0,T.n_nodes());
	vector<bool> up_to_date(2*T.n_branches(),false);
	peeling_info operations2 = get_branches(T,node,up_to_date);
	double p2 = Pr(residues,operations2,MModel,MC,distributions);
	
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

    for(int i=0;i<operations.size();i++)
      L.up_to_date[operations[i].b] = true;

    //std::cerr<<"Peeled on "<<operations.size()<<" branches.\n";
    //std::cerr<<" substitution: P="<<P<<std::endl;
    return total;
  }

  double Pr(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC) {
    //------ Allocate space and mark all branches out of date -------//
    vector<RefMatrix> distributions = Likelihood_Cache(T,MModel).unshareable_column();

    //---------- determine the operations to perform ----------------//
    vector<bool> up_to_date(2*T.n_branches(),false);
    int root = T.n_nodes()-1;
    peeling_info operations = get_branches(T,root,up_to_date);
    
    //---------------- sum the column likelihoods -------------------//
    vector<int> residues(A.size2());

    double p = 0.0;
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);
      p += Pr(residues,operations,MModel,MC,distributions);
    }

    //    std::cerr<<" substitution: P="<<P<<std::endl;
    return p;
  }

  double Pr(const alignment& A,const Parameters& P) {
    double result = Pr(A,P,P.LC);
#ifndef NDEBUG
    Parameters P2 = P;
    P2.recalc();
    double result2 = Pr(A,P.T,P.SModel(),P2);
    assert(result == result2);
#endif
    return result;
  }


}
