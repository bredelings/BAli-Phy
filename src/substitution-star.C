#include "substitution.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>


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

  double Pr_star(const vector<int>& column,const Tree& T,const ReversibleModel& SModel,
		 const vector<Matrix>& transition_P) {
    const alphabet& a = SModel.Alphabet();

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

  efloat_t Pr_star(const alignment& A, const Tree& T, const MultiModel& MModel, const MatCache& MC) 
  {
    efloat_t p = 1;
  
    vector<int> residues(A.size2());

    // Do each node before its parent
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);

      double total=0;
      for(int m=0;m<MModel.n_base_models();m++)
	  total += MModel.distribution()[m] * Pr_star(residues,
						      T,
						      MModel.base_model(m),
						      MC.transition_P(m)
						      );

      // we don't get too close to zero, normally
      assert(0 < total and total <= 1.00000000001);

      p *= total;
    }

    return p;
  }

  efloat_t Pr_star(const alignment& A,const Parameters& P) 
  {
    if (P.T.n_leaves() == 2)
      return Pr(A,P);

    return Pr_star(A, P.T, P.SModel(), P);
  }

  efloat_t Pr_star_constant(const alignment& A,const Parameters& P) 
  {
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

  efloat_t Pr_star_estimate(const alignment& A,const Parameters& P) {
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

  efloat_t Pr_unaligned(const alignment& A,const Parameters& P) {
    const alphabet& a = A.get_alphabet();

    vector<double> count(a.size(),0);

    for(int i=0;i<A.num_sequences();i++) {
      for(int column=0;column<A.length();column++) {
	int l = A(column,i);
	if (a.letter(l))
	  count[l]++;
      }
    }
    
    efloat_t total=1;
    for(int l=0;l<count.size();l++) {
      efloat_t p = P.SModel().frequencies()[l];
      total *= pow(p,count[l]);
    }

    return total;
  }
}
