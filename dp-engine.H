#ifndef DPMATRIX_H
#define DPMATRIX_H

#include "hmm.H"

class DPengine: public HMM {
public:
  /// Sample a path from the HMM
  virtual vector<int> sample_path() const =0;

  /// Compute the probability of all possible paths through theHMM
  virtual double Pr_sum_all_paths() const =0;

  /// Calculates the (log) probability that this path is sampled by the generalized HMM
  virtual double path_P(const vector<int>& g_path) const =0;

  /// Calculate the (log) substitution probability along the path
  virtual double path_Q_subst(const vector<int>& g_path) const=0;

  /// Calculate the (log) substitution probability of this path
  virtual double path_Q(const vector<int>& g_path) const {
    return path_Q_path(g_path) + this->path_Q_subst(g_path);
  }

  /// Check of the relative probabilities of path1 and path2 agree with lp1+ls1 and lp2+ls2
  double check(const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2) const;

  DPengine(const vector<int>&,const vector<double>&,const Matrix&);
};



/// 1D Dynamic Programming Matrix
class DParray : public DPengine, public vector< vector<double> > {
  /// Length of the DP array
  int length;
public:
  double path_Q_subst(const vector<int>& path) const {return 0;}

  /// Does state S emit?
  bool di(int S) const {bool e = false; if (state_emit[S]&(1<<0)) e=true;return e;}

  /// Return the size of the array
  unsigned size() const {return length;}

  double path_P(const vector<int>& path) const;

  /// compute FP for the whole array
  void forward();

  /// compute FP for a cell
  virtual void forward(int)=0;

  /// Sample a path from the HMM
  vector<int> sample_path() const;

  /// Compute the probability of all possible paths through theHMM
  double Pr_sum_all_paths() const;

  /// Construct an 1D DP matrix from a length, and an HMM
  DParray(int,const vector<int>&,const vector<double>&,const Matrix&);

  virtual ~DParray() {}
};



/// 1D Dynamic Programming Matrix - with constraints on which states are allowed at each position
class DParrayConstrained: public DParray {
  int order_of_computation() const;
  /// The list of allowed states for each position
  vector< vector<int> > allowed_states;
public:
  /// Access the states allowed at position j
  const vector<int>& states(int j) const {return allowed_states[j];}
  /// Access the states allowed at position j
  vector<int>& states(int j) {return allowed_states[j];}

  /// Compute the forward probabilities for the entire array
  void forward();

  /// Compute the forward probabilities for one cell
  void forward(int);

  void prune();

  /// Construct an 1D DP matrix from a length, and an HMM
  DParrayConstrained(int,const vector<int>&,const vector<double>&,const Matrix&);

  virtual ~DParrayConstrained() {}
};



/// 2D Dynamic Programming Matrix
class DPmatrix : public DPengine, public vector<Matrix> {
  /// Size of first dimension: 0...L1 = 0...S1-1
  int S1;
  /// Size of second dimension: 0...L2 = 0...S2-1
  int S2;
public:
  /// Access size of dim 1
  int size1() const {return S1;}
  /// Access size of dim 2
  int size2() const {return S2;}

  /// Does state S emit in dimension 1?
  bool di(int S) const {bool e = false; if (state_emit[S]&(1<<0)) e=true;return e;}
  /// Does state S emit in dimension 2?
  bool dj(int S) const {bool e = false; if (state_emit[S]&(1<<1)) e=true;return e;}

  /// Compute the forward probabilities for a square
  virtual void forward_cell(int,int,int,int)=0;

  /// Compute the forward probabilities for a square
  void forward_square(int,int,int,int);
  void forward_square();

  /// Compute the forward probabilities for a square
  void forward_band(int);

  /// compute FP for entire matrix, with some points on path pinned
  void forward_pinned(const vector<int>&,double);

  /// Compute the forward probabilities
  vector<int> forward(int,int,const vector<int>&);

  /// Sample a path from the HMM
  vector<int> sample_path() const;

  double path_P(const vector<int>& path) const;

  /// Auxiliary method for computing probability of sampling a path
  double path_check(const vector<int>& path) const;

  /// compute the probability of all possible paths through the HMM
  double Pr_sum_all_paths() const;

  /// Construct a 2D DP matrix from the dimensions, and an HMM
  DPmatrix::DPmatrix(int i1,
		     int i2,
		     const vector<int>& v1,
		     const vector<double>& v2,
		     const Matrix& M);
  virtual ~DPmatrix() {}
};

int bandwidth(const DPmatrix&,const vector<int>&);
int bandwidth2(const DPmatrix&,const vector<int>&);


/// 2D Dynamic Programming Matrix for chains which only emit or don't emit
class DPmatrixNoEmit: public DPmatrix {
public:
  /// Compute the forward probabilities for a cell
  void forward_cell(int,int,int,int);

  double path_Q_subst(const vector<int>& path) const {return 0;}

  DPmatrixNoEmit::DPmatrixNoEmit(int i1,
		     int i2,
		     const vector<int>& v1,
		     const vector<double>& v2,
		     const Matrix& M)
    :DPmatrix(i1,i2,v1,v2,M) 
  {}

  virtual ~DPmatrixNoEmit() {}
};



/// 2D Dynamic Programming Matrix for chains which emit different things
class DPmatrixEmit : public DPmatrix {
protected:
  /// Precomputed emission probabilies for +-
  valarray<double> s1_sub;
  /// Precomputed emission probabilies for -+
  valarray<double> s2_sub;

public:
  /// Probabilities of the different rates
  vector<double> distribution;
  /// Emission probabilities for first sequence
  vector< vector< valarray<double> > > dists1;
  /// Emission probabilities for second sequence
  vector< vector< valarray<double> > > dists2;
  /// Frequencies at the root node - and equilibrium frequencies
  valarray<double> frequency;
  /// The number of different rates
  int nrates() const {return dists1[0].size();}

  double path_Q_subst(const vector<int>& path) const;

  /// Emission probabilities for ++
  double emitMM(int i,int j) const;
  /// Emission probabilities for -+
  double emit_M(int i,int j) const;
  /// Emission probabilities for +-
  double emitM_(int i,int j) const;
  /// Emission probabilities for --
  double emit__(int i,int j) const;

  /// Construct a DP array from an HMM, emission probabilities, and substitution model
  DPmatrixEmit::DPmatrixEmit(const vector<int>&,
			     const vector<double>&,
			     const Matrix&,
			     const vector< double >&,
			     const vector< vector< valarray<double> > >&,
			     const vector< vector< valarray<double> > >&, 
			     const valarray<double>&);
  
  virtual ~DPmatrixEmit() {}
};



/// 2D Dynamic Programming matrix with no constraints on states at each cell
class DPmatrixSimple: public DPmatrixEmit {
public:
  void forward_cell(int,int,int,int);

  DPmatrixSimple(const vector<int> & v1,
		 const vector<double> & v2,
		 const Matrix& M,
		 const vector< double >& d0,
		 const vector< vector< valarray<double> > >& d1,
		 const vector< vector< valarray<double> > >& d2, 
		 const valarray<double>& f):
    DPmatrixEmit(v1,v2,M,d0,d1,d2,f)
  { }

  virtual ~DPmatrixSimple() {}
};



/// Dynamic Programming matrix with constraints on the states
class DPmatrixConstrained: public DPmatrixEmit {
  int order_of_computation() const;
  vector< vector<int> > allowed_states;
public:
  const vector<int>& states(int j) const {return allowed_states[j];}
  vector<int>& states(int j) {return allowed_states[j];}

  void forward_cell(int,int,int,int);

  void prune();

  DPmatrixConstrained(const vector<int> & v1,
	      const vector<double> & v2,
	      const Matrix& M,
	      const vector< double >& d0,
	      const vector< vector< valarray<double> > >& d1,const vector< vector< valarray<double> > >& d2, 
	      const valarray<double>& f):
    DPmatrixEmit(v1,v2,M,d0,d1,d2,f), allowed_states(d2.size()+1)
  { }

  virtual ~DPmatrixConstrained() {}
};

#endif
