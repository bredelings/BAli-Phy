#ifndef DPMATRIX_H
#define DPMATRIX_H

#include "hmm.H"
#include <valarray>


class state_array
{
  int s1;
  int s2;
  int s3;

  //CHECK - does valarray make this faster?
  valarray<double> data;
  vector<int> scale_;

public:
  int size1() const {return s1;}
  int size2() const {return s2;}

  double& operator()(int i,int j) {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    return data[i*s2+j];
  }

  double operator()(int i,int j) const {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    return data[i*s2+j];
  }

  int& scale(int i) {
    assert(0 <= i and i < s1);
    return scale_[i];
  }

  int scale(int i) const {
    assert(0 <= i and i < s1);
    return scale_[i];
  }

  state_array(int i1,int i2)
    :s1(i1),s2(i2),
     data(i1*i2),
     scale_(i1)
  {}
};


class state_matrix
{
  int s1;
  int s2;
  int s3;

  //CHECK - does valarray make this faster?
  valarray<double> data;
  vector<int> scale_;
public:
  int size1() const {return s1;}
  int size2() const {return s2;}
  int size3() const {return s3;}

  double& operator()(int i,int j,int k) {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    assert(0 <= k and k < s3);
    return data[s3*(i*s2+j)+k];
  }

  double operator()(int i,int j,int k) const {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    assert(0 <= k and k < s3);
    return data[s3*(i*s2+j)+k];
  }

  int& scale(int i,int j) {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    return scale_[i*s2 + j];
  }

  int scale(int i,int j) const {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    return scale_[i*s2 + j];
  }

  state_matrix(int i1,int i2,int i3)
    :s1(i1),s2(i2),s3(i3),
     data(i1*i2*i3),
     scale_(s1*s2)
  {}
};



class DPengine: public HMM {
public:

  /// Sample a path from the HMM
  virtual vector<int> sample_path() const =0;

  /// Calculates the (log) probability of all possible paths through theHMM
  virtual efloat_t Pr_sum_all_paths() const =0;

  /// Calculates the (log) probability that this path is sampled by the generalized HMM
  virtual efloat_t path_P(const vector<int>& g_path) const =0;

  /// Calculate the (log) substitution probability along the path
  virtual efloat_t path_Q_subst(const vector<int>& g_path) const=0;

  /// Calculate the (log) probability of this path
  virtual efloat_t path_Q(const vector<int>& g_path) const {
    return path_GQ_path(g_path) + this->path_Q_subst(g_path);
  }

  DPengine(const vector<int>&,const vector<double>&,const Matrix&,double Temp);
};



/// 1D Dynamic Programming Matrix
class DParray : public DPengine, public state_array {
  /// Length of the DP array
  int length;
public:
  efloat_t path_Q_subst(const vector<int>& path) const {return 1;}

  /// Does state S emit?
  bool di(int S) const {bool e = false; if (state_emit[S]&(1<<0)) e=true;return e;}

  /// Return the size of the array
  unsigned size() const {return length;}

  efloat_t path_P(const vector<int>& path) const;

  /// compute FP for the whole array
  void forward();

  /// compute FP for a cell
  virtual void forward(int)=0;

  /// Sample a path from the HMM
  vector<int> sample_path() const;

  /// Compute the probability of all possible paths through theHMM
  efloat_t Pr_sum_all_paths() const;

  /// Construct an 1D DP matrix from a length, and an HMM
  DParray(int,const vector<int>&,const vector<double>&,const Matrix&,double Temp);

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
  DParrayConstrained(int,const vector<int>&,const vector<double>&,const Matrix&, double Temp);

  virtual ~DParrayConstrained() {}
};



/// 2D Dynamic Programming Matrix
class DPmatrix : public DPengine, public state_matrix {
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
  void forward_constrained(const vector<vector<int> >&);

  /// Sample a path from the HMM
  vector<int> sample_path() const;

  /// Compute the forward probabilities and back sample
  vector<int> forward(const vector<vector<int> >&);

  efloat_t path_P(const vector<int>& path) const;

  /// Auxiliary method for computing probability of sampling a path
  efloat_t path_check(const vector<int>& path) const;

  /// compute the probability of all possible paths through the HMM
  efloat_t Pr_sum_all_paths() const;

  /// Construct a 2D DP matrix from the dimensions, and an HMM
  DPmatrix::DPmatrix(int i1,
		     int i2,
		     const vector<int>& v1,
		     const vector<double>& v2,
		     const Matrix& M,
		     double Temp);
  virtual ~DPmatrix() {}
};

int bandwidth(const DPmatrix&,const vector<int>&);
int bandwidth2(const DPmatrix&,const vector<int>&);


/// 2D Dynamic Programming Matrix for chains which only emit or don't emit
class DPmatrixNoEmit: public DPmatrix {
public:
  /// Compute the forward probabilities for a cell
  void forward_cell(int,int,int,int);

  efloat_t path_Q_subst(const vector<int>& path) const {return 1;}

  DPmatrixNoEmit::DPmatrixNoEmit(int i1,
				 int i2,
				 const vector<int>& v1,
				 const vector<double>& v2,
				 const Matrix& M,
				 double Temp
				 )
    :DPmatrix(i1,i2,v1,v2,M,Temp) 
  {}

  virtual ~DPmatrixNoEmit() {}
};



/// 2D Dynamic Programming Matrix for chains which emit different things
class DPmatrixEmit : public DPmatrix {
protected:

  Matrix s12_sub;
  /// Precomputed emission probabilies for +-
  std::vector<double> s1_sub;
  /// Precomputed emission probabilies for -+
  std::vector<double> s2_sub;

public:
  /// Probabilities of the different rates
  vector<double> distribution;
  /// Emission probabilities for first sequence
  vector< Matrix > dists1;
  /// Emission probabilities for second sequence
  vector< Matrix > dists2;
  /// Frequencies at the root node - and equilibrium frequencies
  Matrix frequency;
  /// The number of different rates
  int nrates() const {return dists1[0].size1();}

  efloat_t path_Q_subst(const vector<int>& path) const;

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
			     double Temp,
			     const vector< double >&,
			     const vector< Matrix >&,
			     const vector< Matrix >&, 
			     const Matrix&);
  
  virtual ~DPmatrixEmit() {}
};



/// 2D Dynamic Programming matrix with no constraints on states at each cell
class DPmatrixSimple: public DPmatrixEmit {
public:
  void forward_cell(int,int,int,int);

  DPmatrixSimple(const vector<int> & v1,
		 const vector<double> & v2,
		 const Matrix& M,
		 double Temp,
		 const vector< double >& d0,
		 const vector< Matrix >& d1,
		 const vector< Matrix >& d2, 
		 const Matrix& f):
    DPmatrixEmit(v1,v2,M,Temp,d0,d1,d2,f)
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
		      double Temp,
		      const vector< double >& d0,
		      const vector< Matrix >& d1,
		      const vector< Matrix >& d2, 
		      const Matrix& f):
    DPmatrixEmit(v1,v2,M,Temp,d0,d1,d2,f), allowed_states(d2.size()+1)
  { }

  virtual ~DPmatrixConstrained() {}
};

#endif
