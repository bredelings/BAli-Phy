#ifndef DP_ARRAY_H
#define DP_ARRAY_H

#include <valarray>
#include "dp-engine.H"
#include "refcount.H"

class state_array: public RefObject
{
  int s1;
  int s2;
  int s3;

  valarray<double> data;
  valarray<int> scale_;

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



#endif
