#ifndef DPMATRIX_H
#define DPMATRIX_H

#include "mytypes.H"
#include "logsum.H"

using std::valarray;

inline double sum(const valarray<double>& v) {
  return v.sum();
}


class DP {
  // bitmasks of whether states emit features in each sequence
  vector<double> start_P;

  vector<int> state_emit;
  bool silent(int S) {return not state_emit[S];}
  virtual bool silent_network(int S)=0;

  int nstates() const {return state_emit.size()-1;}
  int endstate() const {return nstates();}

  vector<int> generalize(const vector<int>& path);
  double generalize_P(const vector<int>& path, const Matrix& Q);

  double path_Q_path(const vector<int>& path, const Matrix& GQ);
  virtual double path_Q_subst(const vector<int>& path, const Matrix& GQ)=0;

  virtual double path_Q(const vector<int>& path, const Matrix& GQ) {
    return path_Q_path(path,GQ) + this->path_Q_subst(path,GQ);
  }

  DP(const vector<double&> v1,const vector<double>& v2):start_P(v2),state_emit(v2) 
  {
    assert(start_P.size() == nstates());
  }
};


class DParray : public DP, public vector< vector<double> > {
  vector< vector<int> > allowed_states;
public:
  const vector<int>& states(int j) const {return allowed_states[j];}
  vector<int>& states(int j) {return allowed_states[j];}

  // compute FP for the whole array
  void forward(const Matrix& GQ);

  // compute FP for a cell
  virtual void forward(int,const Matrix& GQ)=0;

  // functions on paths
  vector<int> sample_path(const Matrix& GQ);

  //FIXME - still need to implement checking framework
};


class DPmatrix : public DP, public vector<Matrix> {
  int s1;
  int s2;

protected:
  valarray<double> s1_sub;
  valarray<double> s2_sub;

public:
  // data for emission probabilities 
  const vector<double>& distribution;
  const vector< vector< valarray<double> > >& dists1;
  const vector< vector< valarray<double> > >& dists2;
  const valarray<double>& frequency;
  int nrates() const {return dists1[0].size();}

  // accessor functions
  int size1() const {return s1;}
  int size2() const {return s2;}

  bool di(int S) {bool e = false; if (state_emit[S]&(1<<0)) e=true;return e;}
  bool dj(int S) {bool e = false; if (state_emit[S]&(1<<1)) e=true;return e;}
  bool silent_network(int S);

  // compute FP for a square
  void forward(int,int,int,int,const Matrix& GQ);

  // compute FP for a cell
  virtual void forward(int,int,const Matrix& GQ)=0;

  // compute FP for entire matrix, with some points on path pinned
  void forward(const vector<int>&,int,const Matrix& GQ);

  // functions on paths
  vector<int> sample_path(const Matrix& GQ);

  vector<double> path_Q(const vector<int>& path,const Matrix& GQ);
  double path_P(const vector<int>& path, const Matrix& GQ);
  double path_check(const vector<int>& path, const Matrix& GQ);

  double check(const Matrix&,const Matrix&,const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2);

  double emitMM(int i,int j);
  double emit_M(int i,int j);
  double emitM_(int i,int j);
  double emit__(int i,int j);


  DPmatrix::DPmatrix(int nstates,
		     const vector< double >&,
		     const vector< vector< valarray<double> > >&,
		     const vector< vector< valarray<double> > >&, 
		     const valarray<double>&);

  virtual ~DPmatrix() {}
};

inline double DPmatrix::emitMM(int i,int j) {
  double total=0;
  for(int r=0;r<nrates();r++) 
    total += distribution[r]*sum(dists1[i-1][r] * frequency * dists2[j-1][r]);

  return log(total);
}

inline double DPmatrix::emitM_(int i,int j) {
  return s1_sub[i-1];
}

inline double DPmatrix::emit_M(int i,int j) {
  return s2_sub[j-1];
}

inline double DPmatrix::emit__(int i,int j) {
  return 0;
}

class DPmatrixHMM: public DPmatrix {
  vector< vector<int> > allowed_states;
public:
  const vector<int>& states(int j) const {return allowed_states[j];}
  vector<int>& states(int j) {return allowed_states[j];}

  void forward(int i1,int j1,int i2,int j2,const Matrix& GQ);

  void forward(int,int,const Matrix& GQ);

  DPmatrixHMM(int nstates,
	      const vector< double >& d0,
	      const vector< vector< valarray<double> > >& d1,const vector< vector< valarray<double> > >& d2, 
	      const valarray<double>& f):
    DPmatrix(nstates,d0,d1,d2,f), allowed_states(d2.size()+1)
  { }

  virtual ~DPmatrixHMM() {}
};

inline void DPmatrixHMM::forward(int i2,int j2,const Matrix& GQ) {

  assert(i2<size1());
  assert(j2<size2());

  for(int i=0;i<states(j2).size();i++) {
    int S2 = states(j2)[i];
    Matrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2))
      i1--;

    int j1 = j2;
    if (dj(S2))
      j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0)
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,j2) = log_0;
    for(int s=0;s<states(j1).size();s++) {
      int S1 = states(j1)[s];
      Matrix& FS1 = (*this)[S1];

      FS2(i2,j2) = logsum(FS2(i2,j2), FS1(i1,j1) + GQ(S1,S2));
    }

    //--- Include Emission Probability----
    double sub;
    if (i1 != i2 and j1 != j2)
      sub = emitMM(i2,j2);
    else if (i1 != i2)
      sub = emitM_(i2,j2);
    else if (j1 != j2)
      sub = emit_M(i2,j2);
    else          // silent state - nothing emitted
      sub = emit__(i2,j2);


    FS2(i2,j2) += sub;
  }
}     

inline void DPmatrixHMM::forward(int i1,int j1,int i2,int j2,const Matrix& GQ) {
  DPmatrix::forward(i1,j1,i2,j2,GQ);
}

#endif
