#ifndef DPMATRIX_H
#define DPMATRIX_H

#include "mytypes.H"
#include "logsum.H"

using std::valarray;

inline double sum(const valarray<double>& v) {
  return v.sum();
}

class DPmatrix : public vector<Matrix> {
  int s1;
  int s2;

protected:
  valarray<double> s1_sub;
  valarray<double> s2_sub;

public:
  // bitmasks of whether states emit features in each sequence
  vector<int> state_emit;

  // data for emission probabilities 
  const vector< valarray<double> >& dists1;
  const vector< valarray<double> >& dists2;
  const valarray<double>& frequency;

  // accessor functions
  int size1() const {return s1;}
  int size2() const {return s2;}
  int nstates() const {return size();}
  int endstate() const {return nstates();}

  bool di(int S) {bool e = false; if (state_emit[S]&(1<<0)) e=true;return e;}
  bool dj(int S) {bool e = false; if (state_emit[S]&(1<<1)) e=true;return e;}
  bool silent(int S) {return not di(S) and not dj(S);}
  bool silent_network(int S);

  // compute FP for a square
  void forward(int,int,int,int,const Matrix& GQ);

  // compute FP for a cell
  virtual void forward(int,int,const Matrix& GQ)=0;

  // compute FP for entire matrix, with some points on path pinned
  void forward(const vector<int>&,int,const Matrix& GQ);

  // functions on paths
  vector<int> sample_path(const Matrix& GQ);

  vector<int> generalize(const vector<int>& path);
  double generalize_P(const vector<int>& path, const Matrix& Q);

  vector<double> path_Q(const vector<int>& path,const Matrix& GQ);
  double path_P(const vector<int>& path, const Matrix& GQ);
  double path_check(const vector<int>& path, const Matrix& GQ);


  double check(const Matrix&,const Matrix&,const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2);

  DPmatrix(int nstates,
	   const vector< valarray<double> >& d1,const vector< valarray<double> >& d2, 
	   const valarray<double>& f)
    :vector<Matrix>(nstates,Matrix(d1.size()+1,d2.size()+1)), 
     s1(d1.size()+1),s2(d2.size()+1), 
     s1_sub(d1.size()),s2_sub(d2.size()),
     state_emit(nstates+1),
     dists1(d1),dists2(d2),frequency(f)
  {

    //----- zero-initialize matrices ------//
    for(int i=0;i<s1;i++)
      for(int j=0;j<s2;j++) 
	for(int S=0;S<nstates;S++)
	  (*this)[S](i,j)  = log_0;

    //----- cache G1,G2 emission probabilities -----//
    for(int i=0;i<dists1.size();i++)  
      s1_sub[i] = log(sum( dists1[i] * frequency ));

    for(int i=0;i<dists2.size();i++)
      s2_sub[i] = log(sum( dists2[i] * frequency ));
  }

  virtual ~DPmatrix() {}
};

class DPmatrixHMM: public DPmatrix {
  vector< vector<int> > allowed_states;
public:
  const vector<int>& states(int j) const {return allowed_states[j];}
  vector<int>& states(int j) {return allowed_states[j];}

  void forward(int i1,int j1,int i2,int j2,const Matrix& GQ);

  void forward(int,int,const Matrix& GQ);

  DPmatrixHMM(int nstates,
	      const vector< valarray<double> >& d1,const vector< valarray<double> >& d2, 
	      const valarray<double>& f):
    DPmatrix(nstates,d1,d2,f), allowed_states(d2.size()+1)
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
      sub = log(sum( dists1[i2-1] * frequency * dists2[j2-1] ));
    else if (i1 != i2)
      sub = s1_sub[i2-1];
    else if (j1 != j2)
      sub = s2_sub[j2-1];
    else          // silent state - nothing emitted
      sub = 0;


    FS2(i2,j2) += sub;
  }
}     

inline void DPmatrixHMM::forward(int i1,int j1,int i2,int j2,const Matrix& GQ) {
  DPmatrix::forward(i1,j1,i2,j2,GQ);
}

#endif
