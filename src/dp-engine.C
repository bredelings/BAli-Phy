#include <cmath>
#include "dp-engine.H"
#include "logsum.H"
#include "choose.H"
#include "rng.H"

using std::abs;

double DPengine::check(const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2) const {  

  // Add up the full likelihoods
  double l1 = lp1 + ls1;
  double l2 = lp2 + ls2;
  
  // get the probabilities of sampling each of the paths
  vector<int> path1_G = generalize(path1);
  vector<int> path2_G = generalize(path2);

  double p1 = path_P(path1_G) + generalize_P(path1);
  double p2 = path_P(path2_G) + generalize_P(path2); 

  // get the probabilities of the path through the 3-way HMM
  double qp1 = path_GQ_path(path1_G) + generalize_P(path1);
  double qs1 = path_Q_subst(path1_G);
  double q1 = qp1 + qs1;

  double qp2 = path_GQ_path(path2_G) + generalize_P(path2);
  double qs2 = path_Q_subst(path2_G);
  double q2 = qp2 + qs2;

  double diff = p2-p1-(l2-l1);

  if (path1_G != path2_G) {

    // Do the likelihood, path, and sampling probabilities match?
    std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<endl;
    std::cerr<<"Q1 = "<<q1<<"     Q2 = "<<q2<<"     Q2 - Q1 = "<<q2-q1<<endl;
    std::cerr<<"L1 = "<<l1<<"     L2 = "<<l2<<"     L2 - L1 = "<<l2-l1<<endl;
    std::cerr<<"diff = "<<diff<<std::endl;
    std::cerr<<endl;

    // Do the likelihood and HMM substitition probabilities agree?
    std::cerr<<"QS1 = "<<qs1<<"     QS2 = "<<qs2<<"   QS2 - QS1 = "<<qs2-qs1<<endl;
    std::cerr<<"LS1 = "<<ls1<<"     LS2 = "<<ls2<<"   LS2 - LS1 = "<<ls2-ls1<<endl;
    std::cerr<<endl;

    // Do the likelihood and HMM path probabilities agree?
    std::cerr<<"QP1 = "<<qp1<<"     QP2 = "<<qp2<<"   QP2 - QP1 = "<<qp2-qp1<<endl;
    std::cerr<<"LP1 = "<<lp1<<"     LP2 = "<<lp2<<"   LP2 - LP1 = "<<lp2-lp1<<endl;
    std::cerr<<endl;
  }

  return diff;
}

DPengine::DPengine(const vector<int>& v1,const vector<efloat_t>& v2, const eMatrix&M, double Temp)
  :HMM(v1,v2,M,Temp) 
{ }


int bandwidth(const DPmatrix& M,const vector<int>& path) {
  int max = 0;

  int i=0;
  int j=0;
  for(int c=0;c < path.size();c++) {

    if (M.di(path[c]))
      i++;
    if (M.dj(path[c]))
      j++;

    if (std::abs(i-j) > max)
      max = std::abs(i-j);
  }
  return max;
}


int bandwidth2(const DPmatrix& M,const vector<int>& path) {
  int max = 0;

  const int I = M.size1()-1;
  const int J = M.size2()-1;

  int i=0;
  int j=0;
  for(int c=0;c < path.size();c++) {

    if (M.di(path[c]))
      i++;
    if (M.dj(path[c]))
      j++;

    double w1 = std::abs(  i - double(j)*I/J );
    double w2 = std::abs(  j - double(i)*J/I );
    double w = std::max(w1,w2);
    if ((int)w > ceil(max))
      max = (int)w;
  }
  return max;
}


double DParray::path_P(const vector<int>& g_path) const {
  const int I = size()-1;
  int i=I;
  int l=g_path.size()-1;
  int state2 = g_path[l];

  efloat_t Pr=1.0;
  while (l>0) {

    vector<efloat_t> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1][i]*GQ(state1,state2);

    int state1 = g_path[l-1];
    efloat_t p = choose_P(state1,transition);
    assert(p > 0.0);

    if (di(state1)) i--;

    l--;
    state2 = state1;
    Pr *= p;
  }
  // include probability of choosing 'Start' vs ---+ !
  vector<efloat_t> transition(nstates());
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)[state1][0] * GQ(state1,state2);

  // Get the probability that the previous state was 'Start'
  efloat_t p=0.0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p += choose_P(state1,transition);

  Pr *= p;

  assert(Pr > 0.0);
  return log(Pr);
}

void DParray::forward() {
  for(int i=0;i<size();i++)
    forward(i);
}


vector<int> DParray::sample_path() const {
  vector<int> path;
  
  const int I = size()-1;
  int i = I;

  int state2 = endstate();

  while(i >= 0) {
    path.push_back(state2);
    vector<efloat_t> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1][i]*GQ(state1,state2);

    int state1 = choose(transition);

    if (di(state1)) i--;

    state2 = state1;
  }
  assert(i+di(state2)==0);

  std::reverse(path.begin(),path.end());
  return path;
}

double DParray::Pr_sum_all_paths() const {
  const int I = size()-1;

  efloat_t total = 0.0;
  for(int state1=0;state1<nstates();state1++)
    total += (*this)[state1][I] * GQ(state1,endstate());

  return log(total);
}

DParray::DParray(int l,const vector<int>& v1,const vector<efloat_t>& v2,const eMatrix& M,double Temp)
  :DPengine(v1,v2,M,Temp),
   vector< vector<efloat_t> >(nstates(),vector<efloat_t>(l+1,0)),length(l+1)
{ 
  for(int s=0;s<start_P.size();s++)
    (*this)[s][0] = start_P[s];
}

inline void DParrayConstrained::forward(int i2) {
  assert(i2<size());

  for(int s2=0;s2<states(i2).size();s2++) {
    int S2 = states(i2)[s2];
    vector<efloat_t>& FS2 = (*this)[S2];

    int i1 = i2;
    if (di(S2))
      i1--;

    //--- Don't go off the boundary -----
    if (i1<0) continue;

    //--- Compute Arrival Probability ---
    FS2[i2] = 0;
    for(int s1=0;s1<states(i1).size();s1++) {
      int S1 = states(i1)[s1];

      vector<efloat_t>& FS1 = (*this)[S1];

      FS2[i2] += FS1[i1] * GQ(S1,S2);
    }
  }
}

void DParrayConstrained::forward() {
  for(int i=0;i<size();i++)
    forward(i);
}


int DParrayConstrained::order_of_computation() const {
  unsigned total=0;
  for(int c=0;c<allowed_states.size()-1;c++)
    total += allowed_states[c].size() * allowed_states[c+1].size();

  return total;
}


void DParrayConstrained::prune() {

  std::abort();

  unsigned order1 = order_of_computation();

  // For column
  for(int c = 1;c<allowed_states.size();c++) {

    // and for each allowed state in that column
    for(int s2=allowed_states[c].size()-1;s2 >= 0;s2--) {

      // check to see whether any states in the previous column are connected to it
      bool used = false;
      for(int s1 = 0;s1<allowed_states[c-1].size() and not used;s1++) {
	if (connected(allowed_states[c-1][s1],allowed_states[c][s2]))
	    used = true;
      }

      // if nothing is connected, then remove it
      if (not used)
	allowed_states[c].erase(allowed_states[c].begin()+s2);
    }
  }

  unsigned order2 = order_of_computation();
  std::cerr<<" order1 = "<<order1<<"    order2 = "<<order2<<"  fraction = "<<double(order2)/double(order1)<<endl;
}

DParrayConstrained::DParrayConstrained(int l,const vector<int>& v1,const vector<efloat_t>& v2,const eMatrix& M,double Temp)
  :DParray(l,v1,v2,M,Temp),allowed_states(l+1)
{ }


inline void DPmatrix::forward_square(int x1,int y1,int x2,int y2) {
  assert(x1 <= x2 or y1 <= y2);
  assert(x2 < size1());
  assert(y2 < size2());

  // Since we are using M(0,0) instead of S(0,0), we need to run only the silent states at (0,0)
  // We can only use non-silent states at (0,0) to simulate S

  for(int x=x1;x<=x2;x++)
    for(int y=y1;y<=y2;y++)
      forward_cell(x,y,x1,y1);
}

void DPmatrix::forward_square() {
  const int I = size1()-1;
  const int J = size2()-1;

  forward_square(0,0,I,J);
}

void DPmatrix::forward_band(int bw) {
  const int I = size1()-1;
  const int J = size2()-1;

  double b = std::max(double(bw),double(bw)*J/I)+0.00001;

  for(int x=0;x<=I;x++) {
    double y_bot = double(x)*J/I - b;
    double y_top = double(x)*J/I + b;

    int y1 = (int)ceil(y_bot);
    y1 = std::max(0,y1);
    int y2 = (int)floor(y_top);
    y2 = std::min(J,y2);
    
    for(int y=y1;y<=y2;y++)
      forward_cell(x,y,0,0);
  }
}


// FIXME - must fix entire columns, row, not chosen based on the path
void DPmatrix::forward_constrained(const vector< vector<int> >& pins) {
  const int I = size1()-1;
  const int J = size2()-1;

  if (pins[0].size() == 0) 
    forward_square(0,0,I,J);
  else {
    const vector<int>& x = pins[0];
    const vector<int>& y = pins[1];

    // Propogate from S to first pin
    forward_square(0,0,x[0],y[0]);

    // Propogate from first pin to other pins (if any)
    for(int i=0;i<(int)x.size()-1;i++)
      forward_square(x[i]+1,y[i]+1,x[i+1],y[i+1]);

    int p = x.size()-1;
    forward_square(x[p]+1,y[p]+1,I,J);
  }
}

vector<int> DPmatrix::forward(const vector<vector<int> >& pins) 
{
  forward_constrained(pins);
  return sample_path();
}


double DPmatrix::path_check(const vector<int>& path) const {
  efloat_t Pr=1.0;
  
  const int I = size1()-1;
  const int J = size2()-1;

  int i = 0;
  int j = 0;

  int l = 0;

  // we don't look at transitions FROM the end state, because we 
  //  bail at looking at transitions FROM (I,C) 
  // FIXME - but what if this is actually not E but 7?
  while(true) {
    assert(l<path.size());

    int state1 = path[l];

    if (di(state1)) i++;
    if (dj(state1)) j++;

    if (state1 == endstate())
      break;

    int state2 = path[l+1];

    vector<efloat_t> transition(nstates());
    for(int s=0;s<nstates();s++)
      transition[s] = (*this)[s](i,j)*GQ(s,state2);
    
    efloat_t p = choose_P(state1,transition);
    assert((*this)[state1](i,j) > 0.0);
    assert(GQ(state1,state2) > 0.0);
    assert(p > 0.0);
    
    l++;
    Pr *= p;

    assert(i<=I and j<=J);
  }

  assert(l == path.size()-1);
  assert(i == I and j == J);
  assert(Pr > 0.0);

  return log(Pr);
}

double DPmatrix::path_P(const vector<int>& path) const {
  double P2 = path_check(path);
  std::cerr<<"P(path)2 = "<<P2<<std::endl;

  const int I = size1()-1;
  const int J = size2()-1;
  int i = I;
  int j = J;
  efloat_t Pr=1.0;

  int l = path.size()-1;
  int state2 = path[l];

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - but we would have to check path[-1] to see which state
  //   made sample_path go negative
  // - instead we can check if l==0 - we know that the start state
  //   is at path[-1]
  while (l>0) {

    vector<efloat_t> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1](i,j)*GQ(state1,state2);

    int state1 = path[l-1];
    efloat_t p = choose_P(state1,transition);
    assert(p > 0.0);

    if (di(state1)) i--;
    if (dj(state1)) j--;

    l--;
    state2 = state1;
    Pr *= p;
  }
  assert(l == 0);
  assert(i == 0 and j == 0);

  // include probability of choosing 'Start' vs ---+ !
  vector<efloat_t> transition(nstates());
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)[state1](0,0) * GQ(state1,state2);

  // Get the probability that the previous state was 'Start'
  efloat_t p=0.0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p += choose_P(state1,transition);

  Pr *= p;

  assert(Pr > 0.0);
  std::cerr<<"P(path) = "<<log(Pr)<<std::endl;
  return log(Pr);
}

vector<int> DPmatrix::sample_path() const {
  vector<int> path;

  const int I = size1()-1;
  const int J = size2()-1;
  int i = I;
  int j = J;

  int state2 = endstate();

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - check that we came from (0,0) though
  while (i>=0 and j>=0) {
    path.push_back(state2);
    vector<efloat_t> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1](i,j)*GQ(state1,state2);

    int state1 = choose(transition);

    if (di(state1)) i--;
    if (dj(state1)) j--;

    state2 = state1;
  }
  assert(i+di(state2)==0 and j+dj(state2)==0);

  std::reverse(path.begin(),path.end());
  return path;
}

double DPmatrix::Pr_sum_all_paths() const {
  const int I = size1()-1;
  const int J = size2()-1;

  efloat_t total = 0.0;
  for(int state1=0;state1<nstates();state1++)
    total += (*this)[state1](I,J)*GQ(state1,endstate());

  return log(total);
}


DPmatrix::DPmatrix(int i1,
		   int i2,
		   const vector<int>& v1,
		   const vector<efloat_t>& v2,
		   const eMatrix& M,
		   double Temp)
  :DPengine(v1,v2,M,Temp),
   vector<eMatrix >(nstates(),eMatrix(i1+1,i2+1)),
   S1(i1+1),
   S2(i2+1)
{

  //----- zero-initialize matrices ------//
  for(int S=0;S<nstates();S++)
    for(int i=0;i<size1();i++)
      for(int j=0;j<size2();j++) 
	(*this)[S](i,j)  = 0;

  //----- set up start probabilities -----//
  for(int S=0;S<start_P.size();S++)
    (*this)[S](0,0) = start_P[S];
}

inline void DPmatrixNoEmit::forward_cell(int i2,int j2,int x1,int y1) { 

  assert(i2<size1());
  assert(j2<size2());

  for(int S2=0;S2<nstates();S2++) {
    eMatrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0)
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,j2) = 0;
    for(int S1=0;S1<nstates();S1++) {
      eMatrix& FS1 = (*this)[S1];

      FS2(i2,j2) += FS1(i1,j1) * GQ(S1,S2);
    }
  }
} 

inline double sum(const valarray<double>& v) {
  return v.sum();
}

/*
inline double DPmatrixEmit::emitMM(int i,int j) const {
  double total=0;
  for(int r=0;r<nrates();r++) {
    //    total += distribution[r]*sum(dists1[i-1][r] * frequency * dists2[j-1][r]);   HANDCODED
    double temp=0;
    const valarray<double>& v1 = dists1[i-1][r];
    const valarray<double>& v2 = dists2[j-1][r];
    for(int l=0;l<v1.size();l++)
      temp += v1[l]*frequency[l]*v2[l];
    total += distribution[r]*temp;
  }

  return log(total)/T;
}
*/

// switching dists1[] to matrices actually made things WORSE!

inline efloat_t DPmatrixEmit::emitMM(int i,int j) const {
  return s12_sub(i-1,j-1);
}

inline efloat_t DPmatrixEmit::emitM_(int i,int j) const {
  return s1_sub[i-1];
}

inline efloat_t DPmatrixEmit::emit_M(int i,int j) const {
  return s2_sub[j-1];
}

inline efloat_t DPmatrixEmit::emit__(int i,int j) const {
  return 1.0;
}

double DPmatrixEmit::path_Q_subst(const vector<int>& path) const {
  efloat_t P_sub=1.0;
  int i=0,j=0;
  for(int l=0;l<path.size();l++) {

    int state2 = path[l];
    if (di(state2))
      i++;
    if (dj(state2))
      j++;

    efloat_t sub=0.0;
    if (di(state2) and dj(state2))
      sub = emitMM(i,j);
    else if (di(state2))
      sub = emitM_(i,j);
    else if (dj(state2))
      sub = emit_M(i,j);
    else
      sub = emit__(i,j);


    P_sub *= sub;
  }
  assert(i == size1()-1 and j == size2()-1);
  return log(P_sub);
}


DPmatrixEmit::DPmatrixEmit(const vector<int>& v1,
			   const vector<efloat_t>& v2,
			   const eMatrix& M,
			   double Temp,
			   const vector< double >& d0,
			   const vector< Matrix >& d1,
			   const vector< Matrix >& d2, 
			   const Matrix& f)
  :DPmatrix(d1.size(),d2.size(),v1,v2,M,Temp),
   s12_sub(d1.size(),d2.size()),
   s1_sub(d1.size()),s2_sub(d2.size()),
   distribution(d0),
   dists1(d1),dists2(d2),frequency(f)
{
  
  //----- cache G1,G2 emission probabilities -----//
  for(int i=0;i<dists1.size();i++) {
    double total=0;
    for(int m=0;m<nrates();m++) {
      double temp=0;
      for(int l=0;l<dists1[i].size2();l++)
	temp += frequency(m,l)*dists1[i](m,l);
      total += temp*distribution[m];
    }
    s1_sub[i] = total;
    s1_sub[i] = pow(s1_sub[i],1.0/T);
  }

  for(int i=0;i<dists2.size();i++) {
    double total=0;
    for(int m=0;m<nrates();m++) {
      double temp=0;
      for(int l=0;l<dists2[i].size2();l++)
	temp += frequency(m,l)*dists2[i](m,l);
      total += temp*distribution[m];
    }
    s2_sub[i] = total;
    s2_sub[i] = pow(s2_sub[i],1.0/T);
  }

  //----- pre-calculate scaling factors --------//
  for(int i=0;i<dists2.size();i++) {
    for(int m=0;m<nrates();m++)
      for(int l=0;l<dists2[i][m].size();l++)
	dists2[i](m,l) *= distribution[m] * frequency(m,l);
  }

  //----- cache M emission probabilities -----//
  for(int i=0;i<s12_sub.size1();i++)
    for(int j=0;j<s12_sub.size2();j++) {
      const Matrix& M1 = dists1[i];
      const Matrix& M2 = dists2[j];
      double total=0;
      for(int m=0;m<M1.size1();m++) {
	for(int l=0;l<M1.size2();l++)
	  total += M1(m,l) * M2(m,l);
      }
      s12_sub(i,j) = total;
      s12_sub(i,j) = pow(s12_sub(i,j),1.0/T);
    }

}


inline void DPmatrixSimple::forward_cell(int i2,int j2,int x1, int y1) {

  assert(i2<size1());
  assert(j2<size2());

  for(int S2=0;S2<nstates();S2++) {
    eMatrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0) 
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,j2) = 0;

    for(int S1=0;S1<nstates();S1++) {
      eMatrix& FS1 = (*this)[S1];

      FS2(i2,j2) += FS1(i1,j1) * GQ(S1,S2);
    }

    //--- Include Emission Probability----
    efloat_t sub;
    if (i1 != i2 and j1 != j2)
      sub = emitMM(i2,j2);
    else if (i1 != i2)
      sub = emitM_(i2,j2);
    else if (j1 != j2)
      sub = emit_M(i2,j2);
    else          // silent state - nothing emitted
      sub = emit__(i2,j2);


    FS2(i2,j2) *= sub;
  }
} 
inline void DPmatrixConstrained::forward_cell(int i2,int j2,int x1,int y1) {

  assert(i2<size1());
  assert(j2<size2());

  for(int i=0;i<states(j2).size();i++) {
    int S2 = states(j2)[i];
    eMatrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0) 
      continue;


    //--- Compute Arrival Probability ----
    FS2(i2,j2) = 0.0;
    for(int s=0;s<states(j1).size();s++) {
      int S1 = states(j1)[s];

      eMatrix& FS1 = (*this)[S1];
      
      FS2(i2,j2) +=  FS1(i1,j1) * GQ(S1,S2);
    }

    //--- Include Emission Probability----
    efloat_t sub;
    if (i1 != i2 and j1 != j2)
      sub = emitMM(i2,j2);
    else if (i1 != i2)
      sub = emitM_(i2,j2);
    else if (j1 != j2)
      sub = emit_M(i2,j2);
    else          // silent state - nothing emitted
      sub = emit__(i2,j2);

    FS2(i2,j2) *= sub;
  }
}

int DPmatrixConstrained::order_of_computation() const {
  unsigned total=0;
  for(int c=0;c<allowed_states.size()-1;c++)
    total += allowed_states[c].size() * allowed_states[c+1].size();

  return total;
}


void DPmatrixConstrained::prune() {

  std::abort();

  unsigned order1 = order_of_computation();

  // For column
  for(int c = 1;c<allowed_states.size();c++) {

    // and for each allowed state in that column
    for(int s2=allowed_states[c].size()-1;s2 >= 0;s2--) {

      // check to see whether any states in the previous column are connected to it
      bool used = false;
      for(int s1 = 0;s1<allowed_states[c-1].size() and not used;s1++) {
	if (connected(allowed_states[c-1][s1],allowed_states[c][s2]))
	    used = true;
      }

      // if nothing is connected, then remove it
      if (not used)
	allowed_states[c].erase(allowed_states[c].begin()+s2);
    }
  }

  unsigned order2 = order_of_computation();
  std::cerr<<" order1 = "<<order1<<"    order2 = "<<order2<<"  fraction = "<<double(order2)/double(order1)<<endl;
}
