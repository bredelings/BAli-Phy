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

DPengine::DPengine(const vector<int>& v1,const vector<double>& v2, const Matrix&M, double Temp)
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

  double Pr=0;
  while (l>0) {

    vector<double> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1][i]+GQ(state1,state2);

    int state1 = g_path[l-1];
    double p = choose_P(state1,transition);
    assert(p > log_limit);

    if (di(state1)) i--;

    l--;
    state2 = state1;
    Pr += p;
  }
  // include probability of choosing 'Start' vs ---+ !
  vector<double> transition(nstates());
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)[state1][0] + GQ(state1,state2);

  // Get the probability that the previous state was 'Start'
  double p=log_0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p = logsum(p, choose_P(state1,transition) );

  Pr += p;

  assert(Pr > log_0);
  return Pr;
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
    vector<double> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1][i]+GQ(state1,state2);

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

  double total = log_0;
  for(int state1=0;state1<nstates();state1++)
    total = logsum(total,(*this)[state1][I] + GQ(state1,endstate()));

  return total;
}

DParray::DParray(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M,double Temp)
  :DPengine(v1,v2,M,Temp),
   vector< vector<double> >(nstates(),vector<double>(l+1,log_0)),length(l+1)
{ 
  for(int s=0;s<start_P.size();s++)
    (*this)[s][0] = start_P[s];
}

inline void DParrayConstrained::forward(int i2) {
  assert(i2<size());

  for(int s2=0;s2<states(i2).size();s2++) {
    int S2 = states(i2)[s2];
    vector<double>& FS2 = (*this)[S2];

    int i1 = i2;
    if (di(S2))
      i1--;

    //--- Don't go off the boundary -----
    if (i1<0) continue;

    //--- Compute Arrival Probability ---
    FS2[i2] = log_0;
    for(int s1=0;s1<states(i1).size();s1++) {
      int S1 = states(i1)[s1];
      if (not connected(S1,S2)) continue;

      vector<double>& FS1 = (*this)[S1];

      if (FS1[i1] < log_limit) continue;

      FS2[i2] = logsum(FS2[i2],FS1[i1] + GQ(S1,S2));
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

DParrayConstrained::DParrayConstrained(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M,double Temp)
  :DParray(l,v1,v2,M,Temp),allowed_states(l+1)
{ }


void DPmatrix::forward_square(int x1,int y1,int x2,int y2) {
  assert(x1 < x2 or y1 < y2);
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
void DPmatrix::forward_pinned(const vector<int>& path,double bandwidth) {
  vector<int> icol;
  vector<int> jcol;

  int i=0;
  int j=0;
  icol.push_back(i);
  jcol.push_back(j);
  for(int c=0;c < path.size();c++) {

    if (di(path[c]))
      i++;
    if (dj(path[c]))
      j++;

    if (not silent(path[c])) {
      icol.push_back(i);
      jcol.push_back(j);
    }
  }

  assert(icol[icol.size()-1] == size1()-1 );
  assert(jcol[jcol.size()-1] == size2()-1 );

  vector<int> pins;
  int column=0;
  while(column < icol.size()-1) {
    pins.push_back(column);
    column += geometric(1.0/bandwidth);
  }
  pins.push_back(icol.size()-1);

  // Deal with silent states at (0,0)
  forward_cell(0,0,0,0);

  // Process the squares generated
  for(int i=0;i<pins.size()-1;i++) {
    forward_square(icol[pins[i]],jcol[pins[i]],
		   icol[pins[i+1]],jcol[pins[i+1]]);
  }
}

vector<int> DPmatrix::forward(int features,int bandwidth,const vector<int>& path_old_g) {
  vector<int> path_new_g;

  bool banding = features&(1<<1);
  bool pinning = features&(1<<0);

  //-------------------- Banding -------------------//
  if (banding) {
    const double f = 0.02;

    int b1 = bandwidth2(*this,path_old_g);
    //---------- withing the band -----------//
    if (b1 <= bandwidth) {
      if (myrandomf() < f)
	forward_square();
      else
	forward_band(bandwidth);
      path_new_g = sample_path();
    }
    //--------------- without --------------//
    else {
      forward_square();
      path_new_g = sample_path();

      int b2 = bandwidth2(*this,path_new_g);
      if (b2 <= bandwidth)
	if (myrandomf() < f)
	  ;
	else
	  path_new_g = path_old_g;
    }
  }
  //-------------------- Pinning -------------------//
  else if (pinning) {
    forward_pinned(path_old_g,bandwidth);
    path_new_g = sample_path();
  }
  //------------------ Full Square ------------------//
  else {
    forward_square();
    path_new_g = sample_path();
  }

  return path_new_g;
}


double DPmatrix::path_check(const vector<int>& path) const {
  double Pr=0;
  
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

    vector<double> transition(nstates());
    for(int s=0;s<nstates();s++)
      transition[s] = (*this)[s](i,j)+GQ(s,state2);

    double p = choose_P(state1,transition);
    assert((*this)[state1](i,j) > log_limit);
    assert(GQ(state1,state2) > log_limit);
    assert(p > log_limit);
    
    l++;
    Pr += p;

    assert(i<=I and j<=J);
  }

  assert(l == path.size()-1);
  assert(i == I and j == J);
  assert(Pr > log_limit);

  return Pr;
}

double DPmatrix::path_P(const vector<int>& path) const {
  double P2 = path_check(path);
  std::cerr<<"P(path)2 = "<<P2<<std::endl;

  const int I = size1()-1;
  const int J = size2()-1;
  int i = I;
  int j = J;
  double Pr=0;

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

    vector<double> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1](i,j)+GQ(state1,state2);

    int state1 = path[l-1];
    double p = choose_P(state1,transition);
    assert(p > log_limit);

    if (di(state1)) i--;
    if (dj(state1)) j--;

    l--;
    state2 = state1;
    Pr += p;
  }
  assert(l == 0);
  assert(i == 0 and j == 0);

  // include probability of choosing 'Start' vs ---+ !
  vector<double> transition(nstates());
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)[state1](0,0) + GQ(state1,state2);

  // Get the probability that the previous state was 'Start'
  double p=log_0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p = logsum(p, choose_P(state1,transition) );

  Pr += p;

  assert(Pr > log_0);
  std::cerr<<"P(path) = "<<Pr<<std::endl;
  return Pr;
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
    vector<double> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1](i,j)+GQ(state1,state2);

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

  double total = log_0;
  for(int state1=0;state1<nstates();state1++)
    total = logsum(total,(*this)[state1](I,J)+GQ(state1,endstate()));

  return total;
}


DPmatrix::DPmatrix(int i1,
		   int i2,
		   const vector<int>& v1,
		   const vector<double>& v2,
		   const Matrix& M,
		   double Temp)
  :DPengine(v1,v2,M,Temp),
   vector<Matrix>(nstates(),Matrix(i1+1,i2+1)),
   S1(i1+1),
   S2(i2+1)
{

  //----- zero-initialize matrices ------//
  for(int S=0;S<nstates();S++)
    for(int i=0;i<size1();i++)
      for(int j=0;j<size2();j++) 
	(*this)[S](i,j)  = log_0;

  //----- set up start probabilities -----//
  for(int S=0;S<start_P.size();S++)
    (*this)[S](0,0) = start_P[S];
}

inline void DPmatrixNoEmit::forward_cell(int i2,int j2,int x1,int y1) { 

  assert(i2<size1());
  assert(j2<size2());

  for(int S2=0;S2<nstates();S2++) {
    Matrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0)
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,j2) = log_0;
    for(int S1=0;S1<nstates();S1++) {
      Matrix& FS1 = (*this)[S1];

      FS2(i2,j2) = logsum(FS2(i2,j2), FS1(i1,j1) + GQ(S1,S2));
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

inline double DPmatrixEmit::emitMM(int i,int j) const {
  return s12_sub(i-1,j-1);
}

inline double DPmatrixEmit::emitM_(int i,int j) const {
  return s1_sub[i-1];
}

inline double DPmatrixEmit::emit_M(int i,int j) const {
  return s2_sub[j-1];
}

inline double DPmatrixEmit::emit__(int i,int j) const {
  return 0;
}

double DPmatrixEmit::path_Q_subst(const vector<int>& path) const {
  double P_sub=0;
  int i=0,j=0;
  for(int l=0;l<path.size();l++) {

    int state2 = path[l];
    if (di(state2))
      i++;
    if (dj(state2))
      j++;

    double sub=0;
    if (di(state2) and dj(state2))
      sub = emitMM(i,j);
    else if (di(state2))
      sub = emitM_(i,j);
    else if (dj(state2))
      sub = emit_M(i,j);
    else
      sub = emit__(i,j);

    P_sub += sub;
  }
  assert(i == size1()-1 and j == size2()-1);
  return P_sub;
}


DPmatrixEmit::DPmatrixEmit(const vector<int>& v1,
			   const vector<double>& v2,
			   const Matrix& M,
			   double Temp,
			   const vector< double >& d0,
			   const vector< Matrix >& d1,
			   const vector< Matrix >& d2, 
			   const valarray<double>& f)
  :DPmatrix(d1.size(),d2.size(),v1,v2,M,Temp),
   s12_sub(d1.size(),d2.size()),
   s1_sub(d1.size()),s2_sub(d2.size()),
   distribution(d0),
   dists1(d1),dists2(d2),frequency(f)
{
  
  //----- cache G1,G2 emission probabilities -----//
  for(int i=0;i<dists1.size();i++) {
    double total=0;
    for(int r=0;r<nrates();r++) {
      double temp=0;
      for(int l=0;l<dists1[i].size2();l++)
	temp += frequency[l]*dists1[i](r,l);
      total += temp*distribution[r];
    }
    s1_sub[i] = log(total)/T;
  }

  for(int i=0;i<dists2.size();i++) {
    double total=0;
    for(int r=0;r<nrates();r++) {
      double temp=0;
      for(int l=0;l<dists2[i].size2();l++)
	temp += frequency[l]*dists2[i](r,l);
      total += temp*distribution[r];
    }
    s2_sub[i] = log(total)/T;
  }

  //----- pre-calculate scaling factors --------//
  for(int i=0;i<dists2.size();i++) {
    for(int r=0;r<nrates();r++)
      for(int l=0;l<dists2[i][r].size();l++)
	dists2[i](r,l) *= distribution[r] * frequency[l];
  }

  //----- cache M emission probabilities -----//
  for(int i=0;i<s12_sub.size1();i++)
    for(int j=0;j<s12_sub.size2();j++) {
      const Matrix& M1 = dists1[i];
      const Matrix& M2 = dists2[j];
      double total=0;
      for(int r=0;r<M1.size1();r++) {
	for(int l=0;l<M1.size2();l++)
	  total += M1(r,l) * M2(r,l);
      }
      s12_sub(i,j) = log(total)/T;
    }

}


inline void DPmatrixSimple::forward_cell(int i2,int j2,int x1, int y1) {

  assert(i2<size1());
  assert(j2<size2());

  for(int S2=0;S2<nstates();S2++) {
    Matrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0) 
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,j2) = log_0;
    for(int S1=0;S1<nstates();S1++) {
      if (not connected(S1,S2)) continue;

      Matrix& FS1 = (*this)[S1];

      if (FS1(i1,j1) < log_limit) continue;

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
inline void DPmatrixConstrained::forward_cell(int i2,int j2,int x1,int y1) {

  assert(i2<size1());
  assert(j2<size2());

  for(int i=0;i<states(j2).size();i++) {
    int S2 = states(j2)[i];
    Matrix& FS2 = (*this)[S2];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0) 
      continue;


    //--- Compute Arrival Probability ----
    FS2(i2,j2) = log_0;
    for(int s=0;s<states(j1).size();s++) {
      int S1 = states(j1)[s];

      if (not connected(S1,S2)) continue;

      Matrix& FS1 = (*this)[S1];
      
      if (FS1(i1,j1) < log_limit) continue;

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

int DPmatrixConstrained::order_of_computation() const {
  unsigned total=0;
  for(int c=0;c<allowed_states.size()-1;c++)
    total += allowed_states[c].size() * allowed_states[c+1].size();

  return total;
}


void DPmatrixConstrained::prune() {

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
