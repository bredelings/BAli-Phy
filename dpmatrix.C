#include "dpmatrix.H"
#include "logsum.H"
#include "choose.H"
#include "rng.H"

using std::abs;

// Is this state silent and in a loop of silent states?
bool HMM::silent_network(int S) {
  if (S==endstate())
    return false;

  if (state_emit[S])
    return false;
  else
    return true;
}

vector<int> HMM::generalize(const vector<int>& path) {
  vector<int> g_path = path;
  for(int i=g_path.size()-1;i>0;i--) {
    int S1 = g_path[i-1];
    int S2 = g_path[i];
    if (silent_network(S1) and silent_network(S2))
      g_path.erase(g_path.begin()+i);
  }
  return g_path;
}

// FIXME - this doesn't deal with silent networks that have more than
//         one silent state!
vector<int> HMM::ungeneralize(const vector<int>& g_path) {
  vector<int> path = g_path;
  for(int i=path.size()-1;i>0;i--) {
    int S1 = path[i];
    if (silent_network(S1)) {

      //success is leaving the silent network
      double p_success = 1.0 - exp(Q(S1,S1));  

      int n = geometric(p_success);
      path.insert(path.begin()+i,n-1,S1);
    }
  }

  return path;
}

// FIXME - this doesn't deal with silent networks that have more than
//         one silent state!
double HMM::generalize_P(const vector<int>& path) {
  double Pr = 0;
  for(int i=1; i<path.size(); i++) {
    int S1 = path[i-1];
    int S2 = path[i];

    if (silent_network(S1)) {
      if (silent_network(S2))
	Pr += Q(S1,S2);
      else
	Pr += log(1.0-exp(Q(S1,S1)));
    }
  }
  return Pr;
}

double HMM::path_Q_path(const vector<int>& g_path) {

  double Pr = log_0;
  for(int S=0;S<nstates();S++)
    if (not silent(S))
      Pr = logsum(Pr,start_P[S] + GQ(S,g_path[0]));

  for(int l=1;l<g_path.size();l++)
    Pr += GQ(g_path[l-1],g_path[l]);

  return Pr;
}


HMM::HMM(const vector<int>& v1,const vector<double>& v2,const Matrix& M)
  :Q(M),GQ(M),start_P(v2),state_emit(v1) 
{
  assert(start_P.size() == nstates());

  for(int S1=0;S1<GQ.size1();S1++) {
    if (not silent_network(S1)) continue;
    for(int S2=0;S2<GQ.size1();S2++) {
      if (silent_network(S2)) {
	GQ(S1,S2) = log_0;
	assert(S1==S2); // this isn't the generalized version...
      }
      else {
	GQ(S1,S2) += -log(1.0-exp(Q(S1,S1)));
      }
    }
  }
}

double HMM::check(const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2) {  

  // Add up the full likelihoods
  double l1 = lp1 + ls1;
  double l2 = lp2 + ls2;
  
  // get the probabilities of sampling each of the paths
  vector<int> path1_G = generalize(path1);
  vector<int> path2_G = generalize(path2);

  double p1 = path_P(path1_G); 
  double p2 = path_P(path2_G); 

  p1 += generalize_P(path1);
  p2 += generalize_P(path2);

  // get the probabilities of the path through the 3-way HMM
  double qp1 = path_Q_path(path1_G) + generalize_P(path1);
  double qs1 = path_Q_subst(path1_G);
  double q1 = qp1 + qs1;

  double qp2 = path_Q_path(path2_G) + generalize_P(path2);
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
    std::cerr<<"LS1 = "<<ls1<<"     LS2 = "<<ls2<<"   LS2 - LS1 = "<<ls2-ls1<<endl;
    std::cerr<<"QS1 = "<<qs1<<"     QS2 = "<<qs2<<"   QS2 - QS1 = "<<qs2-qs1<<endl;
    std::cerr<<endl;

    // Do the likelihood and HMM path probabilities agree?
    std::cerr<<"LP1 = "<<lp1<<"     LP2 = "<<lp2<<endl;
    std::cerr<<"QP1 = "<<qp1<<"     QP2 = "<<qp2<<endl;
    std::cerr<<endl;

    // Since we can compute these accurately, make sure they match
    assert(abs(lp1-qp1) < 1.0e-9);
    assert(abs(lp2-qp2) < 1.0e-9);
  }

  return diff;
}

double DParray::path_P(const vector<int>& path) {
  const int I = size()-1;
  int i=I;
  int l=path.size()-1;
  int state2 = path[l];

  double Pr=0;
  while (l>0) {

    vector<double> transition(nstates());
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)[state1][i]+GQ(state1,state2);

    int state1 = path[l-1];
    double p = choose_P(state1,transition);
    assert(p > log_0/100);

    if (di(state1)) i--;

    l--;
    state2 = state1;
    Pr += p;
  }
  // include probability of choosing 'Start' vs ---+ !
  vector<double> transition(nstates());
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)[state1][0] + GQ(state1,state2);

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


vector<int> DParray::sample_path() {
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

DParray::DParray(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M)
  :HMM(v1,v2,M),vector< vector<double> >(nstates(),vector<double>(l+1,log_0)),length(l+1)
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
      vector<double>& FS1 = (*this)[S1];

      FS2[i2] = logsum(FS2[i2],FS1[i1] + GQ(S1,S2));
    }
  }
}

void DParrayConstrained::forward() {
  for(int i=0;i<size();i++)
    forward(i);
}



DParrayConstrained::DParrayConstrained(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M)
  :DParray(l,v1,v2,M),allowed_states(l+1)
{ }


void DPmatrix::forward(int x1,int y1,int x2,int y2) {
  assert(x1 < x2 or y1 < y2);
  assert(x2 < size1());
  assert(y2 < size2());

  const int maxdelta = std::max(x2-x1,y2-y1);

  for(int delta=1; delta<=maxdelta; delta++) {
    if (y1 + delta <= y2)
      for(int i=0;i<delta and x1+i <= x2;i++) 
	forward(x1+i,y1+delta);

    if (x1 + delta <= x2)
      for(int i=0;i<=delta and y1+i <= y2;i++)
	forward(x1+delta,y1+i);
  } 
}

void DPmatrix::forward(const vector<int>& path,double bandwidth) {
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
  forward(0,0);

  // Process the squares generated
  for(int i=0;i<pins.size()-1;i++) {
    forward(icol[pins[i]],jcol[pins[i]],
	    icol[pins[i+1]],jcol[pins[i+1]]);
  }
}


double DPmatrix::path_Q_subst(const vector<int>& path) {
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


double DPmatrix::path_check(const vector<int>& path) {
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
    assert((*this)[state1](i,j) > log_0/100);
    assert(GQ(state1,state2) > log_0/100);
    assert(p > log_0/100);
    
    l++;
    Pr += p;

    assert(i<=I and j<=J);
  }

  assert(l == path.size()-1);
  assert(i == I and j == J);
  assert(Pr > log_0/100);

  return Pr;
}

double DPmatrix::path_P(const vector<int>& path) {
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
    assert(p > log_0/100);

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

  double p=log_0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p = logsum(p, choose_P(state1,transition) );

  Pr += p;

  assert(Pr > log_0);
  std::cerr<<"P(path) = "<<Pr<<std::endl;
  return Pr;
}

vector<int> DPmatrix::sample_path() {
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

DPmatrix::DPmatrix(const vector<int>& v1,
		   const vector<double>& v2,
		   const Matrix& M,
		   const vector< double >& d0,
		   const vector< vector< valarray<double> > >& d1,
		   const vector< vector< valarray<double> > >& d2, 
		   const valarray<double>& f)
  :HMM(v1,v2,M),
   vector<Matrix>(nstates(),Matrix(d1.size()+1,d2.size()+1)), 
   s1_sub(d1.size()),s2_sub(d2.size()),
   distribution(d0),
   dists1(d1),dists2(d2),frequency(f)
{
  
  //----- zero-initialize matrices ------//
  for(int i=0;i<size1();i++)
    for(int j=0;j<size2();j++) 
      for(int S=0;S<nstates();S++)
	(*this)[S](i,j)  = log_0;
  
  //----- cache G1,G2 emission probabilities -----//
  for(int i=0;i<dists1.size();i++) {
    double total=0;
    for(int r=0;r<nrates();r++)
      total += distribution[r]*sum( dists1[i][r] * frequency );
    s1_sub[i] = log(total);
  }
  
  for(int i=0;i<dists2.size();i++) {
    double total=0;
    for(int r=0;r<nrates();r++)
      total += distribution[r]*sum( dists2[i][r] * frequency );
    s2_sub[i] = log(total);
  }

  for(int S=0;S<start_P.size();S++)
    (*this)[S](0,0) = start_P[S];
}

inline void DPmatrixSimple::forward(int x1,int y1,int x2,int y2) {
  assert(x1 < x2 or y1 < y2);
  assert(x2 < size1());
  assert(y2 < size2());

  const int maxdelta = std::max(x2-x1,y2-y1);

  for(int delta=1; delta<=maxdelta; delta++) {
    if (y1 + delta <= y2)
      for(int i=0;i<delta and x1+i <= x2;i++) 
	forward(x1+i,y1+delta);

    if (x1 + delta <= x2)
      for(int i=0;i<=delta and y1+i <= y2;i++)
	forward(x1+delta,y1+i);
  }
}


void DPmatrixSimple::forward(const vector<int>& path,double bandwidth) {
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
  forward(0,0);

  // Process the squares generated
  for(int i=0;i<pins.size()-1;i++) {
    forward(icol[pins[i]],jcol[pins[i]],
	    icol[pins[i+1]],jcol[pins[i+1]]);
  }
}

inline void DPmatrixHMM::forward(int x1,int y1,int x2,int y2) {
  assert(x1 < x2 or y1 < y2);
  assert(x2 < size1());
  assert(y2 < size2());

  const int maxdelta = std::max(x2-x1,y2-y1);

  for(int delta=1; delta<=maxdelta; delta++) {
    if (y1 + delta <= y2)
      for(int i=0;i<delta and x1+i <= x2;i++) 
	forward(x1+i,y1+delta);

    if (x1 + delta <= x2)
      for(int i=0;i<=delta and y1+i <= y2;i++)
	forward(x1+delta,y1+i);
  } 
}


void DPmatrixHMM::forward(const vector<int>& path,double bandwidth) {
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
  forward(0,0);

  // Process the squares generated
  for(int i=0;i<pins.size()-1;i++) {
    forward(icol[pins[i]],jcol[pins[i]],
	    icol[pins[i+1]],jcol[pins[i+1]]);
  }
}


