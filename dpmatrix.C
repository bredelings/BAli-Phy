#include "dpmatrix.H"
#include "logsum.H"
#include "choose.H"
#include "rng.H"
#include "inverse.H"

using std::abs;

// Is this state silent and in a loop of silent states?
bool HMM::silent_network(int S) const {
  assert(S <= nstates()+1);
  return (silent_network_[S] != -1);
}

/// Replace a sequence of silent_network states with just
/// the first state in the sequence, producing a path that
/// would be emitted from the generalized HMM.
vector<int> HMM::generalize(const vector<int>& path) const {
  vector<int> g_path = path;
  for(int i=g_path.size()-1;i>0;i--) {
    int S1 = g_path[i-1];
    int S2 = g_path[i];
    if (silent_network(S1) and silent_network(S2))
      g_path.erase(g_path.begin()+i);
  }
  return g_path;
}

/// Expand generalized states (i.e. silent network states in the
/// generalized HMM) into a sequence of silent network states, as
/// we might observe in the normal HMM.  This is a random process,
/// and the distribution is chosen so that sampling from the generalized
/// HMM and then ungeneralizing is like sampling from the ungeneralized
/// HMM. (But we can't sample from the ungeneralized HMM using DP)
vector<int> HMM::ungeneralize(const vector<int>& g_path) const {
  vector<int> path = g_path;

  // Go backwards along the path, expanding silent states
  for(int i=path.size()-1;i>0;i--) {
    int S1 = path[i];

    if (silent_network(S1)) {
      int S_end = path[i+1];
      vector<int> extra_states;

      // Choose the extra states... S1->S2->.....>S_end
      do {
	vector<double> P(silent_network_states.size()+1);
	for(int s2=0;s2<silent_network_states.size();s2++) {
	  int S2 = silent_network_states[s2];
	  P[s2] = Q(S1,S2) + GQ(S2,S_end);
	}
	P[silent_network_states.size()] = Q(S1,S_end);

	int next = choose(P);
	if (next == silent_network_states.size())
	  break;
	else
	  extra_states.push_back(silent_network_states[next]);
      } while (1);

      // Insert the extra states after the current position
      path.insert(path.begin()+i+1,extra_states.begin(),extra_states.end());
    }
  }

  return path;
}

double HMM::generalize_P_one(vector<int>::const_iterator s1,int n) const {
  double Pr = 0;

  int S_end = *(s1 + n);
  assert(not silent_network(S_end));

  for(int i=0;i<n;i++) {
    int S1 = *(s1 + i);

    vector<double> P(silent_network_states.size()+1);
    for(int s2=0;s2<silent_network_states.size();s2++) {
      int S2 = silent_network_states[s2];
      P[s2] = Q(S1,S2) + GQ(S2,S_end);
    }
    P[silent_network_states.size()] = Q(S1,S_end);
    int choice = silent_network_states.size();
    if (i+1 != n)
      choice = silent_network_[ *(s1+i+1) ];
    Pr += choose_P(choice,P);
  }
  return Pr;
}

/// What is the probability that the generalized path corresponding to this
/// path would emit this ungeneralized path?
double HMM::generalize_P(const vector<int>& path) const {
  double Pr = 0;
  for(int i=0; i<path.size()-1; i++) {
    if (silent_network(path[i])) {
      int start = i;
      // look along until we find a state not in the silent network (terminates at E)
      while(silent_network(path[i]))
	i++;
      Pr += generalize_P_one(path.begin()+start,i-start);
    }
  }
  return Pr;
}
//      if (silent_network(S2))
//	Pr += Q(S1,S2);
//      else
//	Pr += log(1.0-exp(Q(S1,S1)));

double HMM::path_Q_path(const vector<int>& g_path) const {

  double Pr = log_0;
  for(int S=0;S<nstates();S++)
    if (not silent(S))
      Pr = logsum(Pr,start_P[S] + GQ(S,g_path[0]));

  for(int l=1;l<g_path.size();l++)
    Pr += GQ(g_path[l-1],g_path[l]);

  return Pr;
}


HMM::HMM(const vector<int>& v1,const vector<double>& v2,const Matrix& M)
  :silent_network_(v1.size()),Q(M),GQ(M),
   start_P(v2),state_emit(v1) 
{
  assert(start_P.size() == nstates());

  // Compute which nodes are part of the silent network

  //  o First see if silent nodes are connected to silent nodes
  for(int S1=0;S1<nstates()+1;S1++) {
    if (state_emit[S1])
      silent_network_[S1] = -1;
    else {
      bool connected=false;
      for(int S2=0;S2<nstates()+1;S2++) {
	if (Q(S1,S2) > log_0/100 and not state_emit[S2]) {
	  connected=true;
	  break;
	}
      }
      silent_network_[S1] = connected?1:-1;
    }
  }
  silent_network_[endstate()] = -1;

  //  o Then see if some of these silent nodes aren't actually part of the network
  bool changed=true;
  while(changed) {
    changed=false;
    for(int S1=0;S1<nstates()+1;S1++) {
      if (silent_network_[S1] == -1) continue;

      bool connected = false;
      for(int S2=0;S2<nstates()+1;S2++) {
	if (Q(S1,S2) > log_0/100 and silent_network_[S2]) {
	  connected = true;
	}
      }
      silent_network_[S1] = connected?1:-1;
      if (not connected) changed = true;
    }
  }

  // Compute the backwards mapping: silent_network_ -> silent_network_states
  for(int S1=0;S1<nstates();S1++) {
    if (silent_network(S1)) {
      silent_network_states.push_back(S1);
      silent_network_[S1] = silent_network_states.size()-1;  
    }
  }

  // FIXME - If there are silent states that aren't part of the silent network,
  // then their order matters - they must be before states they lead to.
  // We don't deal with that in computing the state ordering yet (see below) :

  // Compute the state order
  vector<int> temp;
  for(int S1=0;S1<nstates()+1;S1++) {
    // we haven't implemented this case yet - see FIXME above
    assert(not silent(S1) or silent_network(S1) or S1 == endstate());
    if (silent(S1))
      temp.push_back(S1);
    else
      order_.push_back(S1);
  }
  order_.insert(order_.end(),temp.begin(),temp.end());


  // Compute the Generalized Transition Matrix, if we've got silent_network states
  if (silent_network_states.size()) {

    // construct a mapping from node index to non-silent-node index
    vector<int> non_silent_network;
    for(int S=0;S<nstates()+1;S++)
      if (not silent_network(S))
	non_silent_network.push_back(S);

    typedef ublas::matrix<double,ublas::column_major> MatrixC;
    // Solve equation G = Q2 + Q1*G for G; IMQ1 = I - Q1
    MatrixC IMQ1(silent_network_states.size(),silent_network_states.size());
    MatrixC Q2(silent_network_states.size(),non_silent_network.size());
    
    // fill IMQ1 = I - Q1
    for(int i=0;i<silent_network_states.size();i++)
      for(int j=0;j<silent_network_states.size();j++) {
	IMQ1(i,j) = - exp( Q(silent_network_states[i],silent_network_states[j]) );
	if (i==j) IMQ1(i,j) += 1.0;
      }
    
    // fill Q2
    for(int i=0;i<silent_network_states.size();i++)
      for(int a=0;a<non_silent_network.size();a++)
	Q2(i,a) = exp ( Q(silent_network_states[i],non_silent_network[a]) );
    
    Matrix G = solve(IMQ1,Q2);
    
    // Actually modify the elements of GQ
    for(int i=0;i<silent_network_states.size();i++) {
      int S1 = silent_network_states[i];
      // silent network -> not silent network
      for(int j=0;j<non_silent_network.size();j++) {
	int S2 = non_silent_network[j];
	if (Q(S1,S2) < log_0/100)
	  GQ(S1,S2) = log_0;
	else {
	  assert(G(i,j) > 0);
	  GQ(S1,S2) = log(G(i,j));
	}
      }
      // silent network -> silent network (not allowed)
      for(int j=0;j<silent_network_states.size();j++) {
	int S2 = silent_network_states[j];
	GQ(S1,S2) = log_0;
      }
    }
  }
}

double HMM::check(const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2) const {  

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
    std::cerr<<"LP1 = "<<lp1<<"     LP2 = "<<lp2<<"   LS2 - LS1 = "<<lp2-lp1<<endl;
    std::cerr<<"QP1 = "<<qp1<<"     QP2 = "<<qp2<<"   QS2 - QS1 = "<<qp2-qp1<<endl;
    std::cerr<<endl;
  }

  return diff;
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
		   const Matrix& M)
  :HMM(v1,v2,M),
   vector<Matrix>(nstates(),Matrix(i1+1,i2+1)),
   S1(i1+1),
   S2(i2+1)
{
  //----- zero-initialize matrices ------//
  for(int i=0;i<size1();i++)
    for(int j=0;j<size2();j++) 
      for(int S=0;S<nstates();S++)
	(*this)[S](i,j)  = log_0;

  //----- set up start probabilities -----//
  for(int S=0;S<start_P.size();S++)
    (*this)[S](0,0) = start_P[S];
}

void DPmatrixNoEmit::forward(int x1,int y1,int x2,int y2) {
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
			   const vector< double >& d0,
			   const vector< vector< valarray<double> > >& d1,
			   const vector< vector< valarray<double> > >& d2, 
			   const valarray<double>& f)
  :DPmatrix(d1.size(),d2.size(),v1,v2,M),
   s1_sub(d1.size()),s2_sub(d2.size()),
   distribution(d0),
   dists1(d1),dists2(d2),frequency(f)
{
  
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


