#include "dpmatrix.H"
#include "logsum.H"
#include "choose.H"
#include "rng.H"

using std::abs;

void DPmatrix::forward(int x1,int y1,int x2,int y2,const Matrix& GQ) {
  const int maxdelta = std::max(x2-x1,y2-y1);

  for(int delta=1; delta<=maxdelta; delta++) {
    if (delta<size2())
      for(int i=0;i<delta and i<size1();i++) 
	forward(x1+i,y1+delta,GQ);

    if (delta<size1())
      for(int i=0;i<=delta and i<size2();i++)
	forward(x1+delta,y1+i,GQ);
  } 
}

void DPmatrix::forward(const vector<int>& path,int bandwidth,const Matrix& GQ) {
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
    column += geometric(bandwidth)+1;
  }
  pins.push_back(icol.size()-1);

  // Deal with silent states at (0,0)
  forward(0,0,GQ);

  // Process the squares generated
  for(int i=0;i<pins.size()-1;i++) {
    forward(icol[pins[i]],jcol[pins[i]],
	    icol[pins[i+1]],jcol[pins[i+1]],GQ);
  }
}


// Is this state silent and in a loop of silent states?
bool DPmatrix::silent_network(int S) {
  if (S==endstate())
    return false;

  if (state_emit[S])
    return false;
  else
    return true;
}

vector<int> DPmatrix::generalize(const vector<int>& path) {
  vector<int> path_g = path;
  for(int i=path_g.size()-1;i>0;i--) {
    int S1 = path_g[i-1];
    int S2 = path_g[i];
    if (silent_network(S1) and silent_network(S2))
      path_g.erase(path_g.begin()+i);
  }
  return path_g;
}

// FIXME - this doesn't deal with silent networks that have more than
//         one silent state!
double DPmatrix::generalize_P(const vector<int>& path, const Matrix& Q) {
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

vector<double> DPmatrix::path_Q(const vector<int>& path,const Matrix& GQ) {
  double P_path=0;
  double P_sub=0;
  int i=0,j=0;
  for(int l=0;l<path.size();l++) {

    int state2 = path[l];
    if (di(state2))
      i++;
    if (dj(state2))
      j++;

    if (l == 0) {
      double sum=log_0;
      for(int S=0;S<nstates();S++)
	if (S != 7)
	  sum = logsum(sum,(*this)[S](0,0)+GQ(S,state2));
      P_path += sum;
    }
    else {
      P_path += GQ(path[l-1],state2);
    }

    double sub=0;
    if (di(state2) and dj(state2))
      sub = log(sum(dists1[i-1] * frequency * dists2[j-1]));
    else if (di(state2))
      sub = s1_sub[i-1];
    else if (dj(state2))
      sub = s2_sub[j-1];

    P_sub += sub;
  }
  assert(i == size1()-1 and j == size2()-1);
  vector<double> p;
  p.push_back(P_path);
  p.push_back(P_sub);
  return p;
}


double DPmatrix::path_check(const vector<int>& path, const Matrix& GQ) {
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

double DPmatrix::path_P(const vector<int>& path, const Matrix& GQ) {
  double P2 = path_check(path,GQ);
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
    transition[state1] = (*this)[state1](0,0)+GQ(state1,state2);

  double p=log_0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p = logsum(p, choose_P(state1,transition) );

  Pr += p;

  assert(Pr > log_0);
  std::cerr<<"P(path) = "<<Pr<<std::endl;
  return Pr;
}

vector<int> DPmatrix::sample_path(const Matrix& GQ) {
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

double DPmatrix::check(const Matrix& Q,const Matrix& GQ,const vector<int>& path1,const vector<int>& path2,double lp1,double ls1,double lp2,double ls2) {  

  // Add up the full likelihoods
  double l1 = lp1 + ls1;
  double l2 = lp2 + ls2;
  
  // get the probabilities of sampling each of the paths
  vector<int> path1_G = generalize(path1);
  vector<int> path2_G = generalize(path2);

  double p1 = path_P(path1_G,GQ); 
  double p2 = path_P(path2_G,GQ); 

  p1 += generalize_P(path1,Q);
  p2 += generalize_P(path2,Q);

  // get the probabilities of the path through the 3-way HMM
  vector<double> QP = path_Q(path1_G,GQ);
  double qp1 = QP[0] + generalize_P(path1,Q);
  double qs1 = QP[1];
  double q1 = qp1 + qs1;

  QP = path_Q(path2_G,GQ);
  double qp2 = QP[0] + generalize_P(path2,Q);
  double qs2 = QP[1];
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
