#include "dpmatrix.H"
#include "logsum.H"
#include "choose.H"

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
  const int endstate = nstates();

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

    if (state1 == endstate)
      break;

    int state2 = path[l+1];

    vector<double> transition(nstates());
    for(int s=0;s<nstates();s++)
      transition[s] = (*this)[s](i,j)+GQ(s,state2);

    double p = choose_P(state1,transition);
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

