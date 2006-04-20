#include <cmath>
#include "dp-array.H"
#include "pow2.H"
#include "choose.H"


efloat_t DParray::path_P(const vector<int>& g_path) const 
{
  const int I = size()-1;
  int i=I;
  int l=g_path.size()-1;
  int state2 = g_path[l];

  vector<double> transition(nstates());

  efloat_t Pr=1.0;
  while (l>0) 
  {
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)(i,state1)*GQ(state1,state2);

    int state1 = g_path[l-1];
    double p = choose_P(state1,transition);
    assert(p > 0.0);

    if (di(state1)) i--;

    l--;
    state2 = state1;
    Pr *= p;
  }

  // include probability of choosing 'Start' vs ---+ !
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)(0,state1) * GQ(state1,state2);

  // Get the probability that the previous state was 'Start'
  double p=0.0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p += choose_P(state1,transition);

  Pr *= p;

  assert(Pr > 0.0);
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

  vector<double> transition(nstates());

  while(i >= 0) {
    path.push_back(state2);
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)(i,state1)*GQ(state1,state2);

    int state1 = choose_scratch(transition);

    if (di(state1)) i--;

    state2 = state1;
  }
  assert(i+di(state2)==0);

  std::reverse(path.begin(),path.end());
  return path;
}

efloat_t DParray::Pr_sum_all_paths() const {
  const int I = size()-1;

  double total = 0.0;
  for(int state1=0;state1<nstates();state1++)
    total += (*this)(I,state1) * GQ(state1,endstate());

  return pow<efloat_t>(2.0,scale(I)) * total;
}

/// Here we set the SEQUENCE length 'l'.  This gives rise to a
/// DP array of length 'l+1'.
void DParray::set_length(int l)
{
  length = l+1;
  state_array::resize(length, nstates());

  // clear the beginning of the array
  for(int s=0;s<start_P.size();s++)
    (*this)(0,s) = start_P[s];

  Pr_total = 0;
}

DParray::DParray(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M,double Beta)
  :DPengine(v1,v2,M,Beta)
{ 
  set_length(l);

  for(int s=0;s<start_P.size();s++)
    (*this)(0,s) = start_P[s];
}

inline void DParrayConstrained::forward(int i2) 
{
  assert(i2<size());

  // determine initial scale for this cell
  if (i2==0)
    scale(i2) = 0;
  else
    scale(i2) = scale(i2-1);

  double maximum = 0;

  for(int s2=0;s2<states(i2).size();s2++) 
  {
    int S2 = states(i2)[s2];

    int i1 = i2;
    if (di(S2))
      i1--;

    //----- don't go off the boundary -----//
    if (i1<0) continue;

    //---- compute arrival probability ----//
    double temp = 0;
    const unsigned NS1 = states(i1).size();
    for(int s1=0;s1<NS1;s1++) {
      int S1 = states(i1)[s1];

      temp += (*this)(i1,S1) * GQ(S1,S2);
    }

    // record maximum 
    if (temp > maximum) maximum = temp;

    // store the result
    (*this)(i2,S2) = temp;
  }

  //------- if exponent is too low, rescale ------//
  if (maximum > 0 and maximum < fp_scale::cutoff) {
    int logs = -(int)log2(maximum);
    double scale_ = pow2(logs);
    for(int s2=0;s2<states(i2).size();s2++)  
    {
      int S2 = states(i2)[s2];
      (*this)(i2,S2) *= scale_;
    }
    scale(i2) -= logs;
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

void DParrayConstrained::set_length(int l)
{
  DParray::set_length(l);
  allowed_states.resize(size());
}

DParrayConstrained::DParrayConstrained(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M,double Beta)
  :DParray(l,v1,v2,M,Beta)
{ 
  allowed_states.resize(size());
}


