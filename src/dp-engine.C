#include <cmath>
#include "dp-engine.H"
#include "logsum.H"
#include "choose.H"
#include "rng.H"

#include "pow2.H"

using std::abs;
using std::max;

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

  return powe<efloat_t>(2.0,scale(I)) * total;
}

DParray::DParray(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M,double Temp)
  :DPengine(v1,v2,M,Temp),
   state_array(l+1,nstates()),
   length(l+1)
{ 
  for(int s=0;s<start_P.size();s++)
    (*this)(0,s) = start_P[s];

  scale(0) = 0;
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
    for(int s1=0;s1<states(i1).size();s1++) {
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

DParrayConstrained::DParrayConstrained(int l,const vector<int>& v1,const vector<double>& v2,const Matrix& M,double Temp)
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


efloat_t DPmatrix::path_check(const vector<int>& path) const 
{
  efloat_t Pr=1.0;
  
  const int I = size1()-1;
  const int J = size2()-1;

  int i = 0;
  int j = 0;

  int l = 0;

  vector<double> transition(nstates());

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

    for(int s=0;s<nstates();s++)
      transition[s] = (*this)(i,j,s)*GQ(s,state2);
    
    double p = choose_P(state1,transition);
    assert((*this)(i,j,state1) > 0.0);
    assert(GQ(state1,state2) > 0.0);
    assert(p > 0.0);
    
    l++;
    Pr *= p;

    assert(i<=I and j<=J);
  }

  assert(l == path.size()-1);
  assert(i == I and j == J);
  assert(Pr > 0.0);

  return Pr;
}

efloat_t DPmatrix::path_P(const vector<int>& path) const {
  double P2 = path_check(path);
  std::cerr<<"P(path)2 = "<<P2<<std::endl;

  const int I = size1()-1;
  const int J = size2()-1;
  int i = I;
  int j = J;
  efloat_t Pr=1.0;

  int l = path.size()-1;
  int state2 = path[l];

  vector<double> transition(nstates());

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - but we would have to check path[-1] to see which state
  //   made sample_path go negative
  // - instead we can check if l==0 - we know that the start state
  //   is at path[-1]
  while (l>0) {

    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)(i,j,state1)*GQ(state1,state2);

    int state1 = path[l-1];
    double p = choose_P(state1,transition);
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
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)(0,0,state1) * GQ(state1,state2);

  // Get the probability that the previous state was 'Start'
  double p=0.0;
  for(int state1=0;state1<nstates();state1++)  
    if (not silent(state1))
      p += choose_P(state1,transition);

  Pr *= p;

  assert(Pr > 0.0);
  std::cerr<<"P(path) = "<<log(Pr)<<std::endl;
  return Pr;
}

vector<int> DPmatrix::sample_path() const 
{
  vector<int> path;

  const int I = size1()-1;
  const int J = size2()-1;
  int i = I;
  int j = J;

  int state2 = endstate();

  vector<double> transition(nstates());

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - check that we came from (0,0) though
  while (i>=0 and j>=0) {
    path.push_back(state2);
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)(i,j,state1)*GQ(state1,state2);

    int state1 = choose_scratch(transition);

    if (di(state1)) i--;
    if (dj(state1)) j--;

    state2 = state1;
  }
  assert(i+di(state2)==0 and j+dj(state2)==0);

  std::reverse(path.begin(),path.end());
  return path;
}

efloat_t DPmatrix::Pr_sum_all_paths() const {
  const int I = size1()-1;
  const int J = size2()-1;

  double total = 0.0;
  for(int state1=0;state1<nstates();state1++)
    total += (*this)(I,J,state1)*GQ(state1,endstate());

  return powe<efloat_t>(2.0,scale(I,J)) * total;
}


DPmatrix::DPmatrix(int i1,
		   int i2,
		   const vector<int>& v1,
		   const vector<double>& v2,
		   const Matrix& M,
		   double Temp)
  :DPengine(v1,v2,M,Temp),
   state_matrix(i1+1,i2+1,nstates()),
   S1(i1+1),
   S2(i2+1)
{

  //----- zero-initialize matrices ------//
  for(int i=0;i<size1();i++)
    for(int j=0;j<size2();j++) 
      for(int S=0;S<nstates();S++)
	(*this)(i,j,S)  = 0;

  //----- set up start probabilities -----//
  for(int S=0;S<start_P.size();S++)
    (*this)(0,0,S) = start_P[S];
}

inline void DPmatrixNoEmit::forward_cell(int i2,int j2,int x1,int y1) 
{ 
  assert(i2<size1());
  assert(j2<size2());

  // determine initial scale for this cell
  if (i2 > 0 and j2 > 0)
    scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );
  else if (i2 > 0)
    scale(i2,j2) = scale(i2-1,j2);
  else if (j2 > 0)
    scale(i2,j2) = scale(i2,j2-1);

  double maximum = 0;

  for(int S2=0;S2<nstates();S2++) 
  {
    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- don't go off the boundary -----
    if (i1<0 or j1<0)
      continue;

    //--- compute arrival probability ----
    double temp  = 0;
    for(int S1=0;S1<nstates();S1++)
      temp += (*this)(i1,j1,S1) * GQ(S1,S2);

    // rescale result to scale of this cell
    if (scale(i1,j1) != scale(i2,j2))
      temp *= pow2(scale(i1,j1)-scale(i2,j2));

    // record maximum
    if (temp > maximum) maximum = temp;

    // store the result
    (*this)(i2,j2,S2) = temp;
  }

  //------- if exponent is too low, rescale ------//
  if (maximum > 0 and maximum < fp_scale::cutoff) {
    int logs = -(int)log2(maximum);
    double scale_ = pow2(logs);
    for(int S2=0;S2<nstates();S2++) 
      (*this)(i2,j2,S2) *= scale_;
    scale(i2,j2) -= logs;
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
  return 1.0;
}

efloat_t DPmatrixEmit::path_Q_subst(const vector<int>& path) const {
  efloat_t P_sub=1.0;
  int i=0,j=0;
  for(int l=0;l<path.size();l++) {

    int state2 = path[l];
    if (di(state2))
      i++;
    if (dj(state2))
      j++;

    double sub=0.0;
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
  return P_sub;
}


DPmatrixEmit::DPmatrixEmit(const vector<int>& v1,
			   const vector<double>& v2,
			   const Matrix& M,
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
    //    s1_sub[i] = pow(s1_sub[i],1.0/T);
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
    //    s2_sub[i] = pow(s2_sub[i],1.0/T);
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
      //      s12_sub(i,j) = pow(s12_sub(i,j),1.0/T);
    }

}


inline void DPmatrixSimple::forward_cell(int i2,int j2,int x1, int y1) {

  assert(i2<size1());
  assert(j2<size2());

  // determine initial scale for this cell
  if (i2 > 0 and j2 > 0)
    scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );
  else if (i2 > 0)
    scale(i2,j2) = scale(i2-1,j2);
  else if (j2 > 0)
    scale(i2,j2) = scale(i2,j2-1);

  double maximum = 0;

  for(int S2=0;S2<nstates();S2++) 
  {
    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0) 
      continue;

    //--- Compute Arrival Probability ----
    double temp  = 0;
    for(int S1=0;S1<nstates();S1++)
      temp += (*this)(i1,j1,S1) * GQ(S1,S2);

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

    temp *= sub;

    // rescale result to scale of this cell
    if (scale(i1,j1) != scale(i2,j2))
      temp *= pow2(scale(i1,j1)-scale(i2,j2));

    // record maximum
    if (temp > maximum) maximum = temp;

    // store the result
    (*this)(i2,j2,S2) = temp;
  }

  //------- if exponent is too low, rescale ------//
  if (maximum > 0 and maximum < fp_scale::cutoff) {
    int logs = -(int)log2(maximum);
    double scale_ = pow2(logs);
    for(int S2=0;S2<nstates();S2++) 
      (*this)(i2,j2,S2) *= scale_;
    scale(i2,j2) -= logs;
  }
} 

inline void DPmatrixConstrained::forward_cell(int i2,int j2,int x1,int y1) {

  assert(i2<size1());
  assert(j2<size2());

  // determine initial scale for this cell
  if (i2 > 0 and j2 > 0)
    scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );
  else if (i2 > 0)
    scale(i2,j2) = scale(i2-1,j2);
  else if (j2 > 0)
    scale(i2,j2) = scale(i2,j2-1);

  double maximum = 0;

  for(int i=0;i<states(j2).size();i++) 
  {
    int S2 = states(j2)[i];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Don't go off the boundary -----
    if (i1<0 or j1<0) 
      continue;

    //--- Compute Arrival Probability ----
    double temp = 0.0;
    for(int s=0;s<states(j1).size();s++) {
      int S1 = states(j1)[s];
      temp +=  (*this)(i1,j1,S1) * GQ(S1,S2);
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

    temp *= sub;

    // rescale result to scale of this cell
    if (scale(i1,j1) != scale(i2,j2))
      temp *= pow2(scale(i1,j1)-scale(i2,j2));

    // record maximum
    if (temp > maximum) maximum = temp;

    // store the result
    (*this)(i2,j2,S2) = temp;
  }

  //------- if exponent is too low, rescale ------//
  if (maximum > 0 and maximum < fp_scale::cutoff) {
    int logs = -(int)log2(maximum);
    double scale_ = pow2(logs);
    for(int i=0;i<states(j2).size();i++) {
      int S2 = states(j2)[i];
      (*this)(i2,j2,S2) *= scale_;
    }
    scale(i2,j2) -= logs;
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
