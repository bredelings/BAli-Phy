#include <cmath>
#include <climits>
#include "dp-matrix.H"
#include "pow2.H"
#include "choose.H"

using std::max;

void state_matrix::clear() {
  if (data) {
    delete data; 
    data = NULL;
  }

  if (scale_) {
    delete scale_; 
    scale_ = NULL;
  }
}

state_matrix::~state_matrix() {
  clear();
}

inline void DPmatrix::clear_cell(int i2,int j2) 
{
  scale(i2,j2) = INT_MIN;
  for(int S=0;S<nstates();S++)
    (*this)(i2,j2,S) = 0;
}

inline void DPmatrix::forward_first_cell(int i2,int j2) 
{ 
  assert(0 < i2 and i2 < size1());
  assert(0 < j2 and j2 < size2());

  // determine initial scale for this cell
  scale(i2,j2) = 0;

  double maximum = 0;

  for(int S2=0;S2<nstates();S2++) 
  {
    double temp;
    if (di(S2) or dj(S2))
      temp = start_P[S2];
    else {
      //--- compute arrival probability ----
      temp  = 0;
      for(int S1=0;S1<nstates();S1++)
	temp += (*this)(i2,j2,S1) * GQ(S1,S2);
    }

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

inline void DPmatrix::forward_square_first(int x1,int y1,int x2,int y2) {
  assert(0 < x1);
  assert(0 < y1);
  assert(x1 <= x2 or y1 <= y2);
  assert(x2 < size1());
  assert(y2 < size2());

  // Since we are using M(0,0) instead of S(0,0), we need to run only the silent states at (0,0)
  // We can only use non-silent states at (0,0) to simulate S

  // clear left border
  for(int y=y1;y<=y2;y++)
    clear_cell(x1-1,y);

  // forward first row, with exception for S(0,0)
  clear_cell(x1,y1-1);
  forward_first_cell(x1,y1);
  for(int y=y1+1;y<=y2;y++)
    forward_cell(x1,y);

  // forward other rows
  for(int x=x1+1;x<=x2;x++) {
    clear_cell(x,y1-1);
    for(int y=y1;y<=y2;y++)
      forward_cell(x,y);
  }
}

inline void DPmatrix::forward_square(int x1,int y1,int x2,int y2) {
  assert(0 < x1);
  assert(0 < y1);
  assert(x1 <= x2 or y1 <= y2);
  assert(x2 < size1());
  assert(y2 < size2());

  // Since we are using M(0,0) instead of S(0,0), we need to run only the silent states at (0,0)
  // We can only use non-silent states at (0,0) to simulate S

  // clear left border
  for(int y=y1;y<=y2;y++)
    clear_cell(x1-1,y);

  for(int x=x1;x<=x2;x++) {
    clear_cell(x,y1-1);
    for(int y=y1;y<=y2;y++)
      forward_cell(x,y);
  }
}

void DPmatrix::forward_square() {
  const int I = size1()-1;
  const int J = size2()-1;

  forward_square_first(1,1,I,J);

  // store total
  double total = 0.0;
  for(int state1=0;state1<nstates();state1++)
    total += (*this)(I,J,state1)*GQ(state1,endstate());

  Pr_total = pow<efloat_t>(2.0,scale(I,J)) * total;
}

// FIXME - fix up pins for new matrix coordinates
void DPmatrix::forward_constrained(const vector< vector<int> >& pins) 
{
  const int I = size1()-1;
  const int J = size2()-1;

  if (pins[0].size() == 0) 
    forward_square();
  else 
  {
    const vector<int>& x = pins[0];
    const vector<int>& y = pins[1];

    // Propogate from S to first pin
    forward_square_first(1,1,x[0],y[0]);

    // Propogate from first pin to other pins (if any)
    for(int i=0;i<(int)x.size()-1;i++)
      forward_square(x[i]+1,y[i]+1,x[i+1],y[i+1]);

    int p = x.size()-1;
    forward_square(x[p]+1,y[p]+1,I,J);
  }
  
  // store total
  double total = 0.0;
  for(int state1=0;state1<nstates();state1++)
    total += (*this)(I,J,state1)*GQ(state1,endstate());

  Pr_total = pow<efloat_t>(2.0,scale(I,J)) * total;
}

vector<int> DPmatrix::forward(const vector<vector<int> >& pins) 
{
  forward_constrained(pins);
  return sample_path();
}


efloat_t DPmatrix::path_P(const vector<int>& path) const 
{
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
  assert(i == 1 and j == 1);

  // include probability of choosing 'Start' vs ---+ !
  for(int state1=0;state1<nstates();state1++)
    transition[state1] = (*this)(1,1,state1) * GQ(state1,state2);

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
  while (i>=1 and j>=1) {
    path.push_back(state2);
    for(int state1=0;state1<nstates();state1++)
      transition[state1] = (*this)(i,j,state1)*GQ(state1,state2);

    int state1 = choose_scratch(transition);

    if (di(state1)) i--;
    if (dj(state1)) j--;

    state2 = state1;
  }
  assert(i+di(state2)==1 and j+dj(state2)==1);

  std::reverse(path.begin(),path.end());
  return path;
}

DPmatrix::DPmatrix(int i1,
		   int i2,
		   const vector<int>& v1,
		   const vector<double>& v2,
		   const Matrix& M,
		   double Beta)
  :DPengine(v1,v2,M,Beta),
   state_matrix(i1,i2,nstates())
{
  const int I = size1()-1;
  const int J = size2()-1;

  for(int state1=0;state1<nstates();state1++)
    (*this)(I,J,state1) = 0;
}

inline void DPmatrixNoEmit::forward_cell(int i2,int j2) 
{ 
  assert(0 < i2 and i2 < size1());
  assert(0 < j2 and j2 < size2());

  // determine initial scale for this cell
  scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );

  double maximum = 0;

  for(int S2=0;S2<nstates();S2++) 
  {
    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

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

// switching dists1[] to matrices actually made things WORSE!
inline double DPmatrixEmit::emitMM(int i,int j) const {
  return s12_sub(i,j);
}

inline double DPmatrixEmit::emitM_(int i,int) const {
  return s1_sub[i];
}

inline double DPmatrixEmit::emit_M(int,int j) const {
  return s2_sub[j];
}

inline double DPmatrixEmit::emit__(int,int) const {
  return 1.0;
}

efloat_t DPmatrixEmit::path_Q_subst(const vector<int>& path) const 
{
  efloat_t P_sub=1.0;
  int i=1,j=1;
  for(int l=0;l<path.size();l++) 
  {
    int state2 = path[l];
    if (di(state2))
      i++;
    if (dj(state2))
      j++;

    double sub;
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

void DPmatrixEmit::prepare_cell(int i,int j) 
{
  assert(i > 0);
  assert(j > 0);
  
  const Matrix& M1 = dists1[i];
  const Matrix& M2 = dists2[j];

  double total=0;
  for(int m=0;m<M1.size1();m++) {
    for(int l=0;l<M1.size2();l++)
      total += M1(m,l) * M2(m,l);
  }

  if (B != 1.0)
    total = pow(total,B);

  s12_sub(i,j) = total;
  //      s12_sub(i,j) = pow(s12_sub(i,j),1.0/T);
}

DPmatrixEmit::DPmatrixEmit(const vector<int>& v1,
			   const vector<double>& v2,
			   const Matrix& M,
			   double Beta,
			   const vector< double >& d0,
			   const vector< Matrix >& d1,
			   const vector< Matrix >& d2, 
			   const Matrix& f)
  :DPmatrix(d1.size(),d2.size(),v1,v2,M,Beta),
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
    s1_sub[i] = pow(total,B);
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
    s2_sub[i] = pow(total,B);
    //    s2_sub[i] = pow(s2_sub[i],1.0/T);
  }

  //----- pre-calculate scaling factors --------//
  for(int i=0;i<dists2.size();i++) {
    for(int m=0;m<nrates();m++)
      for(int l=0;l<dists2[i].size2();l++)
	dists2[i](m,l) *= distribution[m] * frequency(m,l);
  }
}


inline void DPmatrixSimple::forward_cell(int i2,int j2) 
{
  assert(0 < i2 and i2 < size1());
  assert(0 < j2 and j2 < size2());

  prepare_cell(i2,j2);

  // determine initial scale for this cell
  scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );

  double maximum = 0;

  for(int S2=0;S2<nstates();S2++) 
  {
    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

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

inline void DPmatrixConstrained::clear_cell(int i2,int j2) 
{
  scale(i2,j2) = INT_MIN;
  for(int S=0;S<nstates();S++)
    (*this)(i2,j2,S) = 0;
}

inline void DPmatrixConstrained::forward_cell(int i2,int j2) 
{
  assert(0 < i2 and i2 < size1());
  assert(0 < j2 and j2 < size2());

  prepare_cell(i2,j2);

  // determine initial scale for this cell
  scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );

  double maximum = 0;

  for(int i=0;i<states(j2).size();i++) 
  {
    int S2 = states(j2)[i];

    //--- Get (i1,j1) from (i2,j2) and S2
    int i1 = i2;
    if (di(S2)) i1--;

    int j1 = j2;
    if (dj(S2)) j1--;

    //--- Compute Arrival Probability ----
    double temp = 0.0;
    const unsigned NS1 = states(j1).size();
    for(int s=0;s<NS1;s++) {
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


vector<int> DPmatrixConstrained::sample_path() const 
{
  vector<int> path;

  const int I = size1()-1;
  const int J = size2()-1;

  // we are at S1(i,j) coming from S2, and see to determine S1
  int S2 = endstate();
  int i = I;
  int j = J;

  vector<double> transition(nstates());

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - check that we came from (0,0) though
  while (i>=1 and j>=1) 
  {
    path.push_back(S2);

    transition.resize(states(j).size());
    for(int s1=0;s1<states(j).size();s1++) 
    {
      int S1 = states(j)[s1];
      transition[s1] = (*this)(i,j,S1)*GQ(S1,S2);
    }

    int s1 = choose_scratch(transition);
    int S1 = states(j)[s1];

    if (di(S1)) i--;
    if (dj(S1)) j--;

    S2 = S1;
  }
  assert(i+di(S2)==1 and j+dj(S2)==1);

  std::reverse(path.begin(),path.end());
  return path;
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

      // FIXME! this assumes that dj(s2) is true.

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
