#include <fstream>
#include "parameters.H"
#include "exponential.H"
#include "logsum.H"


void EquilibriumModel::recalc() {

  // Determine diagonal entries
  for(int i=0;i<S.size1();i++) {
    double sum=0;
    for(int j=0;j<S.size2();j++) {
      if (i==j) continue;
      sum += S(i,j);
    }
    S(i,i) = -sum;
  }

  // Move from 'S' to 'S+F'
  for(int i=0;i<S.size1();i++)
    for(int j=0;j<S.size2();j++)
      Q(i,j) = S(i,j)*pi[j];


  // Rescale so expected that mutation rate is 1
  double scale=0;
  for(int i=0;i<S.size1();i++) 
    scale += rates()(i,i)*pi[i];

  Q /= -scale;

  std::cerr<<"scale1 = "<<scale<<endl;

  scale=0;
  for(int i=0;i<S.size1();i++) 
    scale += rates()(i,i)*pi[i];

  std::cerr<<"scale2 = "<<scale<<endl;

}

Matrix EquilibriumModel::transition_p(double t) const {
  BMatrix D(a->size(),a->size());
  for(int i=0;i<a->size();i++)
    D(i,i) = pi[i];
  return exp(S,D,t);
}

void HKY::recalc() {
  assert(a->size()==4);

  const int A = (*a)['A'];
  const int G = (*a)['G'];
  const int C = (*a)['C'];
  const int T = (*a)['T'];

  S(A,G) = kappa();
  S(A,C) = 1;
  S(A,T) = 1;

  S(G,A) = kappa();
  S(G,C) = 1;
  S(G,T) = 1;

  S(C,A) = 1;
  S(C,G) = 1;
  S(C,T) = kappa();

  S(T,A) = 1;
  S(T,G) = 1;
  S(T,C) = kappa();

  EquilibriumModel::recalc();
}

void EQU::recalc() {
  for(int i=0;i<a->size();i++)
    for(int j=0;j<a->size();j++)
      S(i,j) = 1;

  EquilibriumModel::recalc();
}

void Empirical::recalc() {
  EquilibriumModel::recalc();
}

void Empirical::load_file(const char* filename) {
  std::ifstream ifile(filename);
  for(int i=0;i<a->size();i++)
    for(int j=0;j<i;j++) {
      ifile>>S(i,j);
      S(j,i) = S(i,j);
    }

  for(int i=0;i<a->size();i++)
    ifile>>pi[i];
}

void IndelModel::construct_lengthp(int n) {
  using std::vector;

  vector< vector<double> > p1;
  p1.push_back( vector<double>(n,log_0) );
  p1.push_back( vector<double>(n,log_0) );
  p1.push_back( vector<double>(n,log_0) );
  p1.push_back( vector<double>(n,log_0) );

  vector< vector<double> > p2 = p1;

  p1[0][0] = pi[0];
  p1[1][0] = pi[1];
  p1[2][0] = pi[2];
  p1[3][0] = pi[3];

  for(int t=1;t<n;t++) {

    p2[1][0] = log_0;
    for(int s1=0;s1<4;s1++) 
      p2[1][0] = logsum(p2[1][0],p1[s1][0] + Q[s1][1]);

    p2[3][0] = log_0;
    for(int s1=0;s1<4;s1++) 
      p2[3][0] = logsum(p2[3][0],p1[s1][0] + Q[s1][3]);

    for(int L=1;L<=t;L++) {
      for(int s2=0;s2<4;s2++) {
	p2[s2][L] = log_0;
	for(int s1=0;s1<4;s1++) {
	  if (s2==0 or s2==2)
	    p2[s2][L] = logsum(p2[s2][L],p1[s1][L-1]+Q[s1][s2]);
	  else
	    p2[s2][L] = logsum(p2[s2][L],p1[s1][L]  +Q[s1][s2]);
	}
      }
    }

    if (t%100 == 0) {
      vector<double> totals(4);
      for(int s=0;s<4;s++) {
	totals[s] = log_0;
	for(int i=0;i<p1[s].size();i++)
	  totals[s] = logsum(totals[s],p1[s][i]);
	totals[s] = exp(totals[s]);
      }
      
      std::cerr<<"t = "<<t-1<<
	"  totalM = "<<totals[0]<<
	"  totalG1 = "<<totals[1]<<
	"  totalG2 = "<<totals[2]<<
	"  totalE = "<<totals[3]<<
	"  total = "<<totals[0]+totals[1]+totals[2]+totals[3]<<
	std::endl;
    }

    p1 = p2;
    for(int i=0;i<4;i++) {
      p1[i][n-2] = logsum(p1[i][n-2],p1[i][n-1]);
      p1[i][n-1] = log_0;
    }

  }

  p_length = p1[3];
  for(int i=0;i<p_length.size();i++)
    std::cerr<<i<<"  "<<p_length[i]<<std::endl;

}

IndelModel::IndelModel(double LO,double LE)
  : pi(4),P(4,4),Q(4,4),R(4,4)
  //,p_length(10000,0) 
{

  lambda_O = LO;
  lambda_E = LE;

  delta   = exp(lambda_O);
  epsilon = exp(lambda_E);
  tau     = 1.0e-3;

  /* Chain w/o transitions to End state */
  P(0,0) = log(1.0-delta-delta*(1.0-delta) );
  P(0,1) = log(delta);
  P(0,2) = log(delta *(1.0-delta) );
  P(0,3) = log_0;

  P(1,0) = log(1.0 - epsilon) + log(1.0 - delta);
  P(1,1) = log(epsilon);
  P(1,2) = log(1.0 - epsilon) + log(delta);
  P(1,3) = log_0;

  P(2,0) = log(1.0 - epsilon);
  P(2,1) = log_0;
  P(2,2) = log(epsilon);
  P(2,3) = log_0;

  P(3,0) = log_0;
  P(3,1) = log_0;
  P(3,2) = log_0;
  P(3,3) = 0;

  /* Chain with transitions to End state */
  Q = P;
  for(int i=0;i<3;i++) {
    for(int j=0;j<3;j++) 
      Q(i,j) += log(1.0 - tau);
    Q(i,3) = log(tau);
  }

  /* Chain w/o transitions to M or G1 states */
  R = Q;
  for(int i=0;i<3;i++) {
    R(i,0) = log_0;
    R(i,2) = log_0;
    double sum = logsum(R(i,1),R(i,3));
    R(i,1) -= sum;
    R(i,3) -= sum;
  }

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = log(1.0-delta-delta*(1.0-delta) );
  pi[1] = log(delta);
  pi[2] = log(delta *(1.0-delta) );
  pi[3] = log_0;  // must be log_0

  construct_lengthp(1000);
}

void Parameters::setlength(int b,double l) {
  assert(l >= 0);
  assert(b >= 0 and b < T.branches());
  T.branch(b).length() = l;
  substitution_[b] = SModel->transition_p(T.branch(b).length());
}


void Parameters::recalc() {
  substitution_.clear();
  for(int b=0;b<T.branches();b++) 
    substitution_.push_back(SModel->transition_p(T.branch(b).length()));
}


Parameters::Parameters(SubstitutionModel& SM,double lambda_O,double lambda_E,const SequenceTree& t)
  :SModel(&SM),IModel(lambda_O,lambda_E),T(t)
{
  branch_mean = 1.0;
  recalc();
}
