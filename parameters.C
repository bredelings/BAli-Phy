#include "parameters.H"
#include "logsum.H"


void IndelModel::construct_length_plus_p(int n) {
  using std::vector;

  vector<double>& f_E = p_length_plus;

  vector<double> f_M(n);
  vector<double> f_G2(n);
  f_E.resize(n);



  f_M[0] = pi[0];
  f_G2[0] = pi[2];
  f_E[0] = pi[3]; 

  for(int i=1;i<n;i++) {
    f_M[i] = logsum(f_M[i-1]+Q(0,0),f_G2[i-1]+Q(2,0));
    f_G2[i] = logsum(f_M[i-1]+Q(0,2),f_G2[i-1]+Q(2,2));
    f_E[i] = logsum(f_M[i]+Q(0,3),f_G2[i]+Q(2,3));
  }
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

IndelModel::IndelModel()
  : P(4,4),pi(4),Q(4,4)
{ }


IndelModel1::IndelModel1(int maxlength,double lambda_O,double lambda_E)
{
  epsilon = exp(lambda_E);
  delta   = exp(lambda_O)/(1.0-epsilon);
  tau     = 1.0e-3;

  assert(delta > 0.0);
  
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

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = 0;
  pi[1] = log_0;
  pi[2] = log_0;
  pi[3] = log_0;  // must be log_0

  //  pi[0] = log(1.0-delta-delta*(1.0-delta) );
  //  pi[1] = log(delta);
  //  pi[2] = log(delta *(1.0-delta) );
  //  pi[3] = log_0;  // must be log_0

  construct_lengthp(maxlength);
  construct_length_plus_p(maxlength);
}

IndelModel2::IndelModel2(int maxlength,double lambda_O,double lambda_E,double b)
{
  epsilon = exp(lambda_E);
  delta   = exp(lambda_O)/(1.0-epsilon);
  beta    = exp(b);
  tau     = 1.0e-3;

  assert(delta > 0.0);
  assert(beta*delta < 1.0);
  
  /* Chain w/o transitions to End state */
  P(0,0) = log(1.0 - 2.0*delta);
  P(0,1) = log(delta);
  P(0,2) = log(delta);
  P(0,3) = log_0;

  P(1,0) = log(1.0 - epsilon) + log(1.0 - beta*delta);
  P(1,1) = log(epsilon + (1.0-epsilon)*beta*delta/2.0);
  P(1,2) = log(1.0 - epsilon) + log(beta*delta/2.0);
  P(1,3) = log_0;

  P(2,0) = log(1.0 - epsilon) + log(1.0 - beta*delta);
  P(2,1) = log(1.0 - epsilon) + log(beta*delta/2.0);
  P(2,2) = log(epsilon + (1.0-epsilon)*beta*delta/2.0);
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

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = 0;
  pi[1] = log_0;
  pi[2] = log_0;
  pi[3] = log_0;  // must be log_0

  //  pi[0] = log(1.0-2.0*delta);
  //  pi[1] = log(delta);
  //  pi[2] = log(delta);
  //  pi[3] = log_0;  // must be log_0

  construct_lengthp(maxlength);
  construct_length_plus_p(maxlength);
}

SingleIndelModel::SingleIndelModel(int maxlength,double LO) {
  delta=LO;

  assert(delta > 0.0);
  tau     = 1.0e-3;

  /* Chain w/o transitions to End state */
  P(0,0) = log(1.0 - 2.0*delta);
  P(0,1) = log(delta);
  P(0,2) = log(delta);
  P(0,3) = log_0;

  P(1,0) = log(1.0 - 2.0*delta);
  P(1,1) = log(delta);
  P(1,2) = log(delta);
  P(1,3) = log_0;

  P(2,0) = log(1.0 - 2.0*delta);
  P(2,1) = log(delta);
  P(2,2) = log(delta);
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

  /* Initial Distribution */
  /* This is for the character before the first character - can't be E */
  pi[0] = 0;
  pi[1] = log_0;
  pi[2] = log_0;
  pi[3] = log_0;  // must be log_0

  construct_lengthp(maxlength);
  construct_length_plus_p(maxlength);
}

void Parameters::setlength(int b,double l) {
  assert(l >= 0);
  assert(b >= 0 and b < T.branches());
  T.branch(b).length() = l;
  for(int r=0;r<SModel().nrates();r++)
    transition_P_[r][b] = SModel_->transition_p(l,r);
}


void Parameters::recalc() {
  for(int b=0;b<T.branches();b++) 
    setlength(b,T.branch(b).length());
}


Parameters& Parameters::operator=(const Parameters& P) {
  transition_P_ = P.transition_P_;

  delete SModel_;
  SModel_ = P.SModel_->clone();

  IModel = P.IModel;

  T = P.T;

  branch_mean = P.branch_mean;

  return (*this);
}

Parameters::Parameters(const Parameters& P):
  transition_P_(P.transition_P_),SModel_(P.SModel_->clone()),IModel(P.IModel),T(P.T),
  branch_mean(P.branch_mean)
{ }


Parameters::Parameters(const substitution::MultiRateModel& SM,const IndelModel& IM,const SequenceTree& t)
  :SModel_(SM.clone()),IModel(IM),
   transition_P_(vector< vector <Matrix> >(SM.nrates(),
					  vector<Matrix>(t.branches(),
							 Matrix(SM.Alphabet().size(),
								SM.Alphabet().size()
								)
							 ) 
					  ) 
		 ),
   T(t)
{
  branch_mean = 0.1;
  recalc();

  
}
