#include "parameters.H"
#include "exponential.H"
#include "logsum.H"


void HKY::recalc() {
  assert(a->size()==4);

  const std::valarray<double>& pi = frequencies_;

  const int A = (*a)['A'];
  const int G = (*a)['G'];
  const int C = (*a)['C'];
  const int T = (*a)['T'];

  rates_(A,G) = kappa()*pi[G];
  rates_(A,C) = pi[C];
  rates_(A,T) = pi[T];

  rates_(G,A) = kappa()*pi[A];
  rates_(G,C) = pi[C];
  rates_(G,T) = pi[T];

  rates_(C,A) = pi[A];
  rates_(C,G) = pi[G];
  rates_(C,T) = kappa()*pi[T];

  rates_(T,A) = pi[A];
  rates_(T,G) = pi[G];
  rates_(T,C) = kappa()*pi[C];

  for(int i=0;i<4;i++) {
    double sum=0;
    for(int j=0;j<4;j++) {
      if (i==j) continue;
      sum += rates_(i,j);
    }
    rates_(i,i) = -sum;
  }

  double scale=0;
  for(int i=0;i<4;i++) 
    scale += rates()(i,i)*pi[i];

  rates_ /= -scale;

  std::cerr<<"scale1 = "<<scale<<endl;

  scale=0;
  for(int i=0;i<4;i++) 
    scale += rates()(i,i)*pi[i];

  std::cerr<<"scale2 = "<<scale<<endl;
}

void HKY::fiddle() {
}

Matrix HKY::transition_p(double t) const {
  return exp(rates_,t);
}

void IndelModel::construct_lengthp(int n) {
  std::vector<double> pM_A(n,log_0);
  std::vector<double> pG1_A(n,log_0);
  std::vector<double> pG2_A(n,log_0);
  std::vector<double> pE_A(n,log_0);
  
  std::vector<double> pM_B(n,log_0);
  std::vector<double> pG1_B(n,log_0);
  std::vector<double> pG2_B(n,log_0);
  std::vector<double> pE_B(n,log_0);

  pM_A[0]  = pi_M;
  pG1_A[0] = pi_G1;
  pG2_A[0] = pi_G2;

  for(int t=1;t<n;t++) {

    pG2_B[0] = logsum(pM_A[0]  +pM_G2, pG1_A[0]  +pG1_G2, pG2_A[0]+pG2_G2);
    double pE_0 = logsum(pM_A[0]  +pM_E,  pG1_A[0]  +pG1_E,  pG2_A[0]  +pG2_E);
    pE_B[0] = logsum(pE_B[0],pE_0);

    for(int i=1;i<n;i++) {
      pM_B[i]  = logsum(pM_A[i-1]+pM_M,  pG1_A[i-1]+pG1_M,  pG2_A[i-1]+pG2_M );
      pG1_B[i] = logsum(pM_A[i-1]+pM_G1, pG1_A[i-1]+pG1_G1, pG2_A[i-1]+pG2_G1);
      pG2_B[i] = logsum(pM_A[i]  +pM_G2, pG1_A[i]  +pG1_G2, pG2_A[i]  +pG2_G2);

      double pE_i 
               = logsum(pM_A[i]  +pM_E,  pG1_A[i]  +pG1_E,  pG2_A[i]  +pG2_E);
      pE_B[i]  = logsum(pE_B[i],pE_i);
    }

    if (t%100 == 0) {
      double totalM = log_0;
      for(int i=0;i<pM_A.size();i++)
	totalM = logsum(totalM,pM_A[i]);
      totalM = exp(totalM);
      
      double totalG1 = log_0;
      for(int i=0;i<pG1_A.size();i++)
	totalG1 = logsum(totalG1,pG1_A[i]);
      totalG1 = exp(totalG1);
      
      double totalG2 = log_0;
      for(int i=0;i<pG2_A.size();i++)
	totalG2 = logsum(totalG2,pG2_A[i]);
      totalG2 = exp(totalG2);
      
      double totalE = log_0;
      for(int i=0;i<pE_A.size();i++)
	totalE = logsum(totalE,pE_A[i]);
      totalE = exp(totalE);
      
      std::cout<<"t = "<<t-1<<
	"  totalM = "<<totalM<<
	"  totalG1 = "<<totalG1<<
	"  totalG2 = "<<totalG2<<
	"  totalE = "<<totalE<<
	"  total = "<<totalM+totalG1+totalG2+totalE<<
	std::endl;
    }

    pM_A  = pM_B;
    pG1_A = pG1_B;
    pG2_A = pG2_B;
    pE_A  = pE_B;
  }

  p_length = pE_A;
  for(int i=0;i<p_length.size();i++)
    std::cout<<p_length[i];
  std::cout<<std::endl;

}

IndelModel::IndelModel(double LO,double LE) {
  lambda_O = LO;
  lambda_E = LE;

  delta   = exp(lambda_O);
  epsilon = exp(lambda_E);

  tau     = 1.0e-3;

  pM_M   = log(1.0-2.0*delta);
  pM_G1  = log(delta);
  pM_G2  = log(delta);
  pM_E   = log_0;

  pG1_M  = log(1.0 - epsilon) + log(1.0 - delta);
  pG1_G1 = log(epsilon);
  pG1_G2 = log(1.0 - epsilon) + log(delta);
  pG1_E  = log_0;

  pG2_M  = log(1.0 - epsilon);
  pG2_G1 = log_0;
  pG2_G2 = log(epsilon);
  pG2_E  = log_0;

  /* Modify for exponential gap length */
 
  pM_M   += log(1.0 - tau);
  pM_G1  += log(1.0 - tau);
  pM_G2  += log(1.0 - tau);
  pM_E    = log(tau);

  pG1_M  += log(1.0 - tau);
  pG1_G1 += log(1.0 - tau);
  pG1_G2 += log(1.0 - tau);
  pG1_E   = log(tau);

  pG2_M  += log(1.0 - tau);
  pG2_G1 += log(1.0 - tau);
  pG2_G2 += log(1.0 - tau);
  pG2_E   = log(tau);

  pi_M  = 0;
  pi_G1 = log_0;
  pi_G2 = log_0;
  pi_E  = log_0;

  construct_lengthp(10000);
}

void Parameters::setlength(int b,double l) {
  assert(l >= 0);
  assert(b >= 0 and b < T.branches());
  T.branch(b).length = l;
  substitution_[b] = SModel->transition_p(T.branch(b).length);
}


void Parameters::recalc() {
  substitution_.clear();
  for(int i=0;i<T.branches();i++) 
    substitution_.push_back(SModel->transition_p(T.branch(i).length));
}


Parameters::Parameters(SubstitutionModel& SM,double lambda_E,double lambda_O,const SequenceTree& t)
  :SModel(&SM),IModel(lambda_O,lambda_E),T(t)
{
  branch_mean = 1.0;
  recalc();
}
