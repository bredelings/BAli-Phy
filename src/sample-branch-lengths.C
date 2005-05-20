#include "rng.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"
#include "5way.H"
#include "substitution-cache.H"
#include "substitution-index.H"
#include "substitution.H"
#include "likelihood.H"

bool do_MH_move(const alignment& A,Parameters& P,const Parameters& P2,double rho) {
  if (P.accept_MH(A,P,A,P2,rho)) {
    P=P2;
    //    std::cerr<<"accepted\n";
    return true;
  }
  else {
    //    std::cerr<<"rejected\n";
    return false;
  }
}

double branch_twiddle(double& T,double mu,double sigma=0.3) {
  T += gaussian(0,mu*sigma);
  return 1;
}

double branch_twiddle_positive(double& T,double mu,double sigma=0.3) {
  double ratio = branch_twiddle(T,mu,sigma);
  T = std::abs(T);
  return ratio;
}

double log_branch_twiddle(double& T, double sigma=0.3) {
  double ratio = exp( gaussian(0,sigma) );
  T *= ratio;
  return ratio;
}

MCMC::result_t change_branch_length(const alignment& A, Parameters& P,int b) 
{
  MCMC::result_t result(0.0,6);
  result[0] = 1.0;
  result[2] = 1.0;
  result[4] = 1.0;
  
  //------------ Propose new length -------------//
  const double length = P.T.branch(b).length();
  double newlength = length;
  double ratio = branch_twiddle_positive(newlength,P.branch_mean);
  
  //---------- Construct proposed Tree ----------//
  select_root(P.T, b, P.LC);

  Parameters P2 = P;
  P2.setlength(b,newlength);

  //--------- Do the M-H step if OK--------------//
  if (do_MH_move(A,P,P2,ratio)) {
    result[1] = 1;
    result[3] = std::abs(length - newlength);
    result[5] = std::abs(log((newlength+0.001)/(length+0.001)));
  }

  return result;
}

MCMC::result_t change_branch_length_multi(const alignment& A, Parameters& P,int b) {
  const int n=3;
  MCMC::result_t result = change_branch_length(A,P,b);
  for(int i=1;i<n;i++)
    result += change_branch_length(A,P,b);
  return result;
}

MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& P,int b) 
{
  MCMC::result_t result(0.0,10);

  result[0] = 1.0;

  //------------- Propose new length --------------//
  const double length = P.T.branch(b).length();
  double newlength = length;
  double ratio = branch_twiddle(newlength,P.branch_mean);

  //----- positive  =>  propose length change -----//
  if (newlength >= 0) 
  {
    result[2] = 1.0;
    result[6] = 1.0;

    //---------- Construct proposed Tree ----------//
    select_root(P.T, b, P.LC);

    Parameters P2 = P;
    P2.setlength(b,newlength);

    //--------- Do the M-H step if OK--------------//
    if (do_MH_move(A,P,P2,ratio)) {
      result[1] = 1;
      result[3] = 1;
      result[7] = std::abs(newlength - length);
    }
  }

  //----- negative  => propose topology ---------//
  else 
  {
    result[4] = 1.0;
    result[8] = 1.0;

    //----- Generate the Different Topologies ------//
    vector<alignment> a(2,A);
    vector<Parameters> p(2,P);
    
    SequenceTree& T2 = p[1].T;
    
    vector<int> nodes = A5::get_nodes_random(T2,b);
    int b1 = T2.directed_branch(nodes[4],nodes[1]);
    int b2 = T2.directed_branch(nodes[5],nodes[2]);
    T2.exchange_subtrees(b1,b2);

    invalidate_subA_index_branch(a[1],T2,b);

    p[1].setlength(b,-newlength);
    
    //------ Sample the Different Topologies ------//
    int C = two_way_topology_sample(a,p,b);

    if (C == -1)
      return result;

    A = a[C];
    P = p[C];

    if (C != 0) {
      result[1] = 1;
      result[5] = 1;
      result[9] = std::abs(length - newlength);
    }
  }
  return result;
}

