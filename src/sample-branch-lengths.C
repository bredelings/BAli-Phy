#include "rng.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"
#include "5way.H"
#include "substitution-cache.H"
#include "substitution-index.H"
#include "substitution.H"
#include "likelihood.H"

using MCMC::MoveStats;

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

double branch_twiddle(double& T,double sigma) {
  T += gaussian(0,sigma);
  return 1;
}

double branch_twiddle_positive(double& T,double sigma) {
  double ratio = branch_twiddle(T,sigma);
  T = std::abs(T);
  return ratio;
}

double log_branch_twiddle(double& T, double sigma) {
  double ratio = exp( gaussian(0,sigma) );
  T *= ratio;
  return ratio;
}

MCMC::Result change_branch_length_(const alignment& A, Parameters& P,int b,
				       double sigma,double (*twiddle)(double&,double)) 
{
  MCMC::Result result(2);
  
  //------------ Propose new length -------------//
  const double length = P.T.branch(b).length();
  double newlength = length;

  double ratio = twiddle(newlength,sigma);
  
  //---------- Construct proposed Tree ----------//
  select_root(P.T, b, P.LC);

  Parameters P2 = P;
  P2.setlength(b,newlength);

  //--------- Do the M-H step if OK--------------//
  if (do_MH_move(A,P,P2,ratio)) {
    result.totals[0] = 1;
    result.totals[1] = std::abs(length - newlength);
  }

  return result;
}

void change_branch_length_flat(const alignment& A, Parameters& P,MoveStats& Stats,int b,double sigma)
{
  MCMC::Result result = change_branch_length_(A, P, b, sigma*P.branch_mean, branch_twiddle_positive);

  Stats.inc("branch-length",result);
}

void change_branch_length_log_scale(const alignment& A, Parameters& P,MoveStats& Stats,int b,double sigma)
{
  MCMC::Result result = change_branch_length_(A, P, b, sigma, log_branch_twiddle);

  Stats.inc("branch-length (log)",result);
}

void change_branch_length(const alignment& A, Parameters& P,MoveStats& Stats,int b) {

  double r = loadvalue(P.keys,"log_branch_fraction",0.5);

  if (myrandomf() < r) {
    double sigma = loadvalue(P.keys,"log_branch_sigma",1.0);
    change_branch_length_log_scale(A, P, Stats, b, sigma);
  }
  else {
    double sigma = loadvalue(P.keys,"branch_sigma",1.0);
    change_branch_length_flat(A, P, Stats, b, sigma);
  }
}

void change_branch_length_multi(const alignment& A, Parameters& P,MoveStats& Stats,int b) {
  const int n=3;

  for(int i=1;i<n;i++)
    change_branch_length(A,P,Stats,b);
}

void change_branch_length_and_T(alignment& A, Parameters& P,MoveStats& Stats,int b) 
{
  MCMC::Result result(5,0);

  result.counts[0] = 1;

  //------------- Propose new length --------------//
  const double length = P.T.branch(b).length();
  double newlength = length;
  double ratio = branch_twiddle(newlength,P.branch_mean*0.6);

  //----- positive  =>  propose length change -----//
  if (newlength >= 0) 
  {
    result.counts[1] = 1;
    result.counts[3] = 1;

    //---------- Construct proposed Tree ----------//
    select_root(P.T, b, P.LC);

    Parameters P2 = P;
    P2.setlength(b,newlength);

    //--------- Do the M-H step if OK--------------//
    if (do_MH_move(A,P,P2,ratio)) {
      result.totals[0] = 1;
      result.totals[1] = 1;
      result.totals[3] = std::abs(newlength - length);
    }
  }

  //----- negative  => propose topology ---------//
  else 
  {
    result.counts[2] = 1;
    result.counts[4] = 1;

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
    
    vector<efloat_t> rho(2,1);
    rho[1] = ratio;

    //------ Sample the Different Topologies ------//
    int C = two_way_topology_sample(a,p,rho,b);

    if (C != -1) {
      A = a[C];
      P = p[C];
    }

    if (C > 0) {
      result.totals[0] = 1;
      result.totals[2] = 1;
      result.totals[4] = std::abs(length - newlength);
    }
  }

  Stats.inc("change_branch_length_and_T",result);
}

