#include "rng.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"
#include "5way.H"
#include "substitution-cache.H"
#include "substitution.H"
#include "likelihood.H"

bool do_MH_move(const alignment& A,Parameters& P,const Parameters& P2) {
  if (P.accept_MH(A,P,A,P2)) {
    P=P2;
    std::cerr<<"accepted\n";
    return true;
  }
  else {
    std::cerr<<"rejected\n";
    return false;
  }
}

double branch_twiddle(double T,double mu,double sigma=0.3) {
  return T + gaussian(0,mu*sigma);
}


// FIXME - must modify do_MH_move to use log-scale branch priors for this to work
double log_branch_twiddle(double T, double sigma=0.3) {
  return T * exp( gaussian(0,sigma) );
}

MCMC::result_t change_branch_length(const alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,6);
  result[0] = 1.0;
  result[2] = 1.0;
  result[4] = 1.0;
  
  /********* Propose increment 'epsilon' ***********/
  const double length = P.T.branch(b).length();

  double newlength = branch_twiddle(length,P.branch_mean);
  newlength = std::abs(newlength);
  
  /******** Calculate propsal ratio ***************/
  
  /********** Do the M-H step if OK**************/
  select_root(P.T, b, P.LC);

  Parameters P2 = P;
  P2.setlength(b,newlength);

  if (do_MH_move(A,P,P2)) {
    std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
    result[1] = 1;
    result[3] = std::abs(length - newlength);
    result[5] = std::abs(log((newlength+0.001)/(length+0.001)));
  }
  else
    std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
  return result;
}

MCMC::result_t change_branch_length_multi(const alignment& A, Parameters& P,int b) {
  const int n=3;
  MCMC::result_t result = change_branch_length(A,P,b);
  for(int i=1;i<n;i++)
    result += change_branch_length(A,P,b);
  return result;
}

MCMC::result_t change_branch_length_and_T(alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,10);

  result[0] = 1.0;
  

  /********* Propose increment 'epsilon' ***********/
  const double length = P.T.branch(b).length();
  double newlength = branch_twiddle(length,P.branch_mean);

  std::cerr<<" old length = "<<P.T.branch(b).length()<<"  new length = "<<newlength<<std::endl;

  select_root(P.T, b, P.LC);

  // If the length is positive, simply propose a length change
  if (newlength >= 0) {
    result[2] = 1.0;
    result[6] = 1.0;

    Parameters P2 = P;
    P2.setlength(b,newlength);

    /********** Do the M-H step if OK**************/

    if (do_MH_move(A,P,P2)) {
      std::cerr<<" branch "<<b<<":  "<<length<<" -> "<<newlength<<endl;
      result[1] = 1;
      result[3] = 1;
      result[7] = std::abs(newlength - length);
    }
    else
      std::cerr<<" branch "<<b<<":  "<<length<<" !-> "<<newlength<<endl;
  }
  // If the length is NOT positive, then propose a T change as well
  else {
    result[4] = 1.0;
    result[8] = 1.0;

    
    /****** Generate the Different Topologies *******/
    Parameters P2 = P;
    
    SequenceTree& T2 = P2.T;
    
    vector<int> nodes = A5::get_nodes_random(P.T,b);
    int b1 = T2.directed_branch(nodes[4],nodes[1]);
    int b2 = T2.directed_branch(nodes[5],nodes[2]);
    T2.exchange_subtrees(b1,b2);
    
    P2.setlength(b,-newlength);
    
    if (two_way_topology_sample(A,P,P2,b)) {
      result[1] = 1;
      result[5] = 1;
      result[9] = std::abs(length - newlength);
    }
  }
  return result;
}

