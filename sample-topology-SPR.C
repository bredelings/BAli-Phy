#include <cmath>
#include <valarray>
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "choose.H"
#include "dpmatrix.H"
#include "likelihood.H"

#include "3way.H"
#include "alignment-sums.H"

void check_match_P(const alignment& A,const Parameters& P, double OS, double OP, const vector<int>& path, const HMM& Matrices) {

  /*------------------- Check offsets from path_Q -> P -----------------*/
  vector<int> path_g = Matrices.generalize(path);

  double qs = Matrices.path_Q_subst(path_g) + OS;
  double ls = P.likelihood(A,P);

  double qp = Matrices.path_Q_path(path_g) + Matrices.generalize_P(path) + OP;
  double lp = prior_HMM(A,P);

  double qt = qs + qp + prior(P);
  double lt = P.probability(A,P);

  std::cerr<<"qs = "<<qs<<"    ls = "<<ls<<endl;
  std::cerr<<"qp = "<<qp<<"    lp = "<<lp<<endl;
  std::cerr<<"qt = "<<qt<<"    lt = "<<lt<<endl;

  if ( (std::abs(qs - ls) > 1.0e-9) or (std::abs(qp - lp) > 1.0e-9) or (std::abs(qt - lt) > 1.0e-9)) {
    std::cerr<<A<<endl;
    std::abort();
  }

}

vector<double> sample_P(const alignment& A,const Parameters& P,
			double OS, double OP, double P_choice,
			const vector<int>& path, const HMM& Matrices) 
{
  vector<double> PR(3);

  vector<int> path_g = Matrices.generalize(path);

  // Probability
  PR[0] = P.probability(A,P);

  // Probability of sampling 
  PR[1] = P_choice + Matrices.path_P(path_g) + Matrices.generalize_P(path);

  std::cerr<<"PrS = "<<P_choice<<" + "<<Matrices.path_P(path_g)<<" + "<<Matrices.generalize_P(path)<<endl;

  PR[2] = Matrices.path_Q(path_g) + Matrices.generalize_P(path)+ prior(P) + OS + OP;

  return PR;
}


///Sample between 2 topologies, ignoring gap priors on each case
bool topology_sample_SPR_fgaps(alignment& A,Parameters& P1,const Parameters& P2,int n1,int n2) {

  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  //----------- Generate the Different Matrices ---------//

  vector<alignment> a(3,A);

  vector<Parameters> p(3,P1);
  p[2] = P2;

  vector< vector<int> > nodes(3);

  nodes[0] = A3::get_nodes_random(P1.T,n1);
  nodes[1] = nodes[0];
  nodes[2] = A3::get_nodes_random(P2.T,n1);

  vector<DParrayConstrained> Matrices;
  Matrices.push_back( sample_node_base(a[1],p[1],nodes[1]) );
  Matrices.push_back(Matrices[0]);
  Matrices.push_back( sample_node_base(a[2],p[2],nodes[2]) );

  //-------- Calculate corrections to path probabilities ---------//

  vector<double> OP(3);
  OP[0] = other_prior(a[0],p[0],nodes[0][0]);
  OP[1] = OP[0];
  OP[2] = other_prior(a[2],p[2],nodes[2][0]);
  
  vector<double> OS(3);
  OS[0] = p[1].likelihood(a[1],p[1]);
  OS[1] = OS[0];
  OS[2] = p[2].likelihood(a[2],p[2]);

  //---------- Choose between the topologies (P1,P2,P3) -----------//
  
  vector<double> Pr(2);
  
  Pr[0] = Matrices[1].Pr_sum_all_paths() + OS[1] + OP[1] + prior(p[1]);
  Pr[1] = Matrices[2].Pr_sum_all_paths() + OS[2] + OP[2] + prior(p[2]);
  
  int C = 1 + choose(Pr);
  
#ifndef NDEBUG_DP

  vector< vector<int> > paths;

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<3;i++) {
    vector<int> path   = get_path_3way(A3::project(a[i],nodes[i]),0,1,2,3);
    paths.push_back( path ); 

    int length = a[i].seqlength(nodes[i][0]);

    double OP_i = OP[i] - 2.0*p[i].IModel().lengthp( length );

    check_match_P(a[i], p[i], OS[i], OP_i, path, Matrices[i]);
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<double> > PR(3);

  for(int i=0;i<3;i++) {
    double P_choice = 0;
    if (i==0)
      P_choice = choose_P(0,Pr);
    else
      P_choice = choose_P(i-1,Pr);

    PR[i] = sample_P(a[i], p[i], OS[i], OP[i] , P_choice, paths[i], Matrices[i]);
    PR[i][0] += 2.0*p[i].IModel().lengthp( a[i].seqlength(nodes[i][0]) );
  }


  std::cerr<<"choice = "<<C<<endl;
  std::cerr<<" Pr1  = "<<PR[0][0]<<"    Pr2  = "<<PR[C][0]<<"    Pr2  - Pr1  = "<<PR[C][0] - PR[0][0]<<endl;
  std::cerr<<" PrQ1 = "<<PR[0][2]<<"    PrQ2 = "<<PR[C][2]<<"    PrQ2 - PrQ1 = "<<PR[C][2] - PR[0][2]<<endl;
  std::cerr<<" PrS1 = "<<PR[0][1]<<"    PrS2 = "<<PR[C][1]<<"    PrS2 - PrS1 = "<<PR[C][1] - PR[0][1]<<endl;

  double diff = (PR[C][1] - PR[0][1]) - (PR[C][0] - PR[0][0]);
  std::cerr<<"diff = "<<diff<<endl;
  if (std::abs(diff) > 1.0e-9) {
    std::cerr<<a[0]<<endl;
    std::cerr<<a[C]<<endl;

    std::cerr<<A3::project(a[0],nodes[0])<<endl;
    std::cerr<<A3::project(a[C],nodes[C])<<endl;

    std::abort();
  }
#endif

  /*---------------- Adjust for length of node0 (nodes[0]) changing --------------------*/

  bool success = false;
  if (myrandomf() < exp( A3::log_acceptance_ratio(a[0],p[0],nodes[0],a[C],p[C],nodes[C])))  {
    success = (C == 2);
    A = a[C];
    P1 = p[C];
  }

  return success;
}




///Sample between 2 topologies, ignoring gap priors on each case
bool sample_SPR_and_A(alignment& A,Parameters& P1,const Parameters& P2,int n1,int n2) {

  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  //----------- Generate the Different Matrices ---------//

  vector<alignment> a(3,A);

  vector<Parameters> p(3,P1);
  p[2] = P2;

  vector< vector<int> > nodes(3);

  nodes[0] = A3::get_nodes_branch_random(P1.T,n1,n2);
  nodes[1] = nodes[0];
  nodes[2] = A3::get_nodes_branch_random(P2.T,n1,n2);

  vector<DPmatrixConstrained> Matrices;
  Matrices.push_back( tri_sample_alignment_base(a[1],p[1],nodes[1]) );
  Matrices.push_back(Matrices[0]);
  Matrices.push_back( tri_sample_alignment_base(a[2],p[2],nodes[2]) );

  //-------- Calculate corrections to path probabilities ---------//

  vector<double> OP(3);
  OP[0] = other_prior(a[0],p[0],nodes[0][0]);
  OP[1] = OP[0];
  OP[2] = other_prior(a[2],p[2],nodes[2][0]);
  
  vector<double> OS(3);
  OS[0] = other_subst(a[1],p[1],nodes[1]);  
  OS[1] = OS[0];
  OS[2] = other_subst(a[2],p[2],nodes[2]);

  //---------- Choose between the topologies (P1,P2,P3) -----------//
  
  vector<double> Pr(2);
  
  Pr[0] = Matrices[1].Pr_sum_all_paths() + OS[1] + OP[1] + prior(p[1]);
  Pr[1] = Matrices[2].Pr_sum_all_paths() + OS[2] + OP[2] + prior(p[2]);
  
  int C = 1 + choose(Pr);
  
#ifndef NDEBUG_DP

  vector< vector<int> > paths;

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<3;i++) {
    vector<int> path   = get_path_3way(A3::project(a[i],nodes[i]),0,1,2,3);
    paths.push_back( path ); 

    int length = a[i].seqlength(nodes[i][0]);
    double OP_i = OP[i] - 2.0*p[i].IModel().lengthp( length );

    check_match_P(a[i], p[i], OS[i], OP_i, path, Matrices[i]);
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<double> > PR(3);

  for(int i=0;i<3;i++) {
    double P_choice = 0;
    if (i==0)
      P_choice = choose_P(0,Pr);
    else
      P_choice = choose_P(i-1,Pr);

    PR[i] = sample_P(a[i], p[i], OS[i], OP[i] , P_choice, paths[i], Matrices[i]);
    PR[i][0] += 2.0*p[i].IModel().lengthp( a[i].seqlength(nodes[i][0]) );
  }


  std::cerr<<"choice = "<<C<<endl;
  std::cerr<<" Pr1  = "<<PR[0][0]<<"    Pr2  = "<<PR[C][0]<<"    Pr2  - Pr1  = "<<PR[C][0] - PR[0][0]<<endl;
  std::cerr<<" PrQ1 = "<<PR[0][2]<<"    PrQ2 = "<<PR[C][2]<<"    PrQ2 - PrQ1 = "<<PR[C][2] - PR[0][2]<<endl;
  std::cerr<<" PrS1 = "<<PR[0][1]<<"    PrS2 = "<<PR[C][1]<<"    PrS2 - PrS1 = "<<PR[C][1] - PR[0][1]<<endl;

  double diff = (PR[C][1] - PR[0][1]) - (PR[C][0] - PR[0][0]);
  std::cerr<<"diff = "<<diff<<endl;
  if (std::abs(diff) > 1.0e-9) {
    std::cerr<<a[0]<<endl;
    std::cerr<<a[C]<<endl;

    std::cerr<<A3::project(a[0],nodes[0])<<endl;
    std::cerr<<A3::project(a[C],nodes[C])<<endl;

    std::abort();
  }
#endif

  /*---------------- Adjust for length of node0 (nodes[0]) changing --------------------*/

  bool success = false;
  if (myrandomf() < exp( A3::log_acceptance_ratio(a[0],p[0],nodes[0],a[C],p[C],nodes[C])))  {
    success = (C == 2);
    A = a[C];
    P1 = p[C];
  }

  return success;
}




///Sample between 2 topologies, ignoring gap priors on each case
bool topology_sample_SPR_sgaps(alignment& A,Parameters& P1,const Parameters& P2) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);

  /*********** Choose A Topology ************/
  int choice = choose2(Pr1,Pr2);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  return success;
}

bool topology_sample_SPR(alignment& A,Parameters& P1,const Parameters& P2,int n1, int n2) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);

  if (P1.IModel().full_tree)
    return topology_sample_SPR_fgaps(A,P1,P2,n1,n2);
  else
    return topology_sample_SPR_sgaps(A,P1,P2);
}

SequenceTree do_SPR(const SequenceTree& T1, int n1, int n2, int b1) {
  //----- Select the branch to move to ------//
  valarray<bool> subtree_nodes = T1.partition(n1,n2);
  vector<int> branches;
  vector<double> lengths;

  for(int i=0;i<T1.branches();i++) {
    if (not subtree_nodes[T1.branch(i).parent()] or 
	not subtree_nodes[T1.branch(i).child()]) {
      branches.push_back(i);
      lengths.push_back(T1.branch(i).length());
    }
  }

  int b2 = branches[ choose_nonlog(lengths) ];

  SequenceTree T2 = T1;

  //------ Generate the new topology ------//
  if (T2.branch(b2).parent() == n1 or T2.branch(b2).child() == n1)
    ;
  else
    T2.SPR(n1,n2,b2);

  //------ Find the two new branches ------//
  vector<int> connected;
  for(int i=0;i<T2.branches();i++) {
    if (i == b1) 
      continue;

    if (T2.branch(i).parent() == n1 or T2.branch(i).child() == n1)
      connected.push_back(i);
  }
  assert(connected.size() == 2);
  

  //------- Place the split randomly -------//
  double total = T2.branch(connected[0]).length() + T2.branch(connected[1]).length();
  T2.branch(connected[0]).length() = myrandomf() * total;
  T2.branch(connected[1]).length() = total - T2.branch(connected[0]).length();

  return T2;
}


MCMC::result_t sample_SPR(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P1;

  SequenceTree& T1 = P1.T;
  SequenceTree& T2 = P2.T;

  //----- Get nodes for directed branch ------//
  int n1 = T1.branch(b).parent();
  int n2 = T1.branch(b).child();
  if (myrandomf()< 0.5)
    std::swap(n1,n2);
  if (T1[n1].leaf())
    std::swap(n1,n2);

  //----- Generate the Different Topologies ----//

  //  std::cerr<<"before = "<<T1<<endl;

  T2 = do_SPR(T1,n1,n2,b);
  P2.recalc();
  
  //  std::cerr<<"after = "<<T2<<endl;

  bool success = topology_sample_SPR(A,P1,P2,n1,n2);

  if (success)
    result[1] = 1;

  return result;
}

MCMC::result_t sample_SPR_and_A(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  if (not P1.IModel().full_tree)
    throw myexception()<<"Can't use 'sample_SPR_and_A' with star gaps yet";

  Parameters P2 = P1;

  SequenceTree& T1 = P1.T;
  SequenceTree& T2 = P2.T;

  //----- Get nodes for directed branch ------//
  int n1 = T1.branch(b).parent();
  int n2 = T1.branch(b).child();
  if (myrandomf()< 0.5)
    std::swap(n1,n2);
  if (T1[n1].leaf())
    std::swap(n1,n2);

  //----- Generate the Different Topologies ----//
  std::cerr<<"before = "<<T1<<endl;

  T2 = do_SPR(T1,n1,n2,b);
  P2.recalc();
  
  std::cerr<<"after = "<<T2<<endl;

  bool success = sample_SPR_and_A(A,P1,P2,n1,n2);

  if (success)
    result[1] = 1;

  return result;
}
