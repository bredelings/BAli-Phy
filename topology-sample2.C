#include <valarray>
#include <iostream>
#include <cmath>
#include <cassert>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"
#include "util.H"
#include "rng.H"
#include "5way.H"
#include "dpmatrix.H"
#include "alignment-sums.H"


#include "3way.H"
#include "substitution.H"
#include "sample.H"


// for prior_HMM_nogiven
#include "likelihood.H"

// We are sampling from a 5-way alignment (along 5 branches)

// Its a 4-way dynamic programming, though - so the only thing
// that matters is the order of the 4D path. (I think...)

// We want to scramble the sorting method for the branches
// Perhaps that should be the NEXT step?  We can scramble the
// node names, though - we use those to know which leaf node
// is connected to which internal node.

// Branches are labelled 0-3, as are the leaves.  Internal nodes
// are 4,5; internal branch is 5.

using std::abs;
using std::valarray;

using namespace A5;

bool two_way_topology_sample_fgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  alignment old = A;

  alignment A1 = old;
  alignment A2 = old;

  /*---------------- Setup node names ------------------*/
  assert(b >= P1.T.leafbranches());
  const vector<int> nodes1 = A5::get_nodes_random(P1.T,b);
  const vector<int> nodes2 = A5::get_nodes_random(P2.T,b);

  // sample the path from each matrix, remember the gap probability of that topology
  DParrayConstrained Matrices1 = sample_two_nodes_base(A1,P1,nodes1);
  DParrayConstrained Matrices2 = sample_two_nodes_base(A2,P2,nodes2);

  // Choose from one of the (Ai,Pi) pairs
  double PS1 = P1.likelihood(A1,P1);
  double PA1 = Matrices1.Pr_sum_all_paths();
  double PP1 = prior(P1);

  double PS2 = P1.likelihood(A2,P2);
  double PA2 = Matrices2.Pr_sum_all_paths();
  double PP2 = prior(P2);

  vector<double> P(2);
  P[0] = PS1 + PA1 + PP1;
  P[1] = PS2 + PA2 + PP2;

  int choice = choose(P);

  // Get some pointers to the chosen one.
  DParrayConstrained const* CM = &Matrices1;
  alignment const* CA = &A1;
  Parameters const* CP = &P1;
  const vector<int>* Cnodes = &nodes1;
  if (choice == 1){
    CM = &Matrices2;
    CA = &A2;
    CP = &P2;
    Cnodes = &nodes2;
  }

#ifndef NDEBUG_DP
  /*------- Get the Probabilities of the new and old states --------*/
  const vector<int>& nodes_old    = nodes1;
  const vector<int> nodes_new     = *Cnodes;

  int l1_old = old.seqlength(nodes_old[4]);
  int l1_new = CA->seqlength(nodes_old[4]);

  int l2_old = old.seqlength(nodes_old[5]);
  int l2_new = CA->seqlength(nodes_old[5]);

  double Pr1 = probability3(old,P1) + 2.0*(P1.IModel().lengthp(l1_old) + P1.IModel().lengthp(l2_old));
  double Pr2 = probability3(*CA,*CP) + 2.0*(P1.IModel().lengthp(l1_new) + P1.IModel().lengthp(l2_new));;

  vector<int> states_list = construct_states();


  /*--------------- Get the new and old paths ---------------*/
  assert(b >= P1.T.leafbranches());
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_old = get_path(project(old,nodes1),newnodes,states_list);
  //  vector<int> path_old = A5::get_path(old,nodes_old,states_list);
  vector<int> path_g_old = Matrices1.generalize(path_old);

  vector<int> path_new = get_path(project(*CA,*Cnodes),newnodes,states_list);
  //  vector<int> path_new = A5::get_path(*CA,nodes_new,states_list);
  vector<int> path_g_new = CM->generalize(path_new);

  /*--------------- Compute the sampling probabilities ---------------*/
  double SP1 = choose_P(0,P) + Matrices1.path_P(path_g_old) + Matrices1.generalize_P(path_old);
  double SP2 = choose_P(choice,P) + CM->path_P(path_g_new) + CM->generalize_P(path_new);

  double diff = (Pr2 - Pr1) - (SP2 - SP1);
  std::cerr<<"diff = "<<diff<<std::endl;
  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,nodes_old)<<endl;
    std::cerr<<project(A,nodes_new)<<endl;

    std::abort();
  }
#endif

  // Go ahead with the choice
  if (choice != 0) {
    A  = *CA;
    P1 = *CP;
  }

  return (choice != 0);
}

// Do we need the different sample_two_nodes_base routines to use the same
// branch ordering?  We are just 
//  o considering a set of paths (w/ associated topologies)
//  o dividing them up unto groups
//  o choosing between the groups
//  o choosing between the paths in each group
// However, we need the routine to be gibbs.  That is, we need to have each
// input sequence propose the others 

// We could consider a set of moves, each of which has a specified branch ordering
// or something.  Then we would need to have each propose the others condition
// on the branch ordering for the 4 leaf branches.

// We could probably have a set ordering for each topology, as long as each
// proposed the other.... (this would be nice!)


//NOTE: in order to compare the trees which we are summing over different branches,
//      we would have to somehow incorporate the differences in the rest of the likelihood.
//      But in the current code we are ignoring that.
//         - FIXME: calculate the full likelihood, subtract the current alignment, and the add
//           in the sum over all alignments.  (alignment = alignments in the HMM here... )


/// This has to be Gibbs, and use the same substitution::Model in each case...
bool three_way_topology_sample_fgaps(alignment& A,Parameters& P1, const Parameters& P2, 
		      const Parameters& P3,int b) {
  alignment old = A;

  alignment A1 = old;
  alignment A2 = old;
  alignment A3 = old;

  /*---------------- Setup node names ------------------*/
  assert(b >= P1.T.leafbranches());
  const vector<int> nodes1 = A5::get_nodes_random(P1.T,b);
  const vector<int> nodes2 = A5::get_nodes_random(P2.T,b);
  const vector<int> nodes3 = A5::get_nodes_random(P3.T,b);

  // sample the path from each matrix, remember the gap probability of that topology
  DParrayConstrained Matrices1 = sample_two_nodes_base(A1,P1,nodes1);
  DParrayConstrained Matrices2 = sample_two_nodes_base(A2,P2,nodes2);
  DParrayConstrained Matrices3 = sample_two_nodes_base(A3,P3,nodes3);

  // Choose from one of the (Ai,Pi) pairs
  double PS1 = P1.likelihood(A1,P1);
  double PA1 = Matrices1.Pr_sum_all_paths();
  double PP1 = prior(P1);

  double PS2 = P1.likelihood(A2,P2);
  double PA2 = Matrices2.Pr_sum_all_paths();
  double PP2 = prior(P2);

  double PS3 = P1.likelihood(A3,P3);
  double PA3 = Matrices3.Pr_sum_all_paths();
  double PP3 = prior(P3);

  vector<double> P(3);
  P[0] = PS1 + PA1 + PP1;
  P[1] = PS2 + PA2 + PP2;
  P[2] = PS3 + PA3 + PP3;

  int choice = choose(P);

  // Get some pointers to the chosen one.
  DParrayConstrained const* CM = &Matrices1;
  alignment const* CA = &A1;
  Parameters const* CP = &P1;
  const vector<int>* Cnodes = &nodes1;
  if (choice == 1){
    CM = &Matrices2;
    CA = &A2;
    CP = &P2;
    Cnodes = &nodes2;
  }
  else if (choice == 2){
    CM = &Matrices3;
    CA = &A3;
    CP = &P3;
    Cnodes = &nodes3;
  }

#ifndef NDEBUG_DP
  /*------- Get the Probabilities of the new and old states --------*/
  const vector<int>& nodes_old    = nodes1;
  const vector<int> nodes_new     = *Cnodes;

  int l1_old = old.seqlength(nodes_old[4]);
  int l1_new = CA->seqlength(nodes_old[4]);

  int l2_old = old.seqlength(nodes_old[5]);
  int l2_new = CA->seqlength(nodes_old[5]);

  double Pr1 = probability3(old,P1) + 2.0*(P1.IModel().lengthp(l1_old) + P1.IModel().lengthp(l2_old));
  double Pr2 = probability3(*CA,*CP) + 2.0*(P1.IModel().lengthp(l1_new) + P1.IModel().lengthp(l2_new));;

  vector<int> states_list = construct_states();


  /*--------------- Get the new and old paths ---------------*/
  assert(b >= P1.T.leafbranches());
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_old = get_path(project(old,nodes1),newnodes,states_list);
  //  vector<int> path_old = A5::get_path(old,nodes_old,states_list);
  vector<int> path_g_old = Matrices1.generalize(path_old);

  vector<int> path_new = get_path(project(*CA,*Cnodes),newnodes,states_list);
  //  vector<int> path_new = A5::get_path(*CA,nodes_new,states_list);
  vector<int> path_g_new = CM->generalize(path_new);

  /*--------------- Compute the sampling probabilities ---------------*/
  double SP1 = choose_P(0,P) + Matrices1.path_P(path_g_old) + Matrices1.generalize_P(path_old);
  double SP2 = choose_P(choice,P) + CM->path_P(path_g_new) + CM->generalize_P(path_new);

  double diff = (Pr2 - Pr1) - (SP2 - SP1);
  std::cerr<<"diff = "<<diff<<std::endl;
  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,nodes_old)<<endl;
    std::cerr<<project(A,nodes_new)<<endl;

    std::abort();
  }
#endif

  // Go ahead with the choice
  if (choice != 0) {
    A  = *CA;
    P1 = *CP;
  }

  return (choice != 0);
}

///Sample between 3 topologies, ignoring gap priors on each case
bool three_way_topology_sample_sgaps(alignment& A,Parameters& P1,const Parameters& P2, 
			       const Parameters& P3,int b) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);
  double Pr3 = P1.probability(A,P3);

  /*********** Choose A Topology ************/
  int choice = choose(Pr1,Pr2,Pr3);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  else if (choice == 2) {
    P1 = P3;
    success = true;
  }
  return true;
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample_sgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);

  /*********** Choose A Topology ************/
  int choice = choose(Pr1,Pr2);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  return true;
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);

  if (P1.IModel().full_tree)
    return two_way_topology_sample_fgaps(A,P1,P2,b);
  else
    return two_way_topology_sample_sgaps(A,P1,P2,b);
}

MCMC::result_t two_way_topology_sample(alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int>nodes = A5::get_nodes_random(P.T,b);

  Parameters P2 = P;
  P2.T.exchange(nodes[1],nodes[2]);
  
  bool success = two_way_topology_sample(A,P,P2,b);
  if (success)
    result[1] = 1;

  return result;
}

MCMC::result_t two_way_topology_sample2(alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;
  
  const tree& T = P.T;

  vector<int> nodes = A5::get_nodes_random(P.T,b);
  
  //  int b0 = T.find_branch(nodes[0],nodes[4]);
  int b1 = T.find_branch(nodes[1],nodes[4]);
  int b2 = T.find_branch(nodes[2],nodes[5]);
  //  int b3 = T.find_branch(nodes[3],nodes[5]);
  int b4 = T.find_branch(nodes[4],nodes[5]);


  Parameters P2 = P;

  P2.T.exchange(nodes[1],nodes[2]);
  // b0 unchanged
  P2.T.branch(b1).length() = T.branch(b1).length() + T.branch(b4).length();
  P2.T.branch(b2).length() = myrandomf() * T.branch(b2).length();
  // b3 unchanged
  P2.T.branch(b4).length() = T.branch(b2).length() - P2.T.branch(b2).length();
  bool success = two_way_topology_sample(A,P,P2,b);
  if (success)
    result[1] = 1;

  return result;
}


bool three_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,const Parameters& P3,int b) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);
  assert(P2.IModel().full_tree == P3.IModel().full_tree);

  if (P1.IModel().full_tree)
    return three_way_topology_sample_fgaps(A,P1,P2,P3,b);
  else
    return three_way_topology_sample_sgaps(A,P1,P2,P3,b);
}


//FIXME - go through code and create more exceptions, from asserts... 
MCMC::result_t three_way_topology_sample(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int> nodes = A5::get_nodes(P1.T,b);

  /****** Generate the Different Topologies *******/
  Parameters P2 = P1;
  Parameters P3 = P1;

  SequenceTree& T2 = P2.T;
  SequenceTree& T3 = P3.T;

  T2.exchange(nodes[1],nodes[2]);
  T3.exchange(nodes[1],nodes[3]);
  
  bool success = three_way_topology_sample(A,P1,P2,P3,b);



  if (success)
    result[1] = 1;

  return result;
}

alignment swap(const alignment& old,int n1,int n2) {
  alignment A = old;
  for(int column=0;column<A.length();column++)
    std::swap(A(column,n1),A(column,n2));

  return A;
}



double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes) {
  double p = 0.0;

  for(int column=0;column < A.length();column++) {
    bool present = false;
    for(int i=0;i<nodes.size();i++) {
      if (not A.gap(column,nodes[i]))
	present = true;
    }
    if (present) continue;

    p += substitution::Pr(A, P.T, P.SModel(), P, column);
  }

  return p;
}

double other_prior(const alignment& A, const Parameters& P, int n0) {
  const tree& T = P.T;

  double p = 0.0;

  // Add in the branch alignments
  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child = T.branch(b).child();
    if (n0 == parent or n0 == child)
      continue;
    p += prior_branch(A,P.IModel(),parent,child);
  }


  for(int n=0;n<T.num_nodes()-1;n++) {
    if (T[n].leaf())
      continue;

    if (n == n0)
      continue;

    p -= 2.0*P.IModel().lengthp(A.seqlength(n));
  }

  return p;
}



MCMC::result_t three_way_topology_and_alignment_sample(alignment& A,Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  alignment oldA = A;
  Parameters oldP = P;

  vector<int> nodes = A5::get_nodes_random(P.T,b);

  /*--------- Generate the Different Topologies -------*/

  // do topology2
  vector<int> nodes1(4);
  nodes1[0] = nodes[4];
  nodes1[1] = nodes[0];
  nodes1[2] = nodes[1];
  nodes1[3] = nodes[5];
  if (myrandomf()< 0.5)
    std::swap(nodes1[2],nodes1[3]);
  alignment a1 = A;
  Parameters P1 = P;
  DPmatrixHMM Matrices1 = tri_sample_alignment_base(a1,P1,nodes1);


  // do topology2
  vector<int> nodes2(4);
  nodes2[0] = nodes[4];
  nodes2[1] = nodes[0];
  nodes2[2] = nodes[2];
  nodes2[3] = nodes[5];
  if (myrandomf()< 0.5)
    std::swap(nodes2[2],nodes2[3]);
  alignment a2 = A;
  Parameters P2 = P1;
  P2.T.exchange(nodes[1],nodes[2]);
  DPmatrixHMM Matrices2 = tri_sample_alignment_base(a2,P2,nodes2);


  // do topology3
  vector<int> nodes3(4);
  nodes3[0] = nodes[4];
  nodes3[1] = nodes[0];
  nodes3[2] = nodes[3];
  nodes3[3] = nodes[5];
  if (myrandomf()< 0.5)
    std::swap(nodes3[2],nodes3[3]);
  alignment a3 = A;
  Parameters P3 = P1;
  P3.T.exchange(nodes[1],nodes[3]);
  DPmatrixHMM Matrices3 = tri_sample_alignment_base(a3,P3,nodes3);


  /*--------------- Calculate corrections to path probabilities --------------*/

  double OS1 = other_subst(a1,P1,nodes1); 
  double OP1 = other_prior(a1,P1,nodes1[0]);

  double OS2 = other_subst(a2,P2,nodes2); 
  double OP2 = other_prior(a2,P2,nodes2[0]);

  double OS3 = other_subst(a3,P3,nodes3); 
  double OP3 = other_prior(a3,P3,nodes3[0]);

  /*-------------------- Choose between the topologies (P1,P2,P3) ------------------------*/

  vector<double> Pr(3);

  Pr[0] = Matrices1.Pr_sum_all_paths() + OS1 + OP1 + prior(P1);
  Pr[1] = Matrices2.Pr_sum_all_paths() + OS2 + OP2 + prior(P2);
  Pr[2] = Matrices3.Pr_sum_all_paths() + OS3 + OP3 + prior(P3);

  int choice = choose(Pr);

  DPmatrixHMM* CM = 0;
  double OSnew = OS1;
  double OPnew = OP1;
  vector<int> nodes_new = nodes1;
  if (choice == 0) {
    CM = &Matrices1;
    A = a1;
  }
  else if (choice == 1) {
    CM = &Matrices2;
    A = a2;
    P = P2;
    OSnew = OS2;
    OPnew = OP2;
    nodes_new = nodes2;
  }
  else if (choice == 2) {
    CM = &Matrices3;
    A = a3;
    P = P3;
    OSnew = OS3;
    OPnew = OP3;
    nodes_new = nodes3;
  }


  int length_old = oldA.seqlength(nodes[4]);
  int length_new = A.seqlength(nodes[4]);

#ifndef NDEBUG_DP
  /*------------------- Check offsets from path_Q -> P -----------------*/

  vector<int> path_old = get_path_3way(A3::project(oldA,nodes1[0],nodes1[1],nodes1[2],nodes1[3]),0,1,2,3);
  vector<int> path_old_g = Matrices1.generalize(path_old);

  double qs1 = Matrices1.path_Q_subst(path_old) + OS1;
  double ls1 = oldP.likelihood(oldA,oldP);

  double qp1 = Matrices1.path_Q_path(path_old_g) + Matrices1.generalize_P(path_old) + OP1;
  double lp1 = prior_HMM(oldA,oldP) + 2.0*oldP.IModel().lengthp(length_old);

  double qt1 = qs1 + qp1 + prior(oldP);
  double lt1 = oldP.probability(oldA,oldP) + 2.0*oldP.IModel().lengthp(length_old);

  std::cerr<<"qs1 = "<<qs1<<"    ls1 = "<<ls1<<endl;
  std::cerr<<"qp1 = "<<qp1<<"    lp1 = "<<lp1<<endl;
  std::cerr<<"qt1 = "<<qt1<<"    lt1 = "<<lt1<<endl;

  if ( (std::abs(qs1 - ls1) > 1.0e-9) or (std::abs(qp1 - lp1) > 1.0e-9) or (std::abs(qt1 - lt1) > 1.0e-9)) {
    std::cerr<<A3::project(oldA,nodes[0],nodes[1],nodes[2],nodes[3]);
    std::abort();
  }

  /*---------------- Check new topology versus old topology --------------------*/

  vector<int> path_new = get_path_3way(A3::project(A,nodes_new[0],nodes_new[1],nodes_new[2],nodes_new[3]),0,1,2,3);
  vector<int> path_new_g = CM->generalize(path_new);

  double qs2 = CM->path_Q_subst(path_new) + OSnew;
  double ls2 = P.likelihood(A,P);

  double qp2 = CM->path_Q_path(path_new_g) + CM->generalize_P(path_new) + OPnew;
  double lp2 = prior_HMM(A,P) + 2.0*P.IModel().lengthp(length_new);

  double qt2 = qs2 + qp2 + prior(P);
  double lt2 = P.likelihood(A,P) + prior_HMM(A,P) + prior(P) + 2.0*P.IModel().lengthp(length_new);

  std::cerr<<"qs2 = "<<qs2<<"    ls2 = "<<ls2<<endl;
  std::cerr<<"qp2 = "<<qp2<<"    lp2 = "<<lp2<<endl;
  std::cerr<<"qt2 = "<<qt2<<"    lt2 = "<<lt2<<endl;

  if ( (std::abs(qs2 - ls2) > 1.0e-9) or (std::abs(qp2 - lp2) > 1.0e-9) or (std::abs(qt2 - lt2) > 1.0e-9)) {
    std::cerr<<A3::project(A,nodes[0],nodes[1],nodes[2],nodes[3]);
    std::abort();
  }

  

  //-------------- Check relative path probabilities --------------//
  double PrOld = oldP.probability(oldA,oldP) + 2.0*oldP.IModel().lengthp(length_old);
  double PrNew =    P.probability(A,P)       + 2.0*   P.IModel().lengthp(length_new);

  double PrS1 = choose_P(0,Pr)      + Matrices1.path_P(path_old_g)       + Matrices1.generalize_P(path_old);

  std::cerr<<"PrS1 = "<<choose_P(0,Pr)<<" + "<<Matrices1.path_P(path_old_g)<<" + "<< Matrices1.generalize_P(path_old)<<endl;

  double PrS2 = choose_P(choice,Pr) + CM->path_P(path_new_g) + CM->generalize_P(path_new);

  std::cerr<<"PrS2 = "<<choose_P(choice,Pr)<<" + "<<CM->path_P(path_new_g)<<" + "<< CM->generalize_P(path_new)<<endl;

  double PrQ1 = Matrices1.path_Q(path_old_g) + Matrices1.generalize_P(path_old)+ prior(oldP) + OS1 + OP1;

  double PrQ2 = CM->path_Q(path_new_g) + CM->generalize_P(path_new) + prior(P) + OSnew + OPnew;

  std::cerr<<"choice = "<<choice<<endl;
  std::cerr<<" Pr1  = "<<PrOld<<"    Pr2  = "<<PrNew<<"    Pr2  - Pr1  = "<<PrNew - PrOld<<endl;
  std::cerr<<" PrQ1 = "<<PrQ1<<"    PrQ2 = "<<PrQ2<<"    PrQ2 - PrQ1 = "<<PrQ2 - PrQ1<<endl;
  std::cerr<<" PrS1 = "<<PrS1<<"    PrS2 = "<<PrS2<<"    PrS2 - PrS1 = "<<PrS2 - PrS1<<endl;


  std::cerr.flush();
  double diff = (PrS2 - PrS1) - (PrNew - PrOld);
  std::cerr<<"diff = "<<diff<<endl;
  if (abs(diff) > 1.0e-9) {
    std::cerr<<oldA<<endl;
    std::cerr<<A<<endl;

    std::cerr<<A3::project(oldA,nodes1[0],nodes1[1],nodes1[2],nodes1[3])<<endl;
    std::cerr<<A3::project(A,nodes_new[0],nodes_new[1],nodes_new[2],nodes_new[3])<<endl;

    std::cerr.flush();
    std::abort();
  }


#endif

  /*---------------- Adjust for length of node0 (nodes[4]) changing --------------------*/

  double log_ratio = 2.0*(P.IModel().lengthp(length_old) - oldP.IModel().lengthp(length_new));

  bool success = false;
  if (myrandomf() < exp(log_ratio))
    success = (choice != 0);
  else {
    A = oldA;
    P = oldP;
  }

  if (success)
    result[1] = 1;

  return result;
}
