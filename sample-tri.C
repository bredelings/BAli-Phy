#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"
#include "util.H"
#include "rng.H"
#include "3way.H"
#include "alignment-sums.H"

// for peel
#include "substitution.H"

// for prior_HMM_nogiven
#include "likelihood.H"

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::valarray;

using namespace A3;

// FIXME - actually resample the path multiple times - pick one on
// opposite side of the middle 
DPmatrixConstrained tri_sample_alignment_base(alignment& A,const Parameters& P,const vector<int>& nodes) {
  const tree& T = P.T;

  assert(T.connected(nodes[0],nodes[1]));
  assert(T.connected(nodes[0],nodes[2]));
  assert(T.connected(nodes[0],nodes[3]));

  const vector<double>& pi = P.IModel().pi;
  const valarray<double>& frequency = P.SModel().BaseModel().frequencies();

  // std::cerr<<"A = "<<A<<endl;

  /*------------- Compute sequence properties --------------*/
  valarray<bool> group1 = T.partition(nodes[0],nodes[1]);
  valarray<bool> group2 = T.partition(nodes[0],nodes[2]);
  valarray<bool> group3 = T.partition(nodes[0],nodes[3]);

  //  std::clog<<"n0 = "<<nodes[0]<<"   n1 = "<<nodes[1]<<"    n2 = "<<nodes[2]<<"    n3 = "<<nodes[3]<<std::endl;
  //  std::clog<<"A (reordered) = "<<project(A,nodes[0],nodes[1],nodes[2],nodes[3])<<endl;

  //  THIS is not the same as getorder(project(...)) because, even if we select the same columns
  // they may have different indices (remember that project(A) is shorter than A).
  //  Since we use the columns from A, we need to use getorder(A)...
  // However, the NUMBER of columns should be the same. 
  // Hmm.. how to check that we get the same columns - need to deal w/ renaming issue...
  vector<int> columns = getorder(A,nodes[0],nodes[1],nodes[2],nodes[3]);

  vector<int> columns2 = getorder(project(A,nodes[0],nodes[1],nodes[2],nodes[3]),0,1,2,3);
  assert(columns.size() == columns2.size());

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq23;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    if (not A.gap(column,nodes[1]))
      seq1.push_back(column);
    if (not A.gap(column,nodes[2]))
      seq2.push_back(column);
    if (not A.gap(column,nodes[3]))
      seq3.push_back(column);

    if (not A.gap(column,nodes[2]) or not A.gap(column,nodes[3]))
      seq23.push_back(column);
  }

  // Map columns with n2 or n3 to single index 'c'
  vector<int> jcol(seq23.size()+1);
  vector<int> kcol(seq23.size()+1);

  jcol[0] = 0;
  kcol[0] = 0;
  for(int c=1,j=0,k=0;c<seq23.size()+1;c++) {
    if (not A.gap(seq23[c-1],nodes[2]))
      j++;    
    if (not A.gap(seq23[c-1],nodes[3]))
      k++;
    jcol[c] = j;
    kcol[c] = k;
  }

  // Precompute distributions at nodes[0]
  distributions_t distributions = distributions_tree;
  if (not P.SModel().full_tree)
    distributions = distributions_star;

  vector< vector< valarray<double> > > dists1 = distributions(A,P,seq1,nodes[0],group1);
  vector< vector< valarray<double> > > dists23 = distributions(A,P,seq23,nodes[0],group2|group3);


  /*-------------- Create alignment matrices ---------------*/

  const Matrix Q = createQ(P.IModel());

  // Actually create the Matrices & Chain
  DPmatrixConstrained Matrices(get_state_emit(), get_start_P(pi), Q, P.Temp,
		       P.SModel().distribution(), dists1, dists23, frequency);

  // Determine state order - FIXME - make this part of dpmatrix.H (part of HMM)
  vector<int> state_order(nstates);
  for(int i=0;i<state_order.size();i++)
    state_order[i] = i;
  std::swap(state_order[7],state_order[nstates-1]);        // silent states must be last

  // Determine which states are allowed to match (,c2)
  for(int c2=0;c2<Matrices.size2();c2++) {
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    for(int i=0;i<state_order.size();i++) {
      int S2 = state_order[i];

      //---------- Get (,j1,k1) ----------
      int j1 = j2;
      if (dj(S2)) 
	j1--;

      int k1 = k2;
      if (dk(S2)) 
	k1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 or (j1 == j2 and k1 == k2) or (j1 == jcol[c2-1] and k1 == kcol[c2-1]) )
	Matrices.states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  /*------------------ Compute the DP matrix ---------------------*/

  Matrices.prune();
  
  //  vector<int> path_old = get_path_3way(project(A,nodes[0],nodes[1],nodes[2],nodes[3]),0,1,2,3);
  //  vector<int> path_old_g = Matrices.generalize(path_old);

  //  vector<int> path_g = Matrices.forward(P.features,(int)P.constants[0],path_old_g);
  Matrices.forward_square();
  vector<int> path_g = Matrices.sample_path();

  vector<int> path = Matrices.ungeneralize(path_g);

  A = construct(A,path,nodes[0],nodes[1],nodes[2],nodes[3],T,seq1,seq2,seq3);

#ifndef NDEBUG_DP
  //--------------- Check alignment construction ------------------//
  vector<int> path_new = get_path_3way(project(A,nodes),0,1,2,3);

  vector<int> path_new2 = get_path_3way(A,nodes);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.
                                 //    due to ordering stuff required in the path but
                                 //    not store in the alignment A.
  vector<int> path_new_g = Matrices.generalize(path_new);
  if (path_new_g != path_g) {
    std::clog<<"A' (reordered) = "<<project(A,nodes)<<endl;
    std::clog<<"A' = "<<A<<endl;
    std::abort();
  }

  assert(valid(A));
#endif

  std::cerr<<"[tri]bandwidth = "<<bandwidth(Matrices,path_g)<<std::endl;

  std::cerr<<"[tri]bandwidth2 = "<<bandwidth2(Matrices,path_g)<<std::endl;

  return Matrices;
}


alignment tri_sample_alignment(const alignment& old,const Parameters& P,int node1,int node2) {

  /*------------(Gibbs) sample from proposal distribution ------------------*/

  alignment A = old;
  vector<int> nodes = get_nodes_branch_random(P.T,node1,node2);

  DPmatrixConstrained Matrices =  tri_sample_alignment_base(A,P,nodes);

#ifndef NDEBUG_DP
  /*---------- get the paths through the 3way alignment, from the entire alignment ----------*/
  vector<int> path_old = get_path_3way(project(old,nodes),0,1,2,3);
  vector<int> path_new = get_path_3way(project(A,nodes),0,1,2,3);

  //-------------- Check relative path probabilities --------------//
  double s1 = P.likelihood(old,P);
  double s2 = P.likelihood(A,P);

  double lp1 = prior_HMM_nogiven(old,P)/P.Temp;
  double lp2 = prior_HMM_nogiven(A  ,P)/P.Temp;

  double diff = Matrices.check(path_old,path_new,lp1,s1,lp2,s2);

  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,nodes)<<endl;
    std::cerr<<project(A,nodes)<<endl;

    throw myexception()<<__PRETTY_FUNCTION__<<": sampling probabilities were incorrect";
  }
#endif

  /*--------------(MH) Adjust for length of nodes[0] changing --------------------*/
  int length_old = old.seqlength(node1);
  int length_new = A.seqlength(node1);

  double log_ratio = 2.0*(P.IModel().lengthp(length_old)-P.IModel().lengthp(length_new));
  if (myrandomf() < exp(log_ratio))
    return A;
  else
    return old;
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probably of the other branch alignments is unaffected...
bool tri_sample_alignment_branch(alignment& A,Parameters& P1,
				 int node1,int node2,int b,double length2)
{
  //----------- Generate the Different Matrices ---------//
  vector<alignment> a(3,A);
  vector<Parameters> p(3,P1);
  p[2].setlength(b,length2);

  vector<int> nodes = get_nodes_branch_random(P1.T,node1,node2);


  vector<DPmatrixConstrained> Matrices;
  Matrices.push_back(tri_sample_alignment_base(a[1],p[1],nodes) );
  Matrices.push_back(Matrices[0]);
  Matrices.push_back(tri_sample_alignment_base(a[2],p[2],nodes) );
  
  /*----------------------- Choose between P1 and P2 --------------------------*/

  vector<double> Pr(2);
  Pr[0] = Matrices[1].Pr_sum_all_paths() + prior(p[1])/p[1].Temp;
  Pr[1] = Matrices[2].Pr_sum_all_paths() + prior(p[2])/p[2].Temp;

  int C = 1 + choose(Pr);

#ifndef NDEBUG_DP
  vector< vector<int> > paths;
  vector<double> OP(3);
  vector<double> OS(3);

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<3;i++) {
    vector<int> path   = get_path_3way(A3::project(a[i],nodes),0,1,2,3);
    paths.push_back( path ); 

    OP[i] = other_prior(a[i],p[i],nodes);
    OS[i] = other_subst(a[i],p[i],nodes);
    double OP_i = OP[i] - A3::log_correction(a[i],p[i],nodes);

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
    PR[i][0] += A3::log_correction(a[i],p[i],nodes);
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

    std::cerr<<A3::project(a[0],nodes)<<endl;
    std::cerr<<A3::project(a[C],nodes)<<endl;

    throw myexception()<<__PRETTY_FUNCTION__<<": sampling probabilities were incorrect";
  }
#endif

  /*---------------- Adjust for length of node0 (nodes[0]) changing --------------------*/

  bool success = false;
  if (myrandomf() < exp( A3::log_acceptance_ratio(a[0],p[0],nodes,a[C],p[C],nodes)))  {
    success = (C == 2);
    A = a[C];
    P1 = p[C];
  }

  return success;
}
