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
#include "alignment-util.H"
#include "alignment-constraint.H"
#include "likelihood.H"    // for prior()
#include "refcount.H"
#include "dp-matrix.H"

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::valarray;

using namespace A3;

// FIXME - actually resample the path multiple times - pick one on
// opposite side of the middle 
RefPtr<DPmatrixConstrained> tri_sample_alignment_base(alignment& A,const Parameters& P,const vector<int>& nodes) {
  const Tree& T = P.T;

  assert(P.IModel().full_tree);

  assert(T.is_connected(nodes[0],nodes[1]));
  assert(T.is_connected(nodes[0],nodes[2]));
  assert(T.is_connected(nodes[0],nodes[3]));

  const Matrix frequency = substitution::frequency_matrix(P.SModel());

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

  vector< Matrix > dists1 = distributions(A,P,seq1,nodes[0],group1);
  vector< Matrix > dists23 = distributions(A,P,seq23,nodes[0],group2|group3);


  //-------------- Create alignment matrices ---------------//

  vector<int> branches;
  for(int i=1;i<nodes.size();i++)
    branches.push_back(T.branch(nodes[0],nodes[i]) );

  const Matrix Q = createQ(P.branch_HMMs, branches);
  vector<double> start_P = get_start_P(P.branch_HMMs,branches);

  // Actually create the Matrices & Chain
  RefPtr<DPmatrixConstrained> Matrices = new DPmatrixConstrained(get_state_emit(), start_P, Q, P.Temp,
								 P.SModel().distribution(), dists1, dists23, frequency);

  // Determine which states are allowed to match (,c2)
  for(int c2=0;c2<dists23.size()-1;c2++) {
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    for(int i=0;i<Matrices->nstates();i++) {
      int S2 = Matrices->order(i);

      //---------- Get (,j1,k1) ----------
      int j1 = j2;
      if (dj(S2)) 
	j1--;

      int k1 = k2;
      if (dk(S2)) 
	k1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 or (j1 == j2 and k1 == k2) or (j1 == jcol[c2-1] and k1 == kcol[c2-1]) )
	Matrices->states(c2+1).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  //------------------ Compute the DP matrix ---------------------//

  //   Matrices.prune(); prune is broken!
  
  //  vector<int> path_old = get_path_3way(project(A,nodes[0],nodes[1],nodes[2],nodes[3]),0,1,2,3);
  //  vector<int> path_old_g = Matrices.generalize(path_old);

  //  vector<int> path_g = Matrices.forward(P.features,(int)P.constants[0],path_old_g);
  vector<vector<int> > pins = get_pins(P.alignment_constraint,A,group1,group2 or group3,seq1,seq23);
  vector<int> path_g = Matrices->forward(pins);

  if (Matrices->Pr_sum_all_paths() <= 0.0)
    return Matrices;

  vector<int> path = Matrices->ungeneralize(path_g);

  A = construct(A,path,nodes[0],nodes[1],nodes[2],nodes[3],T,seq1,seq2,seq3);

#ifndef NDEBUG_DP
  //--------------- Check alignment construction ------------------//
  vector<int> path_new = get_path_3way(project(A,nodes),0,1,2,3);

  vector<int> path_new2 = get_path_3way(A,nodes);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.
                                 //    due to ordering stuff required in the path but
                                 //    not store in the alignment A.
  vector<int> path_new_g = Matrices->generalize(path_new);
  if (path_new_g != path_g) {
    std::clog<<"A' (reordered) = "<<project(A,nodes)<<endl;
    std::clog<<"A' = "<<A<<endl;
    std::abort();
  }

  assert(valid(A));
#endif

  //  std::cerr<<"[tri]bandwidth = "<<bandwidth(Matrices,path_g)<<std::endl;

  //  std::cerr<<"[tri]bandwidth2 = "<<bandwidth2(Matrices,path_g)<<std::endl;

#ifndef NDEBUG
  check_alignment(A,T,"sample_tri_base:out");
#endif
  return Matrices;
}


int sample_tri_multi(vector<alignment>& a,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP) 
{
  assert(p.size() == nodes.size());

  //----------- Generate the different states and Matrices ---------//
#ifndef NDEBUG
  const alignment A0 = a[0];
  const Parameters P0 = p[0];
#endif

  vector<RefPtr<DPmatrixConstrained> > Matrices;
  for(int i=0;i<p.size();i++) 
  {
    Matrices.push_back( tri_sample_alignment_base(a[i],p[i],nodes[i]) );

    p[i].LC.set_length(a[i].length());
    int b = p[i].T.branch(nodes[i][0],nodes[i][1]);
    p[i].LC.invalidate_branch_alignment(p[i].T, b);
    //    p[i].LC.invalidate_node(p[i].T,nodes[i][0]);
#ifndef NDEBUG
    p[i].likelihood(a[i],p[i]);  // check the likelihood calculation
#endif
  }

  //-------- Calculate corrections to path probabilities ---------//

  vector<efloat_t> OS(p.size(),1);
  vector<efloat_t> OP(p.size(),1);
  for(int i=0; i<p.size(); i++) {
    if (do_OS)
      OS[i] = other_subst(a[i],p[i],nodes[i]);
    if (do_OP)
      OP[i] = other_prior(a[i],p[i],nodes[i]);
  }

  //---------------- Calculate choice probabilities --------------//
  vector<efloat_t> Pr(p.size());
  for(int i=0;i<Pr.size();i++)
    Pr[i] = OS[i] * Matrices[i]->Pr_sum_all_paths() * OP[i] * pow(prior(p[i]),1.0/p[i].Temp);
  assert(Pr[0] > 0.0);

  int C = choose(Pr);

  assert(Pr[C] > 0.0);

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  valarray<bool> ignore1A = not p[0].T.partition(nodes[0][0],nodes[0][1]);
  valarray<bool> ignore2A = not (p[0].T.partition(nodes[0][0],nodes[0][2]) or p[0].T.partition(nodes[0][0],nodes[0][3]) );
  valarray<bool> ignore1(p[0].T.n_nodes()); 
  valarray<bool> ignore2(p[0].T.n_nodes()); 
  for(int i=0;i<ignore1.size();i++) {
    ignore1[i] = ignore1A[i];
    ignore2[i] = ignore2A[i];
  }

  // Don't check impossible combinations
  for(int i=a.size()-1;i>=1;i--) {
    if (Matrices[i]->Pr_sum_all_paths() > 0.0) continue;

    a.erase(a.begin()+i);
    p.erase(p.begin()+i);
    nodes.erase(nodes.begin()+i);
    Matrices.erase(Matrices.begin()+i);
    OS.erase(OS.begin()+i);
    OP.erase(OP.begin()+i);
  }

  // Check that our constraints are met
  for(int i=0;i<a.size();i++) {
    if (not(A_constant(A0,a[i],ignore1))) {
      std::cerr<<A0<<endl;
      std::cerr<<a[i]<<endl;
      assert(A_constant(A0,a[i],ignore1));
    }
    assert(A_constant(A0,a[i],ignore2));
  }
  // Add another entry for the incoming configuration
  a.push_back( A0 );
  p.push_back( P0 );
  nodes.push_back(nodes[0]);
  Matrices.push_back( Matrices[0] );
  OS.push_back( OS[0] );
  OP.push_back( OP[0] );

  vector< vector<int> > paths;

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) {
    paths.push_back( get_path_3way(A3::project(a[i],nodes[i]),0,1,2,3) );
    
    OS[i] = other_subst(a[i],p[i],nodes[i]);
    OP[i] = other_prior(a[i],p[i],nodes[i]);

    efloat_t OP_i = OP[i] / A3::correction(a[i],p[i],nodes[i]);

    check_match_P(a[i], p[i], OS[i], OP_i, paths[i], *Matrices[i]);
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<efloat_t> > PR(p.size());

  for(int i=0;i<p.size();i++) {
    efloat_t P_choice = 1;
    if (i<Pr.size())
      P_choice = choose_P(i,Pr);
    else
      P_choice = choose_P(0,Pr);

    PR[i] = sample_P(a[i], p[i], OS[i], OP[i] , P_choice, paths[i], *Matrices[i]);
    PR[i][0] *= A3::correction(a[i],p[i],nodes[i]);
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR,a);
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we reject the move, then don't do anything
  if (myrandomf() > acceptance_ratio(a[0],p[0],nodes[0],a[C],p[C],nodes[C]))
    return -1;

  return C;
}



void tri_sample_alignment(alignment& A,Parameters& P,int node1,int node2) {

  valarray<bool> s1 = constraint_satisfied(P.alignment_constraint,A);

  /*------------(Gibbs) sample from proposal distribution ------------------*/
#ifndef NDEBUG
  check_alignment(A,P.T,"tri_sample_alignment:in");
#endif
  vector<alignment> a(1,A);
  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = get_nodes_branch_random(P.T,node1,node2);

  int C = sample_tri_multi(a,p,nodes,false,false);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }

#ifndef NDEBUG
  check_alignment(A,P.T,"tri_sample_alignment:out");
#endif

  valarray<bool> s2 = constraint_satisfied(P.alignment_constraint,A);
  report_constraints(s1,s2);
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probability of the other branch alignments is unaffected...

bool tri_sample_alignment_branch(alignment& A,Parameters& P,
				 int node1,int node2,int b,double length2)
{
  //----------- Generate the Different Matrices ---------//
  vector<alignment> a(1,A);
  vector<Parameters> p(2,P);
  p[1].setlength(b,length2);

  vector< vector<int> > nodes (2, get_nodes_branch_random(P.T,node1,node2) );

  int C = sample_tri_multi(a,p,nodes,false,false);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }

  return (C > 0);
}
