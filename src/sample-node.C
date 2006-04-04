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
#include "likelihood.H"    // for prior()
#include "substitution-index.H"
#include "refcount.H"
#include "dp-array.H"

//TODO - 1. calculate the probability of 
//  a) the path we came in with
//  b) the path we chose
//  c) the most probable path?

// 2. Calculate the likelihood of the reassembled matrix and the original matrix
//     - see if the difference is the same as the difference between the path probabilities

//Assumptions:
//  a) we assume that the internal node is the parent
//     sequence in each of the sub-alignments

using std::abs;
using std::valarray;

using namespace A3;

RefPtr<DParrayConstrained> sample_node_base(alignment& A,const Parameters& P,const vector<int>& nodes) {
  const Tree& T = P.T;

  assert(P.IModel().full_tree);

  alignment old = A;

  //  std::cerr<<"old = "<<old<<endl;

  /*------------- Compute sequence properties --------------*/
  int n0 = nodes[0];
  int n1 = nodes[1];
  int n2 = nodes[2];
  int n3 = nodes[3];
  vector<int> columns = getorder(old,n0,n1,n2,n3);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq123;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    if (not old.gap(column,n1))
      seq1.push_back(column);
    if (not old.gap(column,n2))
      seq2.push_back(column);
    if (not old.gap(column,n3))
      seq3.push_back(column);
    if (not old.gap(column,n1) or not old.gap(column,n2) or not old.gap(column,n3))
      seq123.push_back(column);
  }

  // Map columns with n2 or n3 to single index 'c'
  vector<int> icol(seq123.size()+1);
  vector<int> jcol(seq123.size()+1);
  vector<int> kcol(seq123.size()+1);

  icol[0] = 0;
  jcol[0] = 0;
  kcol[0] = 0;
  for(int c=1,i=0,j=0,k=0;c<seq123.size()+1;c++) {
    if (not old.gap(seq123[c-1],n1))
      i++;    
    if (not old.gap(seq123[c-1],n2))
      j++;    
    if (not old.gap(seq123[c-1],n3))
      k++;
    icol[c] = i;
    jcol[c] = j;
    kcol[c] = k;
  }


  /*-------------- Create alignment matrices ---------------*/

  // Cache which states emit which sequences
  vector<int> state_emit(nstates+1);
  for(int S2=0;S2<state_emit.size();S2++) {
    state_emit[S2] = 0;

    if (di(S2) or dj(S2) or dk(S2)) 
      state_emit[S2] |= (1<<0);
  }


  vector<int> branches;
  for(int i=1;i<nodes.size();i++)
    branches.push_back(T.branch(nodes[0],nodes[i]) );

  const Matrix Q = createQ(P.branch_HMMs,branches);
  vector<double> start_P = get_start_P(P.branch_HMMs,branches);
  

  // Actually create the Matrices & Chain
  RefPtr<DParrayConstrained> Matrices = new DParrayConstrained(seq123.size(),state_emit,start_P,Q, P.beta[0]);

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices->size();c2++) {
    int i2 = icol[c2];
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    Matrices->states(c2).reserve(Matrices->nstates());
    for(int i=0;i<Matrices->nstates();i++) {
      int S2 = Matrices->order(i);

      //---------- Get (,j1,k1) ----------
      int i1 = i2;
      if (di(S2)) i1--;

      int j1 = j2;
      if (dj(S2)) j1--;

      int k1 = k2;
      if (dk(S2)) k1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 
	  or (i1 == i2 and j1 == j2 and k1 == k2) 
	  or (i1 == icol[c2-1] and j1 == jcol[c2-1] and k1 == kcol[c2-1]) )
	Matrices->states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  /*------------------ Compute the DP matrix ---------------------*/
  // Matrices.prune();  prune is broken!
  Matrices->forward();

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices->sample_path();
  vector<int> path = Matrices->ungeneralize(path_g);

  A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);

#ifndef NDEBUG
  vector<int> path_new = get_path_3way(project(A,n0,n1,n2,n3),0,1,2,3);
  vector<int> path_new2 = get_path_3way(A,n0,n1,n2,n3);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_g = Matrices->generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(A));


#endif

  return Matrices;
}

int sample_node_multi(vector<alignment>& a,vector<Parameters>& p,const vector< vector<int> >& nodes_,
		      const vector<efloat_t>& rho_, bool do_OS,bool do_OP) {

  vector<vector<int> > nodes = nodes_;
  vector<efloat_t> rho = rho_; 
  assert(p.size() == nodes.size());
 
  //----------- Generate the different states and Matrices ---------//
  const alignment A0 = a[0];
#ifndef NDEBUG_DP
  const Parameters P0 = p[0];
#endif

  vector< RefPtr<DParrayConstrained> > Matrices;
  for(int i=0;i<p.size();i++) {
    Matrices.push_back( sample_node_base(a[i],p[i],nodes[i]) );
    //    p[i].LC.invalidate_node(p[i].T,nodes[i][0]);
#ifndef NDEBUG
    if (i==0) substitution::check_subA(A0,a[0],p[0].T);
    p[i].likelihood(a[i],p[i]);  // check the likelihood calculation
#endif
  }

  //-------- Calculate corrections to path probabilities ---------//

  vector<efloat_t> OS(p.size(),1);
  vector<efloat_t> OP(p.size(),1);
  for(int i=0; i<p.size(); i++) {
    if (do_OS)
      OS[i] = p[i].likelihood(a[i],p[i]);
    if (do_OP)
      OP[i] = other_prior(a[i],p[i],nodes[i]);
  }

  //---------------- Calculate choice probabilities --------------//
  vector<efloat_t> Pr(p.size());
  for(int i=0;i<Pr.size();i++)
    Pr[i] = rho[i] * OS[i] * Matrices[i]->Pr_sum_all_paths() * OP[i] * pow(prior(p[i]), p[i].beta[1]);

  int C = choose_MH(0,Pr);

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  valarray<bool> ignore(false,p[0].T.n_nodes());
  ignore[ nodes[0][0] ] = true;

  // Check that our constraints are met
  for(int i=0;i<a.size();i++) {
    if (not(A_constant(A0,a[i],ignore))) {
      std::cerr<<A0<<endl;
      std::cerr<<a[i]<<endl;
      assert(A_constant(A0,a[i],ignore));
    }
  }

  // Add another entry for the incoming configuration
  a.push_back( A0 );
  p.push_back( P0 );
  nodes.push_back(nodes[0]);
  rho.push_back( rho[0] );
  Matrices.push_back( Matrices[0] );
  OS.push_back( OS[0] );
  OP.push_back( OP[0] );

  vector< vector<int> > paths;

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) {
    paths.push_back( get_path_3way(A3::project(a[i],nodes[i]),0,1,2,3) );
    
    OS[i] = p[i].likelihood(a[i],p[i]);
    OP[i] = other_prior(a[i],p[i],nodes[i]);

    efloat_t OP_i = OP[i] / A3::correction(a[i],p[i],nodes[i]);

    check_match_P(a[i], p[i], OS[i], OP_i, paths[i], *Matrices[i]);
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<efloat_t> > PR(p.size());

  for(int i=0;i<p.size();i++) {
    efloat_t proposal_ratio = 1;
    if (i<Pr.size())
      proposal_ratio = choose_MH_P(0,i,Pr)/choose_MH_P(i,0,Pr);
    else
      proposal_ratio = 1;

    PR[i] = sample_P(a[i], p[i], proposal_ratio, rho[i], paths[i], *Matrices[i]);
    PR[i][0] *= A3::correction(a[i],p[i],nodes[i]);
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR,a);
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we reject the move, then don't do anything
  if (myrandomf() > acceptance_ratio(A0,p[0],nodes[0],a[C],p[C],nodes[C]))
    return -1;

  return C;
}





void sample_node(alignment& A,Parameters& P,int node) 
{
  const Tree& T = P.T;

  vector<alignment> a(1,A);
  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = get_nodes_random(T,node);


  vector<efloat_t> rho(1,1);

  int C = sample_node_multi(a,p,nodes,rho,false,false);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }
}
