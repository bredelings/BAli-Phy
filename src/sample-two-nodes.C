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
#include "alignment-sums.H"
#include "alignment-util.H"

// for prior(p[i])
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

// IDEA: make a routine which encapsulates this sampling, and passes back
//  the total_sum.  Then we can just call sample_two_nodes w/ each of the 3 trees.
// We can choose between them with the total_sum (I mean, sum_all_paths).
// Then, we can just debug one routine, basically.

DParrayConstrained sample_two_nodes_base(alignment& A,const Parameters& P,const vector<int>& nodes) {

  const Tree& T = P.T;
  alignment old = A;

  //  std::cerr<<"old = "<<old<<endl;

  /*------------- Compute sequence properties --------------*/
  vector<int> columns = getorder(old,nodes);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,nodes)<<endl;

  // Find sub-alignments and sequences
  vector<vector<int> > seqs(4);
  vector<int> seqall;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    for(int i=0;i<4;i++)
      if (not old.gap(column,nodes[i]))
	seqs[i].push_back(column);

    if (not old.gap(column,nodes[0]) or 
	not old.gap(column,nodes[1]) or 
	not old.gap(column,nodes[2]) or 
	not old.gap(column,nodes[3]))
      seqall.push_back(column);
  }

  // Map columns with n2 or n3 to single index 'c'
  //  vector< vector<int> > cols(4,vector<int>(seqall.size()+1));
  vector<int> icol(seqall.size()+1);
  vector<int> jcol(seqall.size()+1);
  vector<int> kcol(seqall.size()+1);
  vector<int> lcol(seqall.size()+1);

  icol[0] = 0;
  jcol[0] = 0;
  kcol[0] = 0;
  lcol[0] = 0;
  for(int c=1,i=0,j=0,k=0,l=0;c<seqall.size()+1;c++) {
    if (not old.gap(seqall[c-1],nodes[0]))
      i++;    
    if (not old.gap(seqall[c-1],nodes[1]))
      j++;    
    if (not old.gap(seqall[c-1],nodes[2]))
      k++;
    if (not old.gap(seqall[c-1],nodes[3]))
      l++;
    icol[c] = i;
    jcol[c] = j;
    kcol[c] = k;
    lcol[c] = l;
  }


  /*-------------- Create alignment matrices ---------------*/

  // Construct the 1D state-emit matrix from the 6D one
  vector<int> state_emit_1D = A5::states_list;
  for(int S2=0;S2<state_emit_1D.size();S2++) {
    int state_emit = state_emit_1D[S2]&leafbitsmask;
    if (state_emit)
      state_emit_1D[S2] = 1;
    else
      state_emit_1D[S2] = 0;
  }
  
  // Create the transition matrix first using just the current, fixed ordering
  vector<int> branches(5);
  branches[0] = T.branch(nodes[0],nodes[4]);
  branches[1] = T.branch(nodes[1],nodes[4]);
  branches[2] = T.branch(nodes[2],nodes[5]);
  branches[3] = T.branch(nodes[3],nodes[5]);
  branches[4] = T.branch(nodes[4],nodes[5]);
  const eMatrix Q = createQ(P.branch_HMMs,branches,A5::states_list);
  vector<efloat_t> start_P = get_start_P(P.branch_HMMs,branches,A5::states_list);

  // Actually create the Matrices & Chain
  DParrayConstrained Matrices(seqall.size(), 
			      state_emit_1D, 
			      start_P,
			      Q, 
			      P.Temp);

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices.size();c2++) {
    int i2 = icol[c2];
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    int l2 = lcol[c2];
    for(int i=0;i<Matrices.nstates();i++) {
      int S2 = Matrices.order(i);
      int state2 = A5::states_list[S2];

      //---------- Get (,j1,k1) ----------
      int i1 = i2;
      if (bitset(state2,0)) i1--;

      int j1 = j2;
      if (bitset(state2,1)) j1--;

      int k1 = k2;
      if (bitset(state2,2)) k1--;

      int l1 = l2;
      if (bitset(state2,3)) l1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 
	  or (i1 == i2 and j1 == j2 and k1 == k2 and l1 == l2) 
	  or (i1 == icol[c2-1] and j1 == jcol[c2-1] and k1 == kcol[c2-1] and l1 == lcol[c2-1]) )
	Matrices.states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }

  /*------------------ Compute the DP matrix ---------------------*/

  //  Matrices.prune(); broken!
  Matrices.forward();

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices.sample_path();
  vector<int> path = Matrices.ungeneralize(path_g);

  //  std::cerr<<"generalized A = \n"<<construct(old,path_g,nodes,T,seqs,A5::states_list)<<endl;
  //  std::cerr<<"ungeneralized A = \n"<<construct(old,path,nodes,T,seqs,A5::states_list)<<endl;

  A = construct(old,path,nodes,T,seqs,A5::states_list);

  //  std::cerr<<"A = \n"<<construct(old,path,nodes,T,seqs,A5::states_list)<<endl;

#ifndef NDEBUG_DP
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_new = get_path(project(A,nodes),newnodes,A5::states_list);
  vector<int> path_new2 = get_path(A,nodes,A5::states_list);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_g = Matrices.generalize(path_new);
  assert(path_new_g == path_g);
  assert(path_new   == path);
  assert(valid(A));
#endif

  return Matrices;
}

///FIXME - make a generic routine (templates?)

bool sample_two_nodes_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP) {

  assert(p.size() == nodes.size());
  
  //----------- Generate the different states and Matrices ---------//

  Parameters P_save = p[0];

  vector<alignment> a(p.size(),A);

  vector< DParrayConstrained > Matrices;
  for(int i=0;i<p.size();i++) {
    Matrices.push_back( sample_two_nodes_base(a[i],p[i],nodes[i]) );
    //    p[i].LC.invalidate_node(p[i].T,nodes[i][4]);
    //    p[i].LC.invalidate_node(p[i].T,nodes[i][5]);
#ifndef NDEBUG
    p[i].likelihood(a[i],p[i]);  // check the likelihood calculation
#endif
  }

  //-------- Calculate corrections to path probabilities ---------//

  vector<double> OS(p.size(),0);
  vector<double> OP(p.size(),0);
  for(int i=0; i<p.size(); i++) {
    if (do_OS) 
      OS[i] = p[i].likelihood(a[i],p[i]);
    if (do_OP)
      OP[i] = other_prior(a[i],p[i],nodes[i]);
  }

  //---------------- Calculate choice probabilities --------------//
  vector<double> Pr(p.size());
  for(int i=0;i<Pr.size();i++)
    Pr[i] = OS[i] + Matrices[i].Pr_sum_all_paths() + OP[i] + prior(p[i])/p[i].Temp;

  int C = choose(Pr);

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  valarray<bool> ignore(false,p[0].T.n_nodes());
  ignore[ nodes[0][4] ] = true;
  ignore[ nodes[0][5] ] = true;

  // Check that our constraints are met
  for(int i=0;i<a.size();i++) {
    if (not (A_constant(A,a[i],ignore))) {
      std::cerr<<A<<endl;
      std::cerr<<a[i]<<endl;
      assert(A_constant(A,a[i],ignore));
    }
  }

  // Add another entry for the incoming configuration
  a.push_back( A );
  p.push_back( P_save );
  nodes.push_back(nodes[0]);
  Matrices.push_back( Matrices[0] );
  OS.push_back( OS[0] );
  OP.push_back( OP[0] );

  vector< vector<int> > paths;

  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) {
    paths.push_back( get_path(A5::project(a[i],nodes[i]),newnodes,A5::states_list) );
    
    OS[i] = p[i].likelihood(a[i],p[i]);
    OP[i] = other_prior(a[i],p[i],nodes[i]);

    double OP_i = OP[i] - A5::log_correction(a[i],p[i],nodes[i]);

    check_match_P(a[i], p[i], OS[i], OP_i, paths[i], Matrices[i]);
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<double> > PR(p.size());

  for(int i=0;i<p.size();i++) {
    double P_choice = 0;
    if (i<Pr.size())
      P_choice = choose_P(i,Pr);
    else
      P_choice = choose_P(0,Pr);

    PR[i] = sample_P(a[i], p[i], OS[i], OP[i] , P_choice, paths[i], Matrices[i]);
    PR[i][0] += A5::log_correction(a[i],p[i],nodes[i]);
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  for(int i=0;i<PR.size();i++) {
    std::cerr<<"option = "<<i<<endl;

    std::cerr<<" Pr1  = "<<PR.back()[0]<<"    Pr2  = "<<PR[i][0]<<"    Pr2  - Pr1  = "<<PR[i][0] - PR[0][0]<<endl;
    std::cerr<<" PrQ1 = "<<PR.back()[2]<<"    PrQ2 = "<<PR[i][2]<<"    PrQ2 - PrQ1 = "<<PR[i][2] - PR[0][2]<<endl;
    std::cerr<<" PrS1 = "<<PR.back()[1]<<"    PrS2 = "<<PR[i][1]<<"    PrS2 - PrS1 = "<<PR[i][1] - PR[0][1]<<endl;
    
    double diff = (PR[i][1] - PR.back()[1]) - (PR[i][0] - PR.back()[0]);
    std::cerr<<"diff = "<<diff<<endl;
    if (std::abs(diff) > 1.0e-9) {
      std::cerr<<a.back()<<endl;
      std::cerr<<a[i]<<endl;
      
      std::cerr<<A5::project(a.back(),nodes.back());
      std::cerr<<A5::project(a[C],nodes[C]);

      std::cerr<<"sampling probabilities were incorrect"<<std::endl;
      std::abort();
    }
  }
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we accept the move, then record the changes
  bool success = false;
  if (myrandomf() < exp(A5::log_acceptance_ratio(A,p[0],nodes[0],a[C],p[C],nodes[C]))) {
    success = (C > 0);

    A = a[C];

    if (success)
      p[0] = p[C];
  }
  else
    p[0] = P_save;

  return success;
}


void sample_two_nodes(alignment& A, Parameters& P,int b) {

  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = A5::get_nodes_random(P.T,b);

  sample_two_nodes_multi(A,p,nodes,false,false);
  P = p[0];
}
