/*
   Copyright (C) 2004-2007,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file sample-two-nodes.C
///
/// \brief Contains routines for resampling the sequence at two adjacent internal nodes (5way).
///

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
#include "alignment-constraint.H"
#include "substitution-index.H"
#include <boost/numeric/ublas/io.hpp>
#include "dp-array.H"
#include "timer_stack.H"

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

using std::vector;
using std::abs;
using std::endl;

using boost::dynamic_bitset;

using namespace A5;

// IDEA: make a routine which encapsulates this sampling, and passes back
//  the total_sum.  Then we can just call sample_two_nodes w/ each of the 3 trees.
// We can choose between them with the total_sum (I mean, sum_all_paths).
// Then, we can just debug one routine, basically.

void sample_two_nodes_base(data_partition& P,const vector<int>& nodes,
			   DParrayConstrained*& Matrices)
{
  default_timer_stack.push_timer("alignment::DP1/5-way");
  const Tree& T = P.T();
  alignment& A = *P.A.modify();
  alignment old = A;

  //  std::cerr<<"old = "<<old<<endl;

  //------------- Compute sequence properties --------------//
  vector<int> columns = getorder(old,nodes);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,nodes)<<endl;

  // Find sub-alignments and sequences
  vector<vector<int> > seqs(4);
  for(int i=0;i<seqs.size();i++)
    seqs[i].reserve(A.length());
  vector<int> seqall;
  seqall.reserve(A.length());
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
  vector<double> start_P = get_start_P( P.get_branch_HMMs(branches) );

  // Actually create the Matrices & Chain
  if (not Matrices) 
  {
    const Matrix Q = createQ( P.get_branch_HMMs(branches),A5::states_list);

    Matrices = new DParrayConstrained(seqall.size(), state_emit_1D, 
				      start_P, Q, 
				      P.get_beta());
  }
  else 
  {
    //A5::updateQ(Matrices->Q,P.branch_HMMs,branches,A5::states_list); // 7%
    A5::fillQ(Matrices->Q, P.get_branch_HMMs(branches), A5::states_list); // 16%
    Matrices->update_GQ();         // 12%
    Matrices->start_P = start_P;
    Matrices->set_length(seqall.size());
  }

  // collect the silent-or-correct-emissions for each type columns
  vector< vector<int> > allowed_states_for_mask(16);
  for(int i=0;i<Matrices->nstates();i++) 
  {
    int S2 = Matrices->order(i);
    int state2 = A5::states_list[S2] & 15; // 4 bits = 1+2+4+8
    if (state2 == 0)
      for(int j=0;j<16;j++)
	allowed_states_for_mask[j].push_back(S2);
    else
      allowed_states_for_mask[state2].push_back(S2);
  }

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices->size();c2++) 
  {
    vector<int>& allowed_states = Matrices->states(c2);
    allowed_states.clear();

    if (c2 == 0) {
      allowed_states.reserve(Matrices->nstates());
      for(int i=0;i<Matrices->nstates();i++)
	allowed_states.push_back(Matrices->order(i));
    }
    else {
      unsigned mask=0;

      if (icol[c2] != icol[c2-1]) { mask |= (1<<0); assert(icol[c2] == 1+icol[c2-1]); }
 
      if (jcol[c2] != jcol[c2-1]) { mask |= (1<<1); assert(jcol[c2] == 1+jcol[c2-1]); }

      if (kcol[c2] != kcol[c2-1]) { mask |= (1<<2); assert(kcol[c2] == 1+kcol[c2-1]); }

      if (lcol[c2] != lcol[c2-1]) { mask |= (1<<3); assert(lcol[c2] == 1+lcol[c2-1]); }

      assert(mask);

      allowed_states = allowed_states_for_mask[mask];
    }
  }

  //------------------ Compute the DP matrix ---------------------//

  Matrices->forward();

  // If the DP matrix ended up having probability 0, don't try to sample a path through it!
  if (Matrices->Pr_sum_all_paths() <= 0.0) 
  {
    std::cerr<<"sample_two_nodes_base( ): All paths have probability 0!"<<std::endl;
    default_timer_stack.pop_timer();
    return; // Matrices;
  }

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices->sample_path();
  vector<int> path = Matrices->ungeneralize(path_g);

  //  std::cerr<<"generalized A = \n"<<construct(old,path_g,nodes,T,seqs,A5::states_list)<<endl;
  //  std::cerr<<"ungeneralized A = \n"<<construct(old,path,nodes,T,seqs,A5::states_list)<<endl;

  A = construct(old,path,nodes,T,seqs,A5::states_list);

  P.set_pairwise_alignment(T.directed_branch(nodes[0],nodes[4]), A5::get_pairwise_alignment_from_path(path, 0, 4));
  P.set_pairwise_alignment(T.directed_branch(nodes[1],nodes[4]), A5::get_pairwise_alignment_from_path(path, 1, 4));
  P.set_pairwise_alignment(T.directed_branch(nodes[2],nodes[5]), A5::get_pairwise_alignment_from_path(path, 2, 5));
  P.set_pairwise_alignment(T.directed_branch(nodes[3],nodes[5]), A5::get_pairwise_alignment_from_path(path, 3, 5));
  P.set_pairwise_alignment(T.directed_branch(nodes[4],nodes[5]), A5::get_pairwise_alignment_from_path(path, 4, 5));

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
  vector<int> path_new_g = Matrices->generalize(path_new);
  assert(path_new_g == path_g);
  assert(path_new   == path);
  assert(valid(A));
#endif
  default_timer_stack.pop_timer();
}

static vector<vector<DParrayConstrained*> > cached_dparrays;

///(a[0],p[0]) is the point from which the proposal originates, and must be valid.
int sample_two_nodes_multi(vector<Parameters>& p,const vector< vector<int> >& nodes_,
			   const vector<efloat_t>& rho_,bool do_OS,bool do_OP) 
{

  vector<vector<int> > nodes = nodes_;
  vector<efloat_t> rho = rho_;
  assert(p.size() == nodes.size());
  
  //------------ Check the alignment branch constraints ------------//
  for(int i=0;i<p.size();i++) {
    vector<int> branches;

    branches.push_back(p[i].T->branch(nodes[i][0],nodes[i][4]));
    branches.push_back(p[i].T->branch(nodes[i][1],nodes[i][4]));
    branches.push_back(p[i].T->branch(nodes[i][2],nodes[i][5]));
    branches.push_back(p[i].T->branch(nodes[i][3],nodes[i][5]));
    branches.push_back(p[i].T->branch(nodes[i][4],nodes[i][5]));

    if (any_branches_constrained(branches, *p[i].T, *p[i].TC, p[i].AC))
      return -1;
  }

  //----------- Generate the different states and Matrices ---------//
  efloat_t C1 = A5::correction(p[0],nodes[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
  const Parameters P0 = p[0];
#endif

  // WARNING - cached_dparrays = funky magic
  if (cached_dparrays.size() < p.size())
    cached_dparrays.resize(p.size());
  for(int i=0;i<p.size();i++)
    if (cached_dparrays[i].size() < p[i].n_data_partitions())
      cached_dparrays[i].resize(p[i].n_data_partitions());

  
  vector< vector<DParrayConstrained*> > Matrices(p.size());
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	sample_two_nodes_base(p[i][j],nodes[i],cached_dparrays[i][j]);
	Matrices[i].push_back(cached_dparrays[i][j]);
	//    p[i][j].LC.invalidate_node(p[i].T,nodes[i][4]);
	//    p[i][j].LC.invalidate_node(p[i].T,nodes[i][5]);
#ifndef NDEBUG
	if (i==0) 
	  check_subA(*P0[j].subA, *P0[j].A, *p[0][j].subA, *p[0][j].A, *p[0].T);
	p[i][j].likelihood();  // check the likelihood calculation
#endif
      }
      else
	Matrices[i].push_back(NULL);
	


  //-------- Calculate corrections to path probabilities ---------//

  vector< vector<efloat_t> > OS(p.size());
  vector< vector<efloat_t> > OP(p.size());

  for(int i=0; i<p.size(); i++) 
  {
    if (do_OS)
      for(int j=0;j<p[i].n_data_partitions();j++)
	OS[i].push_back( p[i][j].likelihood() );
    else
      OS[i] = vector<efloat_t>(p[i].n_data_partitions(),efloat_t(1));
    
    if (do_OP)
      for(int j=0;j<p[i].n_data_partitions();j++)
	OP[i].push_back( other_prior(p[i][j],nodes[i]) );
    else
      OP[i] = vector<efloat_t>(p[i].n_data_partitions(),efloat_t(1));
  }

  //---------------- Calculate choice probabilities --------------//
  vector<efloat_t> Pr(p.size());

  for(int i=0;i<Pr.size();i++) 
  {
    Pr[i] = rho[i] * p[i].prior_no_alignment();

    // sum of substitution and alignment probability over all paths
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	Pr[i] *= Matrices[i][j]->Pr_sum_all_paths();
	Pr[i] *= pow(OS[i][j], p[i][j].get_beta());
	Pr[i] *= OP[i][j];
      }
      else
	Pr[i] *= p[i][j].heated_likelihood();
  }

  // Fail if Pr[0] is 0
  if (Pr[0] <= 0.0) return -1;

  int C = -1;
  try {
    C = choose_MH(0,Pr);
  }
  catch (choose_exception<efloat_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  dynamic_bitset<> ignore(p[0].T->n_nodes());
  ignore[ nodes[0][4] ] = true;
  ignore[ nodes[0][5] ] = true;

  // Check that our constraints are met
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (not A_constant(*P0[j].A, *p[i][j].A, ignore)) {
	std::cerr<<*P0[j].A<<endl;
	std::cerr<<*p[i][j].A<<endl;
	assert(A_constant(*P0[j].A, *p[i][j].A, ignore));
      }

  // Add another entry for the incoming configuration
  p.push_back( P0 );
  nodes.push_back(nodes[0]);
  rho.push_back( rho[0] );
  Matrices.push_back( Matrices[0] );
  OS.push_back( OS[0] );
  OP.push_back( OP[0] );

  vector< vector< vector<int> > >paths(p.size());

  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	paths[i].push_back( get_path(A5::project(*p[i][j].A, nodes[i]),newnodes,A5::states_list) );
    
	OS[i][j] = p[i][j].likelihood();
	OP[i][j] = other_prior(p[i][j],nodes[i]);
	
	efloat_t OP_i = OP[i][j] / A5::correction(p[i][j],nodes[i]);
	
	check_match_P(p[i][j], OS[i][j], OP_i, paths[i][j], *Matrices[i][j]);
      }
      else
	paths[i].push_back( vector<int>() );

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<efloat_t> > PR(p.size());

  for(int i=0;i<p.size();i++) 
  {
    efloat_t choice_ratio = 1;
    if (i<Pr.size())
      choice_ratio = choose_MH_P(0,i,Pr)/choose_MH_P(i,0,Pr);
    else
      choice_ratio = 1;
      
    // sample_P(p[i][j], choice_ratio, rho[i], paths[i][j], *Matrices[i][j]);
    // PR[i][j][0] *= A5::correction(p[i][j],nodes[i]);
    PR[i] = vector<efloat_t>(4,1);
    PR[i][0] = p[i].heated_probability(); // p[i].prior_no_alignment() * p[i].prior_alignment() * p[i].likelihood();
    PR[i][2] = rho[i];
    PR[i][3] = choice_ratio;
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment()) {
	vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
	PR[i][0] *= A5::correction(p[i][j],nodes[i]);
	PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
      } 
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR);
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we reject the move, then don't do anything
  efloat_t C2 = A5::correction(p[C],nodes[C]);
  if (myrandomf() > double(C1/C2))
    return -1;

  return C;
}


void sample_two_nodes(Parameters& P,int b) 
{
  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = A5::get_nodes_random(*P.T, b);

  vector<efloat_t> rho(1,1);

  int C = sample_two_nodes_multi(p,nodes,rho,false,false);

  if (C != -1) {
    P = p[C];
  }
}
