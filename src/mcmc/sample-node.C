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
/// \file sample-node.C
///
/// \brief Contains routines for resampling the sequence at an internal node (3way).
///

#include <iostream>
#include <cmath>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"
#include "util.H"
#include "rng.H"
#include "dp/3way.H"
#include "dp/alignment-sums.H"
#include "alignment-util.H"
#include "alignment-constraint.H"
#include "likelihood.H"    // for prior()
#include "substitution-index.H"
#include <boost/shared_ptr.hpp>
#include "dp/dp-array.H"
#include "timer_stack.H"

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
using std::vector;
using std::endl;

using boost::dynamic_bitset;

using namespace A3;

boost::shared_ptr<DParrayConstrained> sample_node_base(data_partition& P,const vector<int>& nodes)
{
  default_timer_stack.push_timer("alignment::DP1/3-way");
  const Tree& T = P.T();

  assert(P.variable_alignment());

  alignment old = *P.A;

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
  vector<bitmask_t> state_emit(nstates+1);
  for(int S2=0;S2<state_emit.size();S2++)
    if (di(S2) or dj(S2) or dk(S2)) 
      state_emit[S2] |= (1<<0);

  vector<int> branches;
  for(int i=1;i<nodes.size();i++)
    branches.push_back(T.branch(nodes[0],nodes[i]) );

  mhmm m1 = P.get_branch_HMM(T.branch(nodes[1],nodes[0]));
  m1.remap_bits({1,0});
  mhmm m2 = P.get_branch_HMM(T.branch(nodes[2],nodes[0]));
  m2.remap_bits({0,2});
  mhmm m3 = P.get_branch_HMM(T.branch(nodes[3],nodes[0]));
  m3.remap_bits({0,3});

  mhmm m123 = Glue(m1,Glue(m2,m3));

  const Matrix Q = createQ( P.get_branch_HMMs(branches) );
  vector<double> start_P = get_start_P( P.get_branch_HMMs(branches) );

  // Actually create the Matrices & Chain
  boost::shared_ptr<DParrayConstrained> 
    Matrices( new DParrayConstrained(seq123.size(),state_emit,start_P,Q, P.get_beta())
	      );

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices->size();c2++) {
    int i2 = icol[c2];
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    Matrices->states(c2).reserve(Matrices->nstates());
    for(int i=0;i<Matrices->nstates();i++) {
      int S2 = Matrices->order(i);

      //---------- Get (i1,j1,k1) ----------
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
	{ } // this state not allowed here
    }
  }


  //------------------ Compute the DP matrix ----------------------//
  Matrices->forward();

  //------------- Sample a path from the matrix -------------------//

  // If the DP matrix ended up having probability 0, don't try to sample a path through it!
  if (Matrices->Pr_sum_all_paths() <= 0.0)
  {
    std::cerr<<"sample_node_base( ): All paths have probability 0!"<<std::endl;
    default_timer_stack.pop_timer();
    return Matrices;
  }

  vector<int> path_g = Matrices->sample_path();
  vector<int> path = Matrices->ungeneralize(path_g);

  *P.A.modify() = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);
  for(int i=1;i<4;i++) {
    int b = T.directed_branch(nodes[0],nodes[i]);
    P.set_pairwise_alignment(b, A3::get_pairwise_alignment_from_path(path, 0, i));
  }

#ifndef NDEBUG
  vector<int> path_new = get_path_3way(project(*P.A,n0,n1,n2,n3),0,1,2,3);
  vector<int> path_new2 = get_path_3way(*P.A,n0,n1,n2,n3);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_g = Matrices->generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(*P.A));


#endif

  default_timer_stack.pop_timer();
  return Matrices;
}

int sample_node_multi(vector<Parameters>& p,const vector< vector<int> >& nodes_,
		      const vector<efloat_t>& rho_, bool do_OS,bool do_OP) 
{
  vector<vector<int> > nodes = nodes_;
  vector<efloat_t> rho = rho_; 
  assert(p.size() == nodes.size());
 
  //------------ Check the alignment branch constraints ------------//
  for(int i=0;i<p.size();i++) {
    vector<int> branches;
    branches.push_back(p[i].T->branch(nodes[i][0],nodes[i][1]));
    branches.push_back(p[i].T->branch(nodes[i][0],nodes[i][2]));
    branches.push_back(p[i].T->branch(nodes[i][0],nodes[i][3]));

    if (any_branches_constrained(branches, *p[i].T, *p[i].TC, p[i].AC))
      return -1;
  }

  //----------- Generate the different states and Matrices ---------//
  efloat_t C1 = A3::correction(p[0],nodes[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
  const Parameters P0 = p[0];
#endif

  vector< vector< boost::shared_ptr<DParrayConstrained> > > Matrices(p.size());
  for(int i=0;i<p.size();i++) {
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
	Matrices[i].push_back( sample_node_base(p[i][j],nodes[i]) );
      else
	Matrices[i].push_back( boost::shared_ptr<DParrayConstrained>() );
  }

  //-------- Calculate corrections to path probabilities ---------//

  vector< vector<efloat_t> > OS(p.size());
  vector< vector<efloat_t> > OP(p.size());

  for(int i=0; i<p.size(); i++) 
  {
    if (do_OS)
      for(int j=0;j<p[i].n_data_partitions();j++)
	if (p[i][j].variable_alignment())
	  OS[i].push_back( p[i][j].likelihood() );
	else
	  OS[i].push_back( 1 );
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

  assert(Pr[0] > 0.0);

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

  assert(Pr[C] > 0.0);

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  dynamic_bitset<> ignore(p[0].T->n_nodes());
  ignore[ nodes[0][0] ] = true;

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

  vector< vector< vector<int> > > paths(p.size());

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	paths[i].push_back( get_path_3way(A3::project(*p[i][j].A,nodes[i]),0,1,2,3) );
	
	OS[i][j] = p[i][j].likelihood();
	OP[i][j] = other_prior(p[i][j],nodes[i]);
	
	efloat_t OP_i = OP[i][j] / A3::correction(p[i][j],nodes[i]);
	
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
    
    
    //    sample_P(p[i], choice_ratio, rho[i], paths[i], Matrices[i]) );
    //    PR[i][j][0] *= A3::correction(p[i][j],nodes[i]);
    PR[i] = vector<efloat_t>(4,1);
    PR[i][0] = p[i].heated_probability();
    PR[i][2] = rho[i];
    PR[i][3] = choice_ratio;
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
	PR[i][0] *= A3::correction(p[i][j],nodes[i]);
	PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
      }
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR);
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we reject the move, then don't do anything
  //FIXME - PARTITION - compute and cache P0 part before changing p[0], then we can
  //                     throw P0 away if we want to.
  efloat_t C2 = A3::correction(p[C],nodes[C]);
  if (myrandomf() > double(C1/C2))
    return -1;

  return C;
}





void sample_node(Parameters& P,int node) 
{
  const Tree& T = *P.T;

  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = get_nodes_random(T,node);

  vector<efloat_t> rho(1,1);

  int C = sample_node_multi(p,nodes,rho,false,false);

  if (C != -1) {
    P = p[C];
  }
}
