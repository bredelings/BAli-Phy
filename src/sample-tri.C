#include <valarray>
#include <iostream>
#include <cmath>

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
#include <boost/shared_ptr.hpp>
#include "dp-matrix.H"

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::valarray;

using namespace A3;

// FIXME - resample the path multiple times - pick one on opposite side of the middle 

boost::shared_ptr<DPmatrixConstrained> tri_sample_alignment_base(data_partition& P,const vector<int>& nodes)
{
  const Tree& T = *P.T;
  alignment& A = *P.A;

  assert(P.has_IModel());

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
  vector<int> columns = getorder(A,nodes[0],nodes[1],nodes[2],nodes[3]);

#ifndef NDEBUG

  // getorder(project(A,...)...) is not the same as getorder(A,...) because columns that are
  // in both project(A,...) and A have different columns numbers in each alignment, and
  // project(A,...) is shorter.

  // However, the NUMBER of columns should be the same. 
  vector<int> columns2 = getorder(project(A,nodes[0],nodes[1],nodes[2],nodes[3]),0,1,2,3);
  assert(columns.size() == columns2.size());
#endif

  // Find sub-alignments and sequences
  vector<int> seq1; seq1.reserve(A.length());
  vector<int> seq2; seq2.reserve(A.length());
  vector<int> seq3; seq3.reserve(A.length());
  vector<int> seq23; seq23.reserve(A.length());
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
  if (not P.smodel_full_tree)
    distributions = distributions_star;

  vector< Matrix > dists1 = distributions(P,seq1,nodes[0],group1);
  vector< Matrix > dists23 = distributions(P,seq23,nodes[0],group2|group3);


  //-------------- Create alignment matrices ---------------//

  vector<int> branches(3);
  for(int i=0;i<3;i++)
    branches[i] = T.branch(nodes[0],nodes[i+1]);

  const Matrix Q = createQ(P.branch_HMMs, branches);
  vector<double> start_P = get_start_P(P.branch_HMMs,branches);

  // Actually create the Matrices & Chain
  boost::shared_ptr<DPmatrixConstrained> 
    Matrices(new DPmatrixConstrained(get_state_emit(), start_P, Q, P.beta[0],
				     P.SModel().distribution(), dists1, dists23, frequency)
	     );

  // Determine which states are allowed to match (,c2)
  for(int c2=0;c2<dists23.size()-1;c2++) {
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    Matrices->states(c2).reserve(Matrices->nstates());
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

  // if the constraints are currently met but cannot be met
  if (pins.size() == 1 and pins[0][0] == -1)
    ; //std::cerr<<"Constraints cannot be expressed in terms of DP matrix paths!"<<std::endl;
  else {
    Matrices->forward_constrained(pins);
    if (Matrices->Pr_sum_all_paths() <= 0.0) 
      std::cerr<<"Constraints give this choice probability 0"<<std::endl;
  }

  if (Matrices->Pr_sum_all_paths() <= 0.0) 
    return Matrices;

  vector<int> path_g = Matrices->sample_path();

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

#ifndef NDEBUG_DP
  check_alignment(A,T,"sample_tri_base:out");
#else
  Matrices->clear();
#endif

  P.LC.set_length(A.length());
  int b = T.branch(nodes[0],nodes[1]);
  P.LC.invalidate_branch_alignment(T, b);

  return Matrices;
}


int sample_tri_multi(vector<Parameters>& p,const vector< vector<int> >& nodes_,
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
#ifndef NDEBUG_DP
  const Parameters P0 = p[0];
#endif

  vector<vector<boost::shared_ptr<DPmatrixConstrained> > > Matrices(p.size());
  for(int i=0;i<p.size();i++) 
  {
    for(int j=0;j<p[i].n_data_partitions();j++) {
      Matrices[i].push_back( tri_sample_alignment_base(p[i][j],nodes[i]) );
      //    p[i][j].LC.invalidate_node(p[i].T,nodes[i][0]);
#ifndef NDEBUG
      p[i][j].likelihood();  // check the likelihood calculation
#endif
    }
  }

  //-------- Calculate corrections to path probabilities ---------//

  vector< vector<efloat_t> > OS(p.size());
  vector< vector<efloat_t> > OP(p.size());

  for(int i=0; i<p.size(); i++) 
  {
    if (do_OS)
      for(int j=0;j<p[i].n_data_partitions();j++) 
	OS[i].push_back( other_subst(p[i][j],nodes[i]));
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
    for(int j=0;j<p[i].n_data_partitions();j++) {
      Pr[i] *= Matrices[i][j]->Pr_sum_all_paths();
      Pr[i] *= pow(OS[i][j], p[i][j].beta[0]);
      Pr[i] *= OP[i][j];
    }
  }

  assert(Pr[0] > 0.0);

  int C = choose_MH(0,Pr);

  assert(Pr[C] > 0.0);

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  valarray<bool> ignore1A = not p[0].T->partition(nodes[0][0],nodes[0][1]);
  valarray<bool> ignore2A = not (p[0].T->partition(nodes[0][0],nodes[0][2]) or p[0].T->partition(nodes[0][0],nodes[0][3]) );
  valarray<bool> ignore1(p[0].T->n_nodes()); 
  valarray<bool> ignore2(p[0].T->n_nodes()); 
  for(int i=0;i<ignore1.size();i++) {
    ignore1[i] = ignore1A[i];
    ignore2[i] = ignore2A[i];
  }

  // Check that our constraints are met
  for(int i=0;i<p.size();i++) {
    for(int j=0;j<p[i].n_data_partitions();j++) {
      if (not(A_constant(*P0[j].A, *p[i][j].A, ignore1))) {
	std::cerr<<P0[j].A<<endl;
	std::cerr<<p[i][j].A<<endl;
	assert(A_constant(*P0[j].A, *p[i][j].A, ignore1));
      }
      assert(A_constant(*P0[j].A, *p[i][j].A, ignore2));
    }
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
  for(int i=0;i<p.size();i++) {

    // check whether this arrangement violates a constraint in any partition
    bool ok = true;
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (Matrices[i][j]->Pr_sum_all_paths() <= 0.0) 
	ok = false;
    if (not ok) {
      assert(i != 0 and i != p.size()-1);
      continue;
    }

    for(int j=0;j<p[i].n_data_partitions();j++) 
    {
      paths[i].push_back( get_path_3way(A3::project(*p[i][j].A, nodes[i]), 0,1,2,3) );
    
      OS[i][j] = other_subst(p[i][j],nodes[i]);
      OP[i][j] = other_prior(p[i][j],nodes[i]);

      efloat_t OP_i = OP[i][j] / A3::correction(p[i][j],nodes[i]);

      check_match_P(p[i][j], OS[i][j], OP_i, paths[i][j], *Matrices[i][j]);
    }
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<efloat_t> > PR(p.size());

  for(int i=0;i<p.size();i++) 
  {
    // check whether this arrangement violates a constraint in any partition
    bool ok = true;
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (Matrices[i][j]->Pr_sum_all_paths() <= 0.0) 
	ok = false;
    if (not ok) {
      assert(i != 0 and i != p.size()-1);
      continue;
    }

    efloat_t choice_ratio = 1;
    if (i<Pr.size())
      choice_ratio = choose_MH_P(0,i,Pr)/choose_MH_P(i,0,Pr);
    else
      choice_ratio = 1;

    //    sample_P(p[i], choice_ratio, rho[i], paths[i], Matrices[i]) );
    PR[i] = vector<efloat_t>(4,1);
    PR[i][0] = p[i].heated_probability();
    PR[i][2] = rho[i];
    PR[i][3] = choice_ratio;
    for(int j=0;j<p[i].n_data_partitions();j++) {
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
  efloat_t C2 = A3::correction(p[C],nodes[C]);
  if (myrandomf() > double(C1/C2))
    return -1;

  return C;
}



void tri_sample_alignment(Parameters& P,int node1,int node2) {

  vector<valarray<bool> > s1(P.n_data_partitions());
  for(int i=0;i<P.n_data_partitions();i++) 
  {
    s1[i].resize(P[i].alignment_constraint.size1());
    s1[i] = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
#ifndef NDEBUG
    check_alignment(*P[i].A, *P[i].T, "tri_sample_alignment:in");
#endif
  }

  //------------(Gibbs) sample from proposal distribution ------------------//
  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = get_nodes_branch_random(*P.T,node1,node2);

  vector<efloat_t> rho(1,1);

  int C = sample_tri_multi(p,nodes,rho,false,false);

  if (C != -1) {
    P = p[C];
  }

  for(int i=0;i<P.n_data_partitions();i++) 
  {
#ifndef NDEBUG
    check_alignment(*P[i].A, *P[i].T,"tri_sample_alignment:out");
#endif

    valarray<bool> s2 = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
    report_constraints(s1[i],s2);
  }
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probability of the other branch alignments is unaffected...

bool tri_sample_alignment_branch(Parameters& P,
				 int node1,int node2,int b,
				 double rho_,double length2)
{
  //----------- Generate the Different Matrices ---------//
  vector<Parameters> p(2,P);
  p[1].setlength(b,length2);

  vector< vector<int> > nodes (2, get_nodes_branch_random(*P.T,node1,node2) );

  vector<efloat_t> rho(2);
  rho[0] = 1;
  rho[1] = rho_;

  int C = sample_tri_multi(p,nodes,rho,false,false);

  if (C != -1) {
    P = p[C];
  }

  return (C > 0);
}


bool tri_sample_alignment_branch_model(Parameters& P,int node1,int node2)
{
  //----------- Generate the Different Matrices ---------//
  vector<Parameters> p(2,P);

  int b = P.T->branch(node1,node2);
  p[1].branch_HMM_type[b] = 1 - p[1].branch_HMM_type[b];
  p[1].recalc_imodel();

  vector< vector<int> > nodes (2, get_nodes_branch_random(*P.T, node1,node2) );

  vector<efloat_t> rho(2,1.0);

  int C = sample_tri_multi(p,nodes,rho,false,false);

  if (C != -1) {
    P = p[C];
  }

  return (C > 0);
}
