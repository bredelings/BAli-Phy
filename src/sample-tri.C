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


//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::valarray;

using namespace A3;

// FIXME - actually resample the path multiple times - pick one on
// opposite side of the middle 
DPmatrixConstrained tri_sample_alignment_base(alignment& A,const Parameters& P,const vector<int>& nodes) {
  letters_OK(A,"sample_tri_base:in");
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

  letters_OK(A,"sample_tri_base:1");

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

  letters_OK(A,"sample_tri_base:2");

  /*-------------- Create alignment matrices ---------------*/

  vector<int> branches;
  for(int i=1;i<nodes.size();i++)
    branches.push_back(T.branch(nodes[0],nodes[i]) );

  const eMatrix Q = createQ(P.branch_HMMs, branches);
  vector<efloat_t> start_P = get_start_P(P.branch_HMMs,branches);

  letters_OK(A,"sample_tri_base:2.5");

  // Actually create the Matrices & Chain
  DPmatrixConstrained Matrices(get_state_emit(), start_P, Q, P.Temp,
			       P.SModel().distribution(), dists1, dists23, frequency);

  letters_OK(A,"sample_tri_base:3");
  // Determine which states are allowed to match (,c2)
  for(int c2=0;c2<Matrices.size2();c2++) {
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    for(int i=0;i<Matrices.nstates();i++) {
      int S2 = Matrices.order(i);

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

  letters_OK(A,"sample_tri_base:out");

  for(int i=0;i<20;i++) {
    vector<DPmatrixConstrained> temp;
    temp.push_back(Matrices);
    letters_OK(A,"sample_tri_base:loop");
  }
  return Matrices;
}


///FIXME - make a generic routine (templates?)

bool sample_tri_multi(alignment& A,vector<Parameters>& p,vector< vector<int> >& nodes,bool do_OS,bool do_OP) {

  letters_OK(A,"sample_tri_multi:in");
  assert(p.size() == nodes.size());

  Parameters P_save = p[0];
  //----------- Generate the different states and Matrices ---------//

  vector<alignment> a(p.size(),A);

  vector< DPmatrixConstrained > Matrices;
  for(int i=0;i<p.size();i++) {
    letters_OK(a[i],"sample_tri_multi:before");
    DPmatrixConstrained temp = tri_sample_alignment_base(a[i],p[i],nodes[i]);
    letters_OK(a[i],"sample_tri_multi:after1"); 
    Matrices.push_back( temp );
    letters_OK(a[i],"sample_tri_multi:after2"); 
    int b = p[i].T.branch(nodes[i][0],nodes[i][1]);
#ifndef NDEBUG
    p[i].likelihood(a[i],p[i]);  // check the likelihood calculation
#endif
  }

  letters_OK(A,"sample_tri_multi:out");
  return true;
}



void tri_sample_alignment(alignment& A,Parameters& P,int node1,int node2) {

  /*------------(Gibbs) sample from proposal distribution ------------------*/

  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = get_nodes_branch_random(P.T,node1,node2);

  sample_tri_multi(A,p,nodes,false,false);
  P = p[0];
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probability of the other branch alignments is unaffected...

bool tri_sample_alignment_branch(alignment& A,Parameters& P,
				 int node1,int node2,int b,double length2)
{
  //----------- Generate the Different Matrices ---------//
  vector<Parameters> p(2,P);
  p[1].setlength(b,length2);

  vector< vector<int> > nodes (2, get_nodes_branch_random(P.T,node1,node2) );

  bool success = sample_tri_multi(A,p,nodes,false,false);
  P = p[0];

  return success;
}
