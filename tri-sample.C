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
#include "dpmatrix.H"

// for peel
#include "substitution.H"

// for prior_HMM_nogiven
#include "likelihood.H"

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::valarray;

using namespace A3;

vector< vector<valarray<double> > > distributions_star(const alignment& A,const Parameters& P,
					const vector<int>& seq,int n0,const valarray<bool>& group) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = P.SModel();
  const SequenceTree& T = P.T;

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());

    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size(),1.0);

      for(int n=0;n<T.leaves();n++) {
	if (not group[n]) continue;

	int letter = A(seq[i],n);
	if (not a.letter(letter)) continue;

	const Matrix& Q = P.transition_P(r,n);

	// Pr(root=l) includes Pr(l->letter)
	for(int l=0;l<a.size();l++)
	  dist[i][r][l] *= Q(l,letter);

      }
    }
  }

  return dist;
}


// FIXME - convert this to just take a valarray<bool> and a root, and pass them directly to substitution.C
vector< vector<valarray<double> > > distributions_tree(const alignment& A,const Parameters& P,
					const vector<int>& seq,int n0,const valarray<bool>& group) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = P.SModel();

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size());
      dist[i][r] = substitution::peel(residues,
				      P.T,
				      MRModel.BaseModel(),
				      P.transition_P(r),
				      n0,group);
    }

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

typedef vector< vector< valarray<double> > > (*distributions_t)(const alignment&, const Parameters&,
							      const vector<int>&,int,const valarray<bool>&);

// FIXME - actually resample the path multiple times - pick one on
// opposite side of the middle 
alignment tri_sample_alignment(const alignment& old,const Parameters& P,
			       int node1,int node2)
{
  const tree& T = P.T;

  const vector<double>& pi = P.IModel().pi;
  const valarray<double>& frequency = P.SModel().BaseModel().frequencies();

  //  std::cerr<<"old = "<<old<<endl;


  /*---------------- Setup node names ------------------*/
  assert(node1 >= T.leaves());

  int n0 = node1;
  int n1 = T[n0].parent();
  int n2 = T[n0].left();
  int n3 = T[n0].right();

  // make sure n1 == node2
  if (node2 == n1)
    ; // good
  else if (node2 == n2)
    std::swap(n1,n2); // 
  else if (node2 == n3)
    std::swap(n1,n3);
  else
    std::abort();

  // randomize the order here
  if (myrandom(2) == 1)
    std::swap(n2,n3);

  /*------------- Compute sequence properties --------------*/
  valarray<bool> group1 = T.partition(n0,n1);
  valarray<bool> group2 = T.partition(n0,n2);
  valarray<bool> group3 = T.partition(n0,n3);

  vector<int> columns = getorder(old,n0,n1,n2,n3);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq23;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    if (not old.gap(column,n1))
      seq1.push_back(column);
    if (not old.gap(column,n2))
      seq2.push_back(column);
    if (not old.gap(column,n3))
      seq3.push_back(column);

    if (not old.gap(column,n2) or not old.gap(column,n3))
      seq23.push_back(column);
  }

  // Map columns with n2 or n3 to single index 'c'
  vector<int> jcol(seq23.size()+1);
  vector<int> kcol(seq23.size()+1);

  jcol[0] = 0;
  kcol[0] = 0;
  for(int c=1,j=0,k=0;c<seq23.size()+1;c++) {
    if (not old.gap(seq23[c-1],n2))
      j++;    
    if (not old.gap(seq23[c-1],n3))
      k++;
    jcol[c] = j;
    kcol[c] = k;
  }

  // Precompute distributions at n0
  distributions_t distributions = distributions_tree;
  if (not P.SModel().full_tree)
    distributions = distributions_star;

  vector< vector< valarray<double> > > dists1 = distributions(old,P,seq1,n0,group1);
  vector< vector< valarray<double> > > dists23 = distributions(old,P,seq23,n0,group2|group3);


  /*-------------- Create alignment matrices ---------------*/

  const Matrix Q = createQ(P.IModel());

  // Actually create the Matrices & Chain
  DPmatrixHMM Matrices(get_state_emit(),get_start_P(pi),Q,
		       P.SModel().distribution(),dists1,dists23,frequency);

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

  if (P.features & (1<<0)) {
    vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
    Matrices.forward(path_old,P.constants[0]);
  }
  else {
    // Since we are using M(0,0) instead of S(0,0), we need this hack to get ---+(0,0)
    // We can only use non-silent states at (0,0) to simulate S
    Matrices.forward(0,0);
  
    Matrices.forward(0,0,seq1.size(),seq23.size());
  }

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices.sample_path();
  vector<int> path = Matrices.ungeneralize(path_g);

  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);

#ifndef NDEBUG_DP
  //--------------- Check alignment construction ------------------//

  // get the paths through the 3way alignment, from the entire alignment
  vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
  vector<int> path_new = get_path_3way(project(A,n0,n1,n2,n3),0,1,2,3);

  vector<int> path_old2 = get_path_3way(old,n0,n1,n2,n3);
  vector<int> path_new2 = get_path_3way(A,n0,n1,n2,n3);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_g = Matrices.generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(A));

  //-------------- Check relative path probabilities --------------//
  double s1 = P.likelihood(old,P);
  double s2 = P.likelihood(A,P);

  double lp1 = prior_HMM_nogiven(old,P);
  double lp2 = prior_HMM_nogiven(A  ,P);

  double diff = Matrices.check(path_old,path_new,lp1,s1,lp2,s2);

  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,n0,n1,n2,n3)<<endl;
    std::cerr<<project(A,n0,n1,n2,n3)<<endl;

    std::abort();
  }
#endif

  /*---------------- Adjust for length of n0 changing --------------------*/
  int length_old = old.seqlength(n0);
  int length_new = A.seqlength(n0);

  double log_ratio = 2.0*(P.IModel().lengthp(length_new)-P.IModel().lengthp(length_old));
  if (myrandomf() < exp(log_ratio))
    return A;
  else
    return old;
}


/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probably of the other branch alignments is unaffected...
bool tri_sample_alignment_branch(alignment& old,Parameters& P1,
				 int node1,int node2,int b,double length2)
{
  const tree& T = P1.T;

  Parameters P2 = P1;
  P2.setlength(b,length2);

  //  std::cerr<<"old = "<<old<<endl;


  /*---------------- Setup node names ------------------*/
  assert(node1 >= T.leaves());

  int n0 = node1;
  int n1 = T[n0].parent();
  int n2 = T[n0].left();
  int n3 = T[n0].right();

  // make sure n1 == node2
  if (node2 == n1)
    ; // good
  else if (node2 == n2)
    std::swap(n1,n2); // 
  else if (node2 == n3)
    std::swap(n1,n3);
  else
    std::abort();

  // randomize the order here
  if (myrandom(2) == 1)
    std::swap(n2,n3);

  /*------------- Compute sequence properties --------------*/
  valarray<bool> group1 = T.partition(n0,n1);
  valarray<bool> group2 = T.partition(n0,n2);
  valarray<bool> group3 = T.partition(n0,n3);

  vector<int> columns = getorder(old,n0,n1,n2,n3);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq23;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    if (not old.gap(column,n1))
      seq1.push_back(column);
    if (not old.gap(column,n2))
      seq2.push_back(column);
    if (not old.gap(column,n3))
      seq3.push_back(column);

    if (not old.gap(column,n2) or not old.gap(column,n3))
      seq23.push_back(column);
  }

  // Map columns with n2 or n3 to single index 'c'
  vector<int> jcol(seq23.size()+1);
  vector<int> kcol(seq23.size()+1);

  jcol[0] = 0;
  kcol[0] = 0;
  for(int c=1,j=0,k=0;c<seq23.size()+1;c++) {
    if (not old.gap(seq23[c-1],n2))
      j++;    
    if (not old.gap(seq23[c-1],n3))
      k++;
    jcol[c] = j;
    kcol[c] = k;
  }

  // Precompute distributions at n0
  distributions_t distributions = distributions_tree;
  if (not P1.SModel().full_tree)
    distributions = distributions_star;

  vector< vector< valarray<double> > > dists1_A = distributions(old,P1,seq1,n0,group1);
  vector< vector< valarray<double> > > dists23_A = distributions(old,P1,seq23,n0,group2|group3);

  vector< vector< valarray<double> > > dists1_B = distributions(old,P2,seq1,n0,group1);
  vector< vector< valarray<double> > > dists23_B = distributions(old,P2,seq23,n0,group2|group3);


  /*-------------- Create alignment matrices ---------------*/

  const vector<double>& pi = P1.IModel().pi;
  const Matrix Q = createQ(P1.IModel());
  const valarray<double>& frequencies = P1.SModel().BaseModel().frequencies();

  // Actually create the Matrices & Chain
  DPmatrixHMM Matrices1(get_state_emit(),get_start_P(pi),Q,
			P1.SModel().distribution(),dists1_A,dists23_A,
			frequencies);

  DPmatrixHMM Matrices2(get_state_emit(),get_start_P(pi),Q,
			P2.SModel().distribution(),dists1_B,dists23_B,
			frequencies);

  // Determine state order - FIXME - make this part of dpmatrix.H (part of HMM)
  vector<int> state_order(nstates);
  for(int i=0;i<state_order.size();i++)
    state_order[i] = i;
  std::swap(state_order[7],state_order[nstates-1]);        // silent states must be last

  // Determine which states are allowed to match (,c2)
  for(int c2=0;c2<Matrices1.size2();c2++) {
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
      if (c2==0 or (j1 == j2 and k1 == k2) or (j1 == jcol[c2-1] and k1 == kcol[c2-1]) ) {
	Matrices1.states(c2).push_back(S2);
	Matrices2.states(c2).push_back(S2);
      }
      else
	; // this state not allowed here
    }
  }


  /*------------------ Compute the DP matrix ---------------------*/

  if (P1.features & (1<<0)) {
    vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
    Matrices1.forward(path_old,P1.constants[0]);

    Matrices2.forward(path_old,P2.constants[0]);
  }
  else {
    // Since we are using M(0,0) instead of S(0,0), we need this hack to get ---+(0,0)
    // We can only use non-silent states at (0,0) to simulate S
    Matrices1.forward(0,0);
    Matrices1.forward(0,0,seq1.size(),seq23.size());

    Matrices2.forward(0,0);
    Matrices2.forward(0,0,seq1.size(),seq23.size());
  }

  //--------------- Choose between P1 and P2 ---------------------//

  const bool gibbs = true;

  double Pr1 = Matrices1.Pr_sum_all_paths() + prior(P1);
  double Pr2 = Matrices2.Pr_sum_all_paths() + prior(P2);

  int choice = 0;
  if (gibbs)
    choice = choose(Pr1,Pr2);
  else if (myrandomf() < exp(Pr2 - Pr1))
    choice = 1;

  Parameters*  ChosenP = &P1;
  DPmatrixHMM* ChosenMatrices = &Matrices1;
  if (choice == 1) {
    ChosenMatrices = &Matrices2;
    ChosenP = &P2;
  }

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = ChosenMatrices->sample_path();
  vector<int> path = ChosenMatrices->ungeneralize(path_g);
  path_g = Matrices1.generalize(path);

  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);

  int length_old = old.seqlength(n0);
  int length_new = A.seqlength(n0);

#ifndef NDEBUG_DP
  //--------------- Check alignment construction ------------------//

  // get the paths through the 3way alignment, from the entire alignment
  vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
  vector<int> path_new = get_path_3way(project(A,n0,n1,n2,n3),0,1,2,3);

  vector<int> path_old2 = get_path_3way(old,n0,n1,n2,n3);
  vector<int> path_new2 = get_path_3way(A,n0,n1,n2,n3);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_old_g = Matrices1.generalize(path_old);
  vector<int> path_new_g = Matrices1.generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(A));

  //-------------- Check relative path probabilities --------------//
  double PrOld = P1.probability(old,P1) + 2.0*P1.IModel().lengthp(length_old);
  double PrNew = P1.probability(A,*ChosenP) + 2.0*P1.IModel().lengthp(length_new);

  double PrS1 = choose_P(0,Pr1,Pr2,log_0)+Matrices1.path_P(path_old_g)
    + Matrices1.generalize_P(path_old);

  std::cerr<<"PrS1 = "<<choose_P(0,Pr1,Pr2,log_0)<<" + "<<Matrices1.path_P(path_old_g)<<" + "<< Matrices1.generalize_P(path_old)<<endl;

  double PrS2 = choose_P(choice,Pr1,Pr2,log_0)+ChosenMatrices->path_P(path_new_g)
    + ChosenMatrices->generalize_P(path_new);

  std::cerr<<"PrS2 = "<<choose_P(choice,Pr1,Pr2,log_0)<<" + "<<ChosenMatrices->path_P(path_new_g)<<" + "<< ChosenMatrices->generalize_P(path_new)<<endl;

  double PrQ1 = Matrices1.path_Q(path_old_g) + 
    Matrices1.generalize_P(path_old)+ prior(P1);

  double PrQ2 = ChosenMatrices->path_Q(path_new_g) + 
    ChosenMatrices->generalize_P(path_new) + prior(*ChosenP);

  std::cerr<<"Gibbs = "<<gibbs<<endl;
  std::cerr<<" Pr1  = "<<PrOld<<"    Pr2  = "<<PrNew<<"    Pr2  - Pr1  = "<<PrNew - PrOld<<endl;
  std::cerr<<" PrQ1 = "<<PrQ1<<"    PrQ2 = "<<PrQ2<<"    PrQ2 - PrQ1 = "<<PrQ2 - PrQ1<<endl;
  std::cerr<<" PrS1 = "<<PrS1<<"    PrS2 = "<<PrS2<<"    PrS2 - PrS1 = "<<PrS2 - PrS1<<endl;


  double diff = (PrS2 - PrS1) - (PrNew - PrOld);
  std::cerr<<"diff = "<<diff<<endl;
  if (abs(diff) > 1.0e-9 and gibbs) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,n0,n1,n2,n3)<<endl;
    std::cerr<<project(A,n0,n1,n2,n3)<<endl;

    std::abort();
  }
#endif

  /*---------------- Adjust for length of n0 changing --------------------*/

  double log_ratio = 2.0*(P1.IModel().lengthp(length_new)-P1.IModel().lengthp(length_old));
  if (myrandomf() < exp(log_ratio)) {
    old=A;
    if (choice) {
      P1 = P2;
      return true;
    }
  }
  return false;
}
