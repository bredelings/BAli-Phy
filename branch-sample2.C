#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "substitution.H"
#include "logsum.H"
#include "likelihood.H"
#include "choose.H"
#include "dpmatrix.H"
#include "2way.H"

//TODO - 1. calculate the probability of 
//  a) the path we came in with
//  b) the path we chose
//  c) the most probable path?

// 2. Calculate the likelihood of the reassembled matrix and the original matrix
//     - see if the difference is the same as the difference between the path probabilities

// SYMMETRY: Because we are only sampling from alignments with the same fixed length
// for both sequences, this process is symmetric

using std::abs;
using namespace A2;

static vector< vector<valarray<double> > > distributions(const alignment& A,const Parameters& Theta,
					const vector<int>& seq,int b,bool up) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = Theta.SModel();

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size());
      dist[i][r] = substitution::peel(residues,
				      Theta.T,
				      MRModel.BaseModel(),
				      Theta.transition_P(r),
				      b,up);
    }

    // double sum = dist[i].sum();
    // it IS possible to have no leaves if internal sequences is non-gap
    //    if (sum < a.size()-1) {
    //      assert(sum <= 1.00000001);
    //      dist[i] /= sum;
    //    }
  }

  return dist;
}

alignment sample_alignment2(const alignment& old,const Parameters& P,int b) {
  const tree& T = P.T;

  const vector<double>& pi = P.IModel.pi;

  const substitution::MultiRateModel& MRModel = P.SModel();
  const valarray<double>& frequency = MRModel.BaseModel().frequencies();

  int node1 = T.branch(b).parent();
  int node2 = T.branch(b).child();

  valarray<bool> group1 = T.partition(node2,node1);

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  for(int column=0;column<old.length();column++) {
    if (old(column,node1) != alphabet::gap)
      seq1.push_back(column);
    if (old(column,node2) != alphabet::gap)
      seq2.push_back(column);
  }


  /******** Precompute distributions at node2 from the 2 subtrees **********/
  vector< vector< valarray<double> > > dists1 = distributions(old,P,seq1,b,true);
  vector< vector< valarray<double> > > dists2 = distributions(old,P,seq2,b,false);

  vector<int> state_emit(4,0);
  state_emit[0] |= (1<<1)|(1<<0);
  state_emit[1] |= (1<<1);
  state_emit[2] |= (1<<0);
  state_emit[3] |= 0;

  vector<double> start_P = pi;
  start_P.erase(start_P.begin()+3);

  DPmatrixSimple Matrices(state_emit,start_P,P.IModel.Q,
			  P.SModel().distribution(),dists1,dists2,frequency);

  /*------------------ Compute the DP matrix ---------------------*/

  if (P.features & (1<<0)) {
    vector<int> path_old = get_path(old,node1,node2);
    Matrices.forward(path_old,P.constants[0]);
  }
  else {
    // Since we are using M(0,0) instead of S(0,0), we need this hack to get ---+(0,0)
    // We can only use non-silent states at (0,0) to simulate S
    Matrices.forward(0,0);
  
    Matrices.forward(0,0,seq1.size(),seq2.size());
  }

  /************** Sample a path from the matrix ********************/

  vector<int> path = Matrices.sample_path();
  path.erase(path.begin()+path.size()-1);

  alignment A = construct(old,path,group1,seq1,seq2);

  /*--------------------------------------------------------------*/
#ifndef NDEBUG
  vector<int> path_old = get_path(old,node1,node2);
  vector<int> path_new = get_path(A,node1,node2);

  path.push_back(3);
  assert(path_new == path);

  double ls1 = substitution::Pr(old,P);
  double ls2 = substitution::Pr(A  ,P);

  double lp1 = prior_branch(old,P.IModel,node1,node2);
  double lp2 = prior_branch(A  ,P.IModel,node1,node2);

  double diff = Matrices.check(path_old,path_new,lp1,ls1,lp2,ls2);
  if (abs(diff) > 1.0e-9) {
    std::cerr<<prior_HMM_nogiven(old,P) - lp1<<endl;
    std::cerr<<prior_HMM_nogiven(A  ,P) - lp2<<endl;

    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    assert(0);
  }

#endif
  /*--------------------------------------------------------------*/
  assert(valid(A));
  return A;
}


