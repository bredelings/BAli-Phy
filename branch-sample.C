#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "2way.H"

// for peel()
#include "substitution.H"

// SYMMETRY: Because we are only sampling from alignments with the same fixed length
// for both sequences, this process is symmetric

using std::abs;
using namespace A2;

inline double sum(const valarray<double>& v) {
  return v.sum();
}


double path_Q_path(const vector<int>& path,const Matrix& Q,const vector<double>& pi) {
  double Pr = log_0;
  for(int S=0;S<3;S++)
    Pr = logsum(Pr,pi[S] + Q(S,path[0]));

  for(int l=1;l<path.size();l++)
    Pr += Q(path[l-1],path[l]);

  return Pr;
}

bool di(int state) {
  return (state == 0 or state==2);
}

bool dj(int state) {
  return (state == 0 or state==1);
}

double path_Q_subst(const vector<int>& path,
		    const vector< vector< valarray<double> > >& dists1,
		    const vector< vector< valarray<double> > >& dists2,
		    const substitution::MultiRateModel& MRModel)
{
  const valarray<double>& frequency = MRModel.BaseModel().frequencies();

  double P_sub=0;
  int i=0,j=0;
  const int I = dists1.size();
  const int J = dists2.size();

  for(int l=0;l<path.size();l++) {

    int state2 = path[l];
    if (di(state2))
      i++;
    if (dj(state2))
      j++;

    double sub=0;
    if (di(state2) and dj(state2)) {
      for(int r=0;r<MRModel.nrates();r++) {
	// double temp = sum( dists1[i-1][r] * frequency * dists2[j-1][r] );  HANDCODED
	double temp=0;
	const valarray<double>& v1 = dists1[i-1][r];
	const valarray<double>& v2 = dists2[j-1][r];
	for(int l=0;l<frequency.size();l++)
	  temp += v1[l]*frequency[l]*v2[l];
 	sub += MRModel.distribution()[r]*temp;
      }
    }
    else if (di(state2)) {
      for(int r=0;r<MRModel.nrates();r++) {
	double temp = MRModel.distribution()[r]* 
	  sum( dists1[i-1][r] * frequency);
	sub += temp;
      }
    }
    else if (dj(state2)) {
      for(int r=0;r<MRModel.nrates();r++) {
	double temp = MRModel.distribution()[r]* 
	  sum( dists2[j-1][r] * frequency);
	sub += temp;
      }
    }
    else
      sub = 1.0;

    P_sub += log(sub);
  }
  assert(i == I and j == J);
  return P_sub;
}




double path_P(const vector<int>& path,const Matrix& M,const Matrix& G1,const Matrix& G2,const Parameters& P) {
  const Matrix& Q = P.IModel().Q;

  const int I = M.size1()-1;
  const int J = M.size2()-1;
  int k = I;
  int l = J;
  double Pr=0;

  int state2 = 3;
  int i = path.size()-1; 
  while(k>0 or l>0) {
    assert(i>0);
    int state1 = path[i-1];
    double p = choose_P( state1,
			 M(k,l) + Q(0,state2),
			 G1(k,l) + Q(1,state2),
			 G2(k,l) + Q(2,state2) );

    if (state1==0)
      {k--;l--;}
    if (state1==1)
      l--;
    if (state1==2)
      k--;

    i--;
    state2 = state1;
    Pr += p;

    assert(k>=0 and l>=0);
  }
  assert(i == 0);
  assert(k == 0 and l == 0);
  return Pr;
}

// What if the problem is primarily that the exact number of extensions varies slightly?
// Or the number of mismatches?  (so, we aren't creating new gaps)
// THEN, for every alignment, we would have maybe 

using std::valarray;


// g1 -> g2, never g2 -> g1

vector<int> sample_path(const Matrix& M,const Matrix& G1,const Matrix& G2,const Parameters& P) {
  const Matrix& Q = P.IModel().Q;

  vector<int> path;
  const int I = M.size1()-1;
  const int J = M.size2()-1;
  int k = I;
  int l = J;

  int state2 = 3;
  while (k>0 or l>0) {
    int state1 = choose( M(k,l) + Q(0,state2),
			 G1(k,l) + Q(1,state2),
			 G2(k,l) + Q(2,state2) );

    if (state1==0)
      {k--;l--;}
    if (state1==1)
      l--;
    if (state1==2)
      k--;

    path.push_back(state1);
    state2 = state1;

    assert(k>=0 and l>=0);
  }
  assert(k == 0 and l == 0);

  std::reverse(path.begin(),path.end());
  return path;
}

// This function is defined in the NEW version of branch-sample (branch-sample2.C)
vector< vector<valarray<double> > > distributions_tree(const alignment& A,const Parameters& P,
						  const vector<int>& seq,int b,bool up);

// This function is defined in the NEW version of branch-sample (branch-sample2.C)
vector< vector<valarray<double> > > distributions_star(const alignment& A,const Parameters& P,
						  const vector<int>& seq,int b,bool up);

typedef vector< vector< valarray<double> > > (*distributions_t)(const alignment&, const Parameters&,
							      const vector<int>&,int,bool);

alignment sample_alignment(const alignment& old,const Parameters& P,int b) {
  const tree& T = P.T;
  const Matrix& Q = P.IModel().Q;

  const vector<double>& pi = P.IModel().pi;

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
  distributions_t distributions = distributions_tree;
  if (not P.SModel().full_tree)
    distributions = distributions_star;

  vector< vector< valarray<double> > > dists1 = distributions(old,P,seq1,b,true);
  vector< vector< valarray<double> > > dists2 = distributions(old,P,seq2,b,false);

  valarray<double> g1_sub(seq2.size());
  for(int i=0;i<seq2.size();i++) {
    double total=0;
    for(int r=0;r<MRModel.nrates();r++)
      total += MRModel.distribution()[r]*sum( dists2[i][r] * frequency );
    g1_sub[i] = log(total);
  }

  valarray<double> g2_sub(seq1.size());
  for(int i=0;i<seq1.size();i++) {
    double total=0;
    for(int r=0;r<MRModel.nrates();r++)
      total += MRModel.distribution()[r]*sum( dists1[i][r] * frequency );
    g2_sub[i] = log(total);
  }


  /********************* Create alignment matrices ***********************/
  Matrix M(seq1.size()+1,seq2.size()+1);
  Matrix G1(seq1.size()+1,seq2.size()+1);
  Matrix G2(seq1.size()+1,seq2.size()+1);

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++) {
      M(i,j)  = log_0;
      G1(i,j) = log_0;
      G2(i,j) = log_0;
    }
  /***********    Initialize the Boundary    ************/
  M(0,0)  = pi[0];
  G1(0,0) = pi[1];
  G2(0,0) = pi[2];

  

  for(int i=1;i<M.size1();i++) {
    M(i,0) = log_0;
    G1(i,0) = log_0;
    G2(i,0) = g2_sub[i-1] + logsum(M (i-1,0) + Q(0,2),
				   G1(i-1,0) + Q(1,2),
				   G2(i-1,0) + Q(2,2));
  }

  for(int i=1;i<M.size2();i++) {
    M(0,i) = log_0;
    G1(0,i) = g1_sub[i-1] + logsum(M (0,i-1) + Q(0,1),
				   G1(0,i-1) + Q(1,1),
				   G2(0,i-1) + Q(2,1));
    G2(0,i) = log_0;
  }


  /******************* Compute the DP matrix **********************/
  const int maxlength = std::max(M.size1(),M.size2());
  for(int n=0; n<maxlength; n++) {
    if (n<M.size2())
      for(int i=1;i<n and i<M.size1();i++) {

	double sub = 0;
	for(int r=0;r<MRModel.nrates();r++) {
	  // sub += MRModel.distribution()[r]*
	  //        sum( dists1[i-1][r] * frequency * dists2[n-1][r] );  HANDCODED
	  double temp=0;
	  const valarray<double>& v1 = dists1[i-1][r];
	  const valarray<double>& v2 = dists2[n-1][r];
	  for(int l=0;l<frequency.size();l++)
	    temp += v1[l]*frequency[l]*v2[l];
	  sub += MRModel.distribution()[r]*temp;
	}


	M(i,n) = log(sub) + logsum(M(i-1,n-1)  + Q(0,0),
				   G1(i-1,n-1) + Q(1,0),
				   G2(i-1,n-1) + Q(2,0));

	G1(i,n) = g1_sub[n-1] + logsum(M (i,n-1) + Q(0,1),
				       G1(i,n-1) + Q(1,1),
				       G2(i,n-1) + Q(2,1));

	G2(i,n) = g2_sub[i-1] + logsum(M (i-1,n) + Q(0,2),
				       G1(i-1,n) + Q(1,2),
				       G2(i-1,n) + Q(2,2));
      }

    if (n<M.size1())
      for(int i=1;i<=n and i<M.size2();i++) {

	double sub = 0;
	for(int r=0;r<MRModel.nrates();r++) {
	  // sub += MRModel.distribution()[r]*
	  //        sum( dists1[n-1][r] * frequency * dists2[i-1][r] );  HANDCODED
	  double temp=0;
	  const valarray<double>& v1 = dists1[n-1][r];
	  const valarray<double>& v2 = dists2[i-1][r];
	  for(int l=0;l<frequency.size();l++)
	    temp += v1[l]*frequency[l]*v2[l];
	  sub += MRModel.distribution()[r]*temp;
	}

	M(n,i)  = log(sub) + logsum(M(n-1,i-1)  + Q(0,0),
				    G1(n-1,i-1) + Q(1,0),
				    G2(n-1,i-1) + Q(2,0));


	G1(n,i) = g1_sub[i-1] + logsum(M (n,i-1) + Q(0,1),
				       G1(n,i-1) + Q(1,1),
				       G2(n,i-1) + Q(2,1));

	G2(n,i) = g2_sub[n-1] + logsum(M (n-1,i) + Q(0,2),
				       G1(n-1,i) + Q(1,2),
				       G2(n-1,i) + Q(2,2));
      }
  }


  /************** Sample a path from the matrix ********************/

  vector<int> path = sample_path(M,G1,G2,P);

  alignment A = construct(old,path,group1,seq1,seq2);
  /*--------------------------------------------------------------*/
#ifndef NDEBUG_DP
  vector<int> path1 = get_path(old,node1,node2);
  vector<int> path2 = get_path(A,node1,node2);
  path.push_back(3);
  assert(path2 == path);

  double ls1 = P.likelihood(old,P);
  double ls2 = P.likelihood(A  ,P);

  double lp1 = P.prior(old,P);
  double lp2 = P.prior(A,P);

  double p1 = path_P(path1,M,G1,G2,P);
  double p2 = path_P(path2,M,G1,G2,P);

  double l1 = P.probability(old,P);
  double l2 = P.probability(A,P);

  double l1B = ls1 + lp1;
  double l2B = ls2 + lp2;

  double diff = p2-p1-(l2-l1);
  double rdiff = diff/(l2-l1);

  double diffB = p2-p1-(l2B-l1B);
  double rdiffB = diffB/(l2B-l1B);

  double qp1 = path_Q_path(path1,Q,pi);
  double qs1 = path_Q_subst(path1,dists1,dists2,MRModel);
  double q1 = qp1 + qs1;

  double qp2 = path_Q_path(path2,Q,pi);
  double qs2 = path_Q_subst(path2,dists1,dists2,MRModel);
  double q2 = qp2 + qs2;

  if (path1 != path2) {
    std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<endl;
    std::cerr<<"Q1 = "<<q1<<"     Q2 = "<<q2<<"     Q2 - Q1 = "<<q2-q1<<endl;
    std::cerr<<"L1 = "<<l1<<"     L2 = "<<l2<<"     L2 - L1 = "<<l2-l1<<endl;
    std::cerr<<"L1B = "<<l1B<<"     L2B = "<<l2B<<"     L2B - L1B = "<<l2B-l1B<<endl<<endl;
    std::cerr<<"diff = "<<diff<<std::endl;
    std::cerr<<"rdiff = "<<rdiff<<std::endl;
    std::cerr<<"rdiffB = "<<rdiffB<<std::endl;

    // Do the likelihood and HMM substitition probabilities agree?
    std::cerr<<"LS1 = "<<ls1<<"     LS2 = "<<ls2<<"   LS2 - LS1 = "<<ls2-ls1<<endl;
    std::cerr<<"QS1 = "<<qs1<<"     QS2 = "<<qs2<<"   QS2 - QS1 = "<<qs2-qs1<<endl;
    std::cerr<<endl;

    // Do the likelihood and HMM path probabilities agree?
    std::cerr<<"LP1 = "<<lp1<<"     LP2 = "<<lp2<<endl;
    std::cerr<<"QP1 = "<<qp1<<"     QP2 = "<<qp2<<endl;
    std::cerr<<endl;



    if (diff != 0.0) {
      if (std::abs(rdiff) > 1.0e-8) {
	old.print(std::cerr);
	A.print(std::cerr);
      }
    }
  }

  assert(isnan(rdiff) or abs(diff) < 1.0e-8);
#endif
  /*--------------------------------------------------------------*/
  assert(valid(A));
  return A;
}


