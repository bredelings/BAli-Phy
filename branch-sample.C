#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "substitution.H"
#include "logsum.H"
#include "likelihood.H"
#include "choose.H"

//TODO - 1. calculate the probability of 
//  a) the path we came in with
//  b) the path we chose
//  c) the most probable path?

// 2. Calculate the likelihood of the reassembled matrix and the original matrix
//     - see if the difference is the same as the difference between the path probabilities

// 3. This won't work for the root node, though.

// g1 -> g2, never g2 -> g1
double path_P(const vector<int>& path,const Matrix& M,const Matrix& G1,const Matrix& G2,const Parameters& Theta) {
  const Matrix& P = Theta.IModel.P;
  const Matrix& R = Theta.IModel.R;
  
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
    double p=0;
    if (k<I)
      p = choose_P( state1,
		    M(k,l) + P(0,state2),
		    G1(k,l) + P(1,state2),
		    G2(k,l) + P(2,state2) );
    else
      p = choose_P( state1,
		    M(k,l) + R(0,state2),
		    G1(k,l) + R(1,state2),
		    G2(k,l) + R(2,state2) );
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
  assert(i==0);

  return Pr;
}

// What if the problem is primarily that the exact number of extensions varies slightly?
// Or the number of mismatches?  (so, we aren't creating new gaps)
// THEN, for every alignment, we would have maybe 

using std::valarray;


bool all_gaps(const alignment& A,int column,const valarray<bool>& mask) {
  for(int i=0;i<A.size2();i++)
    if (mask[i] and not A.gap(column,i))
      return false;
  return true;
}


// g1 -> g2, never g2 -> g1

vector<int> sample_path(const Matrix& M,const Matrix& G1,const Matrix& G2,const Parameters& Theta) {
  const Matrix& P = Theta.IModel.P;
  const Matrix& R = Theta.IModel.R;

  vector<int> path;
  const int I = M.size1()-1;
  const int J = M.size2()-1;
  int k = I;
  int l = J;

  int state2 = 3;
  while (k>0 or l>0) {
    int state1 = -1;
    if (k<I)
      state1 = choose( M(k,l) + P(0,state2),
		       G1(k,l) + P(1,state2),
		       G2(k,l) + P(2,state2) );
    else
      state1 = choose( M(k,l) + R(0,state2),
		       G1(k,l) + R(1,state2),
		       G2(k,l) + R(2,state2) );

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

  std::reverse(path.begin(),path.end());
  return path;
}

inline double sum(const valarray<double>& v) {
  return v.sum();
}


// This can affect the ordering!  Make sure this is OK
alignment construct(const alignment& old, const vector<int>& path, const valarray<bool>& group1, 
		    const vector<int>& seq1,const vector<int>& seq2) {

  valarray<bool> group2 = !group1;

  vector<int> subA1;
  vector<int> subA2;
  for(int column=0;column<old.length();column++) {
    if (not all_gaps(old,column,group1))
      subA1.push_back(column);
    if (not all_gaps(old,column,group2))
      subA2.push_back(column);
  }


  const int newlength = path.size() + (subA1.size()-seq1.size()) + (subA2.size() - seq2.size());
  alignment A = old;
  A.changelength(newlength);

  int c1=0,c2=0,c3=0,c4=0,l=0;
  for(int column=0;column<A.length();column++) {
    //    std::cout<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<endl;
    assert(c1>=c2);
    assert(c3>=c4);
    assert(c1 <= subA1.size());
    assert(c3 <= subA2.size());
    if (c1 < subA1.size() and (c2 == seq1.size() or (c2<seq1.size() and subA1[c1] < seq1[c2]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = old(subA1[c1],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;
    }
    else if (c3 < subA2.size() and (c4 == seq2.size() or (c4<seq2.size() and subA2[c3] < seq2[c4]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(subA2[c3],i);
      }
      c3++;
    }
    else if (path[l]==0) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = old(seq1[c2],i);
	else
	  A(column,i) = old(seq2[c4],i);
      }
      c1++;c2++;c3++;c4++;l++;
    }
    else if (path[l]==1) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(seq2[c4],i);
      }
      c3++;c4++;l++;
    }
    else {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = old(seq1[c2],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;c2++;l++;
    }
    //    std::cout<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<endl<<endl;
  }
  assert(c1 == subA1.size());
  assert(c2 == seq1.size());
  assert(c3 == subA2.size());
  assert(c4 == seq2.size());

  //  std::cerr<<"result=\n"<<A<<endl;
  for(int column=A.length()-1;column>=0;column--) {
    bool only_internal = true;
    for(int j=0;j<A.num_sequences();j++) 
      if (A(column,j) != alphabet::gap)
	only_internal = false;
    if (only_internal)
      A.delete_column(column);
  }

  return A;
}


vector<valarray<double> > distributions(const alignment& A,const Parameters& Theta,
					const vector<int>& seq,int b,bool up) {
  const alphabet& a = A.get_alphabet();

  vector< valarray<double> > dist(seq.size());

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    dist[i].resize(a.size());
    dist[i] = peel(residues,Theta,b,up);

    // double sum = dist[i].sum();
    // it IS possible to have no leaves if internal sequences is non-gap
    //    if (sum < a.size()-1) {
    //      assert(sum <= 1.00000001);
    //      dist[i] /= sum;
    //    }
  }

  return dist;
}

alignment sample_alignment(const alignment& old,const Parameters& Theta,int b) {
  const tree& T = Theta.T;
  const Matrix& P = Theta.IModel.P;
  const Matrix& R = Theta.IModel.R;
  const vector<double>& pi = Theta.IModel.pi;
  const valarray<double>& frequency = Theta.frequencies();

  int node1 = T.branch(b).parent();
  int node2 = T.branch(b).child();

  int old_length1 = old.seqlength(node1);

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
  vector< valarray<double> > dists1 = distributions(old,Theta,seq1,b,true);
  vector< valarray<double> > dists2 = distributions(old,Theta,seq2,b,false);

  valarray<double> g1_sub(seq2.size());
  for(int i=0;i<seq2.size();i++)  
    g1_sub[i] = log(sum( dists2[i] * frequency ));

  valarray<double> g2_sub(seq1.size());
  for(int i=0;i<seq1.size();i++)
    g2_sub[i] = log(sum( dists1[i] * frequency ));


  /********************* Create alignment matrices ***********************/
  Matrix M(seq1.size()+1,seq2.size()+1);
  Matrix G1(seq1.size()+1,seq2.size()+1);
  Matrix G2(seq1.size()+1,seq2.size()+1);

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++) {
      M(i,j)  = log_0;
      G1(i,j) = log_0;
      G2(i,j)  = log_0;
    }
  /***********    Initialize the Boundary    ************/
  M(0,0)  = pi[0];
  G1(0,0) = pi[1];
  G2(0,0) = pi[2];

  for(int i=1;i<M.size1();i++) {
    M(i,0) = log_0;
    G1(i,0) = log_0;
    G2(i,0) = g2_sub[i-1] + logsum(M (i-1,0) + P(0,2),
				   G1(i-1,0) + P(1,2),
				   G2(i-1,0) + P(2,2));
  }

  for(int i=1;i<M.size2();i++) {
    M(0,i) = log_0;
    if (i<seq1.size())
      G1(0,i) = g1_sub[i-1] + logsum(M (0,i-1) + P(0,1),
				     G1(0,i-1) + P(1,1),
				     G2(0,i-1) + P(2,1));
    else
       G1(0,i) = g1_sub[i-1] + logsum(M (0,i-1) + R(0,1),
 				     G1(0,i-1) + R(1,1),
				     G2(0,i-1) + R(2,1));
    G2(0,i) = log_0;
  }


  /******************* Compute the DP matrix **********************/
  const int maxlength = std::max(M.size1(),M.size2());
  for(int n=0; n<maxlength; n++) {
    if (n<M.size2())
      for(int i=1;i<n and i<M.size1();i++) {

	double sub = log(sum( dists1[i-1] * frequency * dists2[n-1] ));

	M(i,n) = sub + logsum(M(i-1,n-1)  + P(0,0),
			      G1(i-1,n-1) + P(1,0),
			      G2(i-1,n-1) + P(2,0));

	if (i<seq1.size())
	  G1(i,n) = g1_sub[n-1] + logsum(M (i,n-1) + P(0,1),
					 G1(i,n-1) + P(1,1),
					 G2(i,n-1) + P(2,1));
	else
	  G1(i,n) = g1_sub[n-1] + logsum(M (i,n-1) + R(0,1),
					 G1(i,n-1) + R(1,1),
					 G2(i,n-1) + R(2,1));

	G2(i,n) = g2_sub[i-1] + logsum(M (i-1,n) + P(0,2),
				       G1(i-1,n) + P(1,2),
				       G2(i-1,n) + P(1,2));
      }

    if (n<M.size1())
      for(int i=1;i<=n and i<M.size2();i++) {

	double sub = log(sum( dists1[n-1] * frequency * dists2[i-1] ));

	M(n,i)  = sub + logsum(M(n-1,i-1)  + P(0,0),
			       G1(n-1,i-1) + P(1,0),
			       G2(n-1,i-1) + P(2,0));

	if (n<seq1.size()) 
	  G1(n,i) = g1_sub[i-1] + logsum(M (n,i-1) + P(0,1),
					 G1(n,i-1) + P(1,1),
					 G2(n,i-1) + P(2,1));
	else
	  G1(n,i) = g1_sub[i-1] + logsum(M (n,i-1) + R(0,1),
					 G1(n,i-1) + R(1,1),
					 G2(n,i-1) + R(2,1));
				       

	G2(n,i) = g2_sub[n-1] + logsum(M (n-1,i) + P(0,2),
				       G1(n-1,i) + P(1,2),
				       G2(n-1,i) + P(2,2));
      }
  }


  /************** Sample a path from the matrix ********************/

  vector<int> path = sample_path(M,G1,G2,Theta);


  alignment A = construct(old,path,group1,seq1,seq2);

  assert(valid(A));
  // we are sampling while fixing the length of seqeuence1
  assert(A.seqlength(node1) == old_length1);

  /*--------------------------------------------------------------*/

  vector<int> path1 = get_path(old,node1,node2);
  vector<int> path2 = get_path(A,node1,node2);
  path.push_back(3);
  assert(path2 == path);
  double p1 = path_P(path1,M,G1,G2,Theta);
  double p2 = path_P(path2,M,G1,G2,Theta);

  double l1 = probability3(old,Theta);
  double l2 = probability3(A,Theta);

  std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<"           L1 = "<<l1<<"     L2 = "<<l2<<"     L2 - L1 = "<<l2-l1<<std::endl<<std::endl;
  double diff = p2-p1-(l2-l1);
  std::cerr<<"diff = "<<diff<<std::endl;
  std::cerr<<"rdiff = "<<diff/(p2-p1)<<std::endl;

  /*--------------------------------------------------------------*/

  return A;
}


