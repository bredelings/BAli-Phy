#include <valarray>
#include <iostream>
#include <cmath>
#include "gaps.H"
#include "sample.H"
#include "myrandom.H"
#include "substitution.H"


using std::valarray;

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;

inline double logsum(double r, double q)
{
  if (fabs(r-q) > 308)
    return ((r > q) ? r : q);
  
  return (r + log(1 + exp(q - r)));
}


bool all_gaps(const alignment& A,int column,const valarray<bool>& mask) {
  for(int i=0;i<A.size2();i++)
    if (mask[i] && A(column,i) != alphabet::gap)
      return false;
  return true;
}


int choose(double x, double y, double z) {
  int choice=2;
  double sum = logsum(z,logsum(x,y));
  //  double x_ = exp(x-sum), y_ = exp(y-sum),z_ = exp(z-sum);
  //  std::cerr<<x_<<"  "<<y_<<"  "<<z_;
  double r = log(myrandomf());
  if (sum + r < x)
    choice=0;
  else if (sum + r < logsum(x,y))
    choice=1;

  //  std::cerr<<" "<<choice<<endl;
  return choice;
}


// g2 -> g1, never g1 -> g2

vector<int> sample_path(const Matrix& m,const Matrix& g1,const Matrix& g2) {
  vector<int> path;
  int k = m.size1() - 1;
  int l = m.size2() - 1;

  int last =0;
  while (k>0 || l>0) {
    int move=-1;
    if (k==0)
      move = 1;
    else if (l==0)
      move = 2;
    else {
      double corrected_m = m(k,l);
      double corrected_g1 = g1(k,l);
      double corrected_g2 = g2(k,l);
      if (last == 1) {
	corrected_m += lambda_O;
	corrected_g1 += lambda_E;
	corrected_g2 += lambda_O;
      }
      else if (last==2) {
	corrected_m += lambda_O;
	corrected_g1 += log_0;
	corrected_g2 += lambda_E;
      }
      //      std::cerr<<"path: "<<k<<", "<<l<<endl;
      move = choose(corrected_m, corrected_g1, corrected_g2) ;
    }

    if (move==0)
      {k--;l--;}
    if (move==1)
      l--;
    if (move==2)
      k--;

    path.push_back(move);
    last = move;
  }

  std::reverse(path.begin(),path.end());
  return path;
}

inline double sum(const valarray<double>& v) {
  return v.sum();
}


alignment construct(const alignment& old, const vector<int>& path, const valarray<bool>& group1, 
		    const vector<int>& seq1,const vector<int>& seq2) {

  valarray<bool> group2 = !group1;

  vector<int> subA1;
  vector<int> subA2;
  for(int column=0;column<old.length();column++) {
    if (!all_gaps(old,column,group1))
      subA1.push_back(column);
    if (!all_gaps(old,column,group2))
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
    if (c1 < subA1.size() && (c2 == seq1.size() || (c2<seq1.size() && subA1[c1] < seq1[c2]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = old(subA1[c1],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;
    }
    else if (c3 < subA2.size() && (c4 == seq2.size() || (c4<seq2.size() && subA2[c3] < seq2[c4]))) {
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


vector< valarray<double> > distributions(const alignment& A,const tree& T,const vector<int>& seq,int node1,int node2) {
  const alphabet& a = A.get_alphabet();
  vector< valarray<double> > dist(seq.size());

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(T.leaves());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    dist[i].resize(a.size());
    dist[i] = peel(a,residues,T,node1,node2);

    double sum = dist[i].sum();
    // it IS possible to have no leaves if internal sequences is non-gap
    //    if (sum < a.size()-1) {
    //      assert(sum <= 1.00000001);
    //      dist[i] /= sum;
    //    }
  }

  return dist;
}

alignment sample(const alignment& old,const tree& T,int node1,int node2) {
  const alphabet& a = old.get_alphabet();
  if (node2<node1) std::swap(node1,node2);

  //  std::cerr<<"-------------------------------------"<<endl;
  //  std::cerr<<"entering sample "<<node1<<":"<<node2<<endl;
  //  std::cerr<<"old="<<endl;
  //  std::cerr<<old<<endl;

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

  /********************* Create alignment matrices ***********************/
  Matrix m(seq1.size()+1,seq2.size()+1);
  Matrix g1(seq1.size()+1,seq2.size()+1);
  Matrix g2(seq1.size()+1,seq2.size()+1);

  for(int i=0;i<m.size1();i++)
    for(int j=0;j<m.size2();j++) {
      m(i,j)  = log_0;
      g1(i,j) = log_0;
      g2(i,j)  = log_0;
    }

  m(0,0) = 0;
  g1(0,0) = log_0;
  g2(0,0) = log_0;

  for(int i=1;i<m.size1();i++) {
    m(i,0) = log_0;
    g1(i,0) = log_0;
    g2(i,0) = lambda_O + (i-1)*lambda_E;
  }

  for(int i=1;i<m.size2();i++) {
    m(0,i) = log_0;
    g1(0,i) = lambda_O + (i-1)*lambda_E;
    g2(0,i) = log_0;
  }


  /******** Precompute distributions at node2 from the 2 subtrees **********/
  vector< valarray<double> > dists1 = distributions(old,T,seq1,node2,node1);
  vector< valarray<double> > dists2 = distributions(old,T,seq2,node1,node2);

  vector<double> sum1(dists1.size());
  for(int i=0;i<dists1.size();i++)
    sum1[i] = log(dists1[i].sum());

  vector<double> sum2(dists2.size());
  for(int i=0;i<dists2.size();i++)
    sum2[i] = log(dists2[i].sum());

  valarray<double> g1_sub(seq2.size());
  for(int i=0;i<seq2.size();i++)
    g1_sub[i] = log(sum( dists2[i] * a.frequency )) - sum2[i];

  valarray<double> g2_sub(seq1.size());
  for(int i=0;i<seq1.size();i++)
    g2_sub[i] = log(sum( dists1[i] * a.frequency )) - sum1[i];



  /******************* Compute the DP matrix **********************/
  valarray<double> temp(a.size());

  const int maxlength = std::max(m.size1(),m.size2());
  for(int n=0; n<maxlength; n++) {
    if (n<m.size2())
      for(int i=1;i<n && i<m.size1();i++) {

	double sub = log(sum( dists1[i-1] * a.frequency * dists2[n-1] )) - sum1[i-1]-sum2[n-1];
	m(i,n) = sub + logsum(m(i-1,n-1),logsum(g1(i-1,n-1),g2(i-1,n-1)));

	g1(i,n) = g1_sub[n-1] + logsum(lambda_E + g1(i,n-1),lambda_O + logsum(g2(i,n-1),m(i,n-1)));

	g2(i,n) = g2_sub[i-1] + logsum(lambda_E + g2(i-1,n),lambda_O + m(i-1,n));
      }

    if (n<m.size1())
      for(int i=1;i<=n && i<m.size2();i++) {

	double sub = log(sum( dists1[n-1]*a.frequency*dists2[i-1] )) - sum1[n-1] - sum2[i-1];
	m(n,i) = sub + logsum(m(n-1,i-1),logsum(g1(n-1,i-1),g2(n-1,i-1)));

	g1(n,i) = g1_sub[i-1] + logsum(lambda_E + g1(n,i-1),lambda_O + logsum(g2(n,i-1),m(n,i-1)));

	g2(n,i) = g2_sub[n-1] + logsum(lambda_E + g2(n-1,i),lambda_O + m(n-1,i));
	
      }
  }

  for(int i=1;i<sum1.size();i++)
    sum1[i] += sum1[i-1];

  for(int i=1;i<sum2.size();i++)
    sum2[i] += sum2[i-1];

  Matrix sums(seq1.size()+1,seq2.size()+1);
  for(int i=0;i<sums.size1();i++) {
    for(int j=0;j<sums.size2();j++) {
      if (i==0 || j=0)
	;
      else {
	sums(i,j) = sum1[i-1] + sum2[j-1];
      }
    }
  }



  /************** Sample a path from the matrix ********************/

  vector<int> path = sample_path(m,g1,g2);

  //  for(int i=0;i<path.size();i++)
  //    std::cerr<<path[i];
  //  std::cerr<<endl;

  //  for(int i=0;i<m.size1()&& i<m.size2();i++)
  //    std::cerr<<i<<"  "<<m(i,i)<<"  "<<g1(i,i)<<"  "<<g2(i,i)<<"  "<<g1(i,i)-m(i,i)<<"   "<<g2(i,i)-m(i,i)<<endl;

  alignment A = construct(old,path,group1,seq1,seq2);

  assert(valid(A));
  return A;
}


