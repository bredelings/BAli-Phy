#include <valarray>
#include <algorithm>
#include "myrandom.H"
#include "sample.H"
#include "substitution.H"
#include <iostream>

using std::valarray;

const double gap_init = exp(-16);
const double gap_extend = exp(-4);

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;

inline double logsum(double r, double q)
{
  if (fabs(r-q) > 308)
    {
      return ((r > q) ? r : q);
    }

  return (r + log(1 + exp(q - r)));
}


bool all_gaps(const alignment& A,int column,const valarray<bool>& mask) {
  for(int i=0;i<A.size2();i++)
    if (mask[i] && A(column,i) != alphabet::gap)
      return false;
  return true;
}

int num_gaps(int n) {
  int sum=0;
  for(int i=0;i<6;i++) 
    if (n & (1<<i)) sum++;
  return sum;
}

int num_shared(int a,int b) {
  return num_gaps(a&b);
}


int choose(double x, double y) {
  double sum = logsum(x,y);
  double r = log(myrandomf());
  if (sum + r < x)
    return 0;
  else
    return 1;
  assert(0);
}

int choose(vector<double>::const_iterator here,int size) {
  vector<double> sum(size);

  sum[0] = *here;here++;
  for(int i=1;i<sum.size();i++) {
    sum[i] = logsum(*here,sum[i-1]);
    here++;  
  }

  double r = log(myrandomf()) + sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;
  assert(0);
}

int choose(const vector<double>& P) {
  return choose(P.begin(),P.size());
}


int mask(int i) {
  if (i&(1<<3))
    return 6;
  else if (i&(1<<4))
    return 5;
  else
    return 3;
}


alignment sample(const alignment& old,const tree& T,int node) {
  assert(T[node].left);
  assert(T[node].right);

  int n1 = T[node].left->name;
  int n2 = T[node].right->name;
  int n3 = T[node].parent->name;

  if (n3 == T.num_nodes()-1) {
    if (node != T[n3].left->name)
      n3 = T[n3].left->name;
    else 
      n3 = T[n3].right->name;
  }

  alignment A = old;

  vector< vector<double> > P;
  vector< vector<int> > indels;
  for(int i=0;i<A.length()+1;i++) {
    P.push_back(vector<double>(74,log_0));
    indels.push_back(vector<int>(2,0));
  }

  for(int column=0;column<A.length();column++) {
    int num_present=0;
    if (A(column,n1) != alphabet::gap)
      num_present++;
    if (A(column,n2) != alphabet::gap)
      num_present++;
    if (A(column,n3) != alphabet::gap)
      num_present++;

    if (num_present == 0) 
      indels[column+1][1] = (1<<30); //never
    else if (num_present == 3) 
      indels[column+1][0] = (1<<30); //never
    else if (num_present == 2) { 
      if (A(column,n1) == alphabet::gap) // 4-1
	indels[column+1][1] = 1<<0;
      if (A(column,n2) == alphabet::gap) // 4-2
	indels[column+1][1] = 1<<1;
      if (A(column,n3) == alphabet::gap) // 4-3
	indels[column+1][1] = 1<<2;
      indels[column+1][0] = (1<<30); //never
    } 
    else if (num_present == 1) {
      if (A(column,n1) != alphabet::gap) {
	indels[column+1][0] = 1<<3;           // 1-4
	indels[column+1][1] = (1<<1)|(1<<2);  // 4-2 & 4-3
      }
      if (A(column,n2) != alphabet::gap) {
	indels[column+1][0] = 1<<4;           // 2-4
	indels[column+1][1] = (1<<0)|(1<<2);  // 4-1 & 4-3
      }
      if (A(column,n3) != alphabet::gap) {
	indels[column+1][0] = 1<<5;           // 3-4
	indels[column+1][1] = (1<<0)|(1<<1);  // 4-1 & 4-2
      }
    }

  }

  P[0][63] = 0; /// 111-111
  P[0][71] = 0; /// 111
  P[0][73] = 0; /// 1

  // If we store 00+10->0 and 10+11->1 then we halve calls to logsum
  for(int column=1;column<P.size();column++) {
    for(int state=0;state<2;state++) {
      int indel = indels[column][state];
      
      if (indel == (1<<30))           // P=0
	continue;

      for(int i=0;i<8;i++) {
	if (P[column-1][64+i] == log_0)
	  continue;

	const int prev_state = (i==7);
	
	int prev_indel = indels[column-1][prev_state];

	int target=(i<<3)+7; // i ->111
	if (!state) {
	  if (prev_state)
	    target = (i<<3)+(prev_indel & mask(indel));
	  else
	    target = (i<<3)+(i & mask(indel));
	}
	
	int gaps = num_gaps(indel);
	
	int extended = num_shared(prev_indel,indel);
	if (i != 7) {
	  int extended2 = num_shared(prev_indel|i,indel);
	  if (extended2>extended)
	    assert(state == 1);
	  extended = extended2;
	}

	int opened = gaps - extended;
	
	double p = log(gap_extend)*extended + log(gap_init)*opened;
	
	P[column][target] = logsum(P[column][target],p + P[column-1][64+i]);
      }
    }
    for(int i=0;i<8;i++) {
      for(int j=0;j<8;j++)
	P[column][64+i] = logsum(P[column][64+i],P[column][(j<<3)+i]);
    }

    for(int i=0;i<7;i++)
      P[column][72] = logsum(P[column][72],P[column][64+i]);

    P[column][73] = P[column][71];
  }

  vector<double> choices(8);
  int current = choose(P[P.size()-1].begin()+64,8);
  for(int i=P.size()-1;i>0;i--) {
    if (current==7) 
      A(i-1,node) = alphabet::not_gap;
    else
      A(i-1,node) = alphabet::gap;
      
    for(int j=0;j<8;j++)
      choices[j] = P[i][(j<<3)+current];
    int next = choose(choices);
    current = next;
  }

  assert(valid(A));
  return A;
}

int choose(double x, double y, double z) {
  int choice=2;
  double sum = logsum(z,logsum(x,y));
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
	corrected_m += log(gap_init);
	corrected_g1 += log(gap_extend);
	corrected_g2 += log(gap_init);
      }
      else if (last==2) {
	corrected_m += log(gap_init);
	corrected_g1 += log_0;
	corrected_g2 += log(gap_extend);
      }
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


double substitution(const alignment& A,const tree& T,const valarray<bool>& mask,
		    int column1, int column2) {
  vector<int> residues(T.leaves());

  assert(column1 != -1 or column2 != -1);

  bool actual_letters=false;
  for(int j=0;j<residues.size();j++) {
    if (mask[j])
      if (column1 == -1)
	residues[j] = alphabet::gap;
      else 
	residues[j] = A(column1,j);
    else
      if (column2 == -1)
	residues[j] = alphabet::gap;
      else 
	residues[j] = A(column2,j);

    if (residues[j] >= 0)
      actual_letters=true;
  }
  
  // What if all nodes are internal? (already summed over)
  if (!actual_letters)
    return 0.0;

  //OK we're constructing this thing rather a lot
  TreeFunc< valarray<double> > distribution(T);
  return substitution(A.get_alphabet(),residues,T,distribution);
}

alignment sample(const alignment& old,const tree& T,int node1,int node2) {
  const alphabet& a = old.get_alphabet();
  if (node2<node1) std::swap(node1,node2);

  //  std::cerr<<"-------------------------------------"<<endl;
  //  std::cerr<<"entering sample "<<node1<<":"<<node2<<endl;
  //  std::cerr<<"old="<<endl;
  //  std::cerr<<old<<endl;

  valarray<bool> group1 = T.partition(node2,node1);
  valarray<bool> group2 = !group1;

  // Find sub-alignments and sequences
  vector<int> subA1;
  vector<int> seq1;
  vector<int> subA2;
  vector<int> seq2;
  for(int column=0;column<old.length();column++) {
    if (old(column,node1) != alphabet::gap)
      seq1.push_back(column);
    if (old(column,node2) != alphabet::gap)
      seq2.push_back(column);
    if (!all_gaps(old,column,group1))
      subA1.push_back(column);
    if (!all_gaps(old,column,group2))
      subA2.push_back(column);
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
    g2(i,0) = log(gap_init) + (i-1)*log(gap_extend);
  }


  for(int i=1;i<m.size2();i++) {
    m(0,i) = log_0;
    g1(0,i) = log(gap_init) + (i-1)*log(gap_extend);
    g2(0,i) = log_0;
  }

  vector< valarray<double> > dists1(seq1.size());
  for(int i=0;i<dists1.size();i++) {
    vector<int> residues(T.leaves());
    for(int j=0;j<residues.size();j++)
      residues[j] = old(seq1[i],j);
    dists1[i].resize(a.size());
    dists1[i] = peel(a,residues,T,node2,node1);
  }

  vector< valarray<double> > dists2(seq2.size());
  for(int i=0;i<dists2.size();i++) {
    vector<int> residues(T.leaves());
    for(int j=0;j<residues.size();j++)
      residues[j] = old(seq2[i],j);
    dists2[i].resize(a.size());
    dists2[i] = peel(a,residues,T,node1,node2);
  }

  const int maxlength = std::max(m.size1(),m.size2());
  for(int n=0; n<maxlength; n++) {
    if (n<m.size2())
      for(int i=1;i<n && i<m.size1();i++) {

	double sub = substitution(old,T,group1,seq1[i-1],seq2[n-1]);
	double sub2 = log((dists1[i-1]*a.frequency*dists2[n-1]).sum());
	m(i,n) = sub + logsum(m(i-1,n-1),logsum(g1(i-1,n-1),g2(i-1,n-1)));

	std::cerr<<"sub: "<<sub<<"  "<<sub2<<endl;

	sub = substitution(old,T,group1,-1,seq2[n-1]);
	sub2 = log((dists2[n-1]*a.frequency).sum());
	g1(i,n) = sub + logsum(log(gap_extend)+g1(i,n-1),log(gap_init)+logsum(g2(i,n-1),m(i,n-1)));

	std::cerr<<"sub: "<<sub<<"  "<<sub2<<endl;

	sub = substitution(old,T,group1,seq1[i-1],-1);
	sub2 = log((dists1[i-1]*a.frequency).sum());
	g2(i,n) = sub + logsum(log(gap_extend)+g2(i-1,n),log(gap_init)+m(i-1,n));
	std::cerr<<"sub: "<<sub<<"  "<<sub2<<endl;

      }

    if (n<m.size1())
      for(int i=1;i<=n && i<m.size2();i++) {

	double sub = substitution(old,T,group1,seq1[n-1],seq2[i-1]);
	m(n,i) = sub + logsum(m(n-1,i-1),logsum(g1(n-1,i-1),g2(n-1,i-1)));

	sub = substitution(old,T,group1,-1,seq2[i-1]);
	g1(n,i) = sub + logsum(log(gap_extend)+g1(n,i-1),log(gap_init)+logsum(g2(n,i-1),m(n,i-1)));

	sub = substitution(old,T,group1,seq1[n-1],-1);
	g2(n,i) = sub + logsum(log(gap_extend)+g2(n-1,i),log(gap_init)+m(n-1,i));
	
      }
  }

  /************** Sample a path from the matrix ********************/

  vector<int> path = sample_path(m,g1,g2);

  //  for(int i=0;i<path.size();i++)
  //    std::cerr<<path[i];
  //  std::cerr<<endl;

  /*
  for(int i=0;i<m.size1()&& i<m.size2();i++)
    std::cerr<<i<<"  "<<m(i,i)<<"  "<<g1(i,i)<<"  "<<g2(i,i)<<"  "<<g1(i,i)-m(i,i)<<"   "<<g2(i,i)-m(i,i)<<endl;
  */

  /************ Recreate the alignment based on the path *************/

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


  //  std::cerr<<"result-final=\n"<<A<<endl;
  //  std::cerr<<"-------------------------------------"<<endl;
  assert(valid(A));
  return A;
}


alignment sample(const alignment& old,const tree& T) {
  alignment A;
  if (myrandomf() < 0.5) {
    int node1 = myrandom(0,T.num_nodes()-3);
    int node2 = T[node1].parent->name;
    if (!T[node2].parent) {
      node1 = T[node2].left->name;
      node2 = T[node2].right->name;
    }
    A = sample(old,T,node1,node2);
  }
  else {
    int node = myrandom(T.leaves(),T.num_nodes()-2);
    A = sample(old,T,node);
  }
  return A;
}

