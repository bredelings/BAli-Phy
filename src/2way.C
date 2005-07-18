#include "2way.H"
#include "alignment-util.H"
#include "substitution-index.H"

using namespace std;

namespace A2 {

alignment construct(const alignment& old, const vector<int>& path, int n1,int n2,
		    const Tree& T, const vector<int>& seq1,const vector<int>& seq2) {

  valarray<bool> group1 = T.partition(n2,n1);
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
  alignment A = blank_copy(old,newlength);
  assert(A.length() == newlength);

  //  std::clog<<"old = "<<old<<"\n";

  int c1=0,c2=0,c3=0,c4=0,l=0;
  for(int column=0;column<A.length();column++) {
    //    std::clog<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<"  "<<l<<"\n";

    assert(c1>=c2);
    assert(c3>=c4);
    assert(c1 <= subA1.size());
    assert(c3 <= subA2.size());

    if (c1 < subA1.size() and (c2 == seq1.size() or (c2<seq1.size() and subA1[c1] < seq1[c2]))) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A(column,i) = old(subA1[c1],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;
      assert(not all_gaps(A,column));
    }
    else if (c3 < subA2.size() and (c4 == seq2.size() or (c4<seq2.size() and subA2[c3] < seq2[c4]))) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(subA2[c3],i);
      }
      c3++;
      assert(not all_gaps(A,column));
    }
    else if (path[l]==0) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A(column,i) = old(seq1[c2],i);
	else
	  A(column,i) = old(seq2[c4],i);
      }
      c1++;c2++;c3++;c4++;l++;
      assert(not all_gaps(A,column));
    }
    else if (path[l]==1) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(seq2[c4],i);
      }
      c3++;c4++;l++;
      assert(not all_gaps(A,column));
    }
    else {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A(column,i) = old(seq1[c2],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;c2++;l++;
      assert(not all_gaps(A,column));
    }
    //    std::clog<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<"  "<<l<<"\n";;
    assert(not all_gaps(A,column));
  }

  assert(c1 == subA1.size());
  assert(c2 == seq1.size());
  assert(c3 == subA2.size());
  assert(c4 == seq2.size());
  assert(l == path.size());

  for(int i=0;i<A.n_sequences();i++) 
    assert(A.seqlength(i) == old.seqlength(i));

  //  std::cerr<<"new = "<<A<<endl;  
  assert(valid(A));

  invalidate_subA_index_all(A);

  return A;
}

}
