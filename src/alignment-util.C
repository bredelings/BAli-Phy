#include "alignment-util.H"

using std::vector;
using std::valarray;

/// Construct a mapping of letters to columns for each leaf sequence
vector< vector<int> > column_lookup(const alignment& A,int nleaves) {
  if (nleaves == -1)
    nleaves = A.size2();

  vector< vector<int> > result;

  for(int i=0;i<nleaves;i++) {
    vector<int> columns;
    for(int column=0;column<A.length();column++) {
      if (not A.gap(column,i))
	columns.push_back(column);
    }
    result.push_back(columns);
  }

  return result;
}

/// Replace each letter with its position in its sequence
alignment M(const alignment& A1) {
  alignment A2 = A1;
  for(int i=0;i<A2.size2();i++) {
    int pos=0;
    for(int column=0;column<A2.length();column++) {
      if (not A2.gap(column,i)) {
	A2(column,i) = pos;
	pos++;
      }
    }

    assert(pos == A2.seqlength(i));

  }
  return A2;
}

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const alignment& A1, int column, int s1, int s2, const alignment& A2,
	     const vector< vector< int> >& column_indices) {
  if (A1.gap(column,s1) and A1.gap(column,s2))
    return true;

  // Turn this into a statement about what s1[column] matches
  if (A1.gap(column,s1))
    std::swap(s1,s2);

  // which column in A2 has the A1(column,s1)-th feature of s1 ?
  int column2 = column_indices[s1][ A1(column,s1) ];
  return (A2(column2,s2) == A1(column,s2));
}


bool A_constant(alignment A1, alignment A2, const valarray<bool>& ignore) {
  assert(A1.size2() == A2.size2());
  assert(ignore.size() == A1.size2());

  // convert to feature-number notation
  A1 = M(A1);
  A2 = M(A2);

  // lookup and cache the column each feature is in
  vector< vector< int> > column_indices = column_lookup(A2);

  //----- Check that the sequence lengths match ------//
  for(int i=0;i<A1.size2();i++) {
    if (ignore[i]) continue;

    if (A1.seqlength(i) != A2.seqlength(i))
      return false;
  }

  //----- Check that each homology in A1 is in A2 -----//
  for(int column=0; column<A1.length(); column++)
    for(int s1=0; s1 < A1.size2(); s1++) {
      if (ignore[s1]) continue;
      for(int s2=s1+1; s2 < A1.size2(); s2++) {
	if (ignore[s2]) continue;
	if (not A_match(A1,column,s1,s2,A2,column_indices))
	  return false;
      }
    }

  return true;
}

bool bit_set(const valarray<bool>& v) {
  for(int i=0;i<v.size();i++)
    if (v[i]) return true;
  return false;
}


/// Check that any two present nodes are connected by a path of present nodes
bool all_characters_connected(const Tree& T,valarray<bool> present,const vector<int>& _ignore) {
  assert(present.size() == T.n_nodes());

  //--------- set the ignored nodes to 'not present' -----------//
  valarray<bool> ignore(false,present.size());
  for(int i=0;i<_ignore.size();i++) {
    int n = _ignore[i];
    present[n] = false;
    ignore[n] = true;
  }

  //---------- for each internal node... -------------//
  for(int n1=T.n_leaves(); n1<T.n_nodes(); n1++) {

    if (present[n1] or ignore[n1]) continue;
      
    //------- if it is '-' and not ignored ... -------//
    vector<const_nodeview> neighbors;
    append(T[n1].neighbors(),neighbors);
    assert(neighbors.size() == 3);

    //---- check the three attatched subtrees ... ----//
    int total=0;
    for(int i=0;i<neighbors.size();i++) {
      valarray<bool> group = T.partition(n1,neighbors[i]);
      if (bit_set(present and group))
	total++;
    }

    //----- nodes should be present in only one. -----//
    if (total > 1)
      return false;
  }
  return true;
}

