#include "substitution-index.H"
#include "util.H"

using std::valarray;
using std::vector;

/* The algorithms in this file provide each column in a sub-alignment
 * with a name that persists through alignment and tree changes that
 * do not make the column non-identifiable.  The name is a column
 * number, so that naming columns corresponds to ordering the columns
 * in the sub-alignment.   We need persistent names in order to cache
 * conditional likelihoods at columns in the sub-alignment,
 * specifically in order to cache across internal-node resampling.
 *
 * Our naming scheme for sub-alignment columns satisfies two
 * important properties: 
 * a) ordering depends only on pairwise alignments behind b and the
 *    topology of the subtree behind b
 * b) ordering depends only on the pairwise alignments PROJECTED TO
 *    THE LEAF SEQUENCES and does not change when the alignment of
 *    leaf sequences is unchanged. 
 *
 * a -> Each sub-alignment corresponds to a directed branch b on the
 * tree, and contains the leaf sequences BEHIND b. If alignments on
 * branches behind b do not change, then the sub-alignment b should
 * not change either.  This isn't automatic because the columns of the
 * sub-alignment may not be fully ordered.  Note that changing the
 * topology behind b would necessitate invalidating the cached
 * conditional likelihoods anyway, so we lose nothing by not
 * preserving column numbers in that case.
 * 
 * b-> This gives the useful property that re-sampling internal nodes
 * does not invalidate any of the names.  However, this implies that
 * the ordering of columns in a sub-alignment can conflict with
 * ordering in the full alignment, or with the order of sub-alignments
 * which include this sub-alignment. 
 */


namespace substitution {

  static inline bool any_present(const alignment& A,int c, const vector<int>& nodes) {
    for(int i=0;i<nodes.size();i++)
      if (not A.gap(c,nodes[i])) 
	return true;
    return false;
  }

  ublas::matrix<int> subA_index(const vector<int>& branches, const alignment& A,const Tree& T)
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),branches.size());

    // copy sub-A indices for each branch
    for(int j=0;j<branches.size();j++) {
      if (branches[j] == -1)
	for(int c=0;c<A.length();c++)
	  subA(c,j) = -1;
      else {
	if (not subA_index_valid(A,branches[j]))
	  update_subA_index_branch(A,T,branches[j]);
	for(int c=0;c<A.length();c++)
	  subA(c,j) = A.note(1,c+1,branches[j]);
      }
    }

    return subA;
  }

  ublas::matrix<int> subA_index(int node,const alignment& A,const Tree& T) 
  {
    // compute node branches
    vector<int> b;
    for(const_in_edges_iterator i = T[node].branches_in();i;i++)
      b.push_back(*i);

    return subA_index(b,A,T);
  }

  ublas::matrix<int> subA_select(const ublas::matrix<int>& subA1) {
    const int I = subA1.size2()-1;

    // count the number of columns to keep
    int L=0;
    for(int c=0;c<subA1.size1();c++)
      if (subA1(c,I) != alphabet::gap) L++;

    ublas::matrix<int> subA2(L,I);

    for(int c=0;c<subA1.size1();c++) {
      int c2 = subA1(c,I);
      if (c2 == alphabet::gap) continue;

      for(int j=0;j<subA2.size2();j++)
	subA2(c2,j) = subA1(c,j);
    }

    return subA2;
  }

  ublas::matrix<int> subA_index_select(const vector<int>& b,const alignment& A,const Tree& T) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(b,A,T);

    // return processed indices
    return subA_select(subA);
  }


  ublas::matrix<int> subA_index_any(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& nodes) 
  {
    vector<int> b2 = b;
    b2.push_back(-1);

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(b2,A,T);

    // select and order the columns we want to keep
    const int I = b.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,I) = l++;
      else
	subA(c,I) = alphabet::gap;

    // return processed indices
    return subA_select(subA);
  }


  ublas::matrix<int> subA_index_any(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& nodes, const vector<int>& seq) 
  {
    vector<int> b2 = b;
    b2.push_back(-1);

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(b2,A,T);

    // select and order the columns we want to keep
    const int I = b.size();
    for(int c=0;c<subA.size1();c++) 
      subA(c,I) = alphabet::gap;

    for(int i=0;i<seq.size();i++) 
      subA(seq[i],I) = i;

#ifndef NDEBUG
    // check reqs...
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	assert(subA(c,I)!=alphabet::gap);
      else
	assert(subA(c,I)==alphabet::gap);
#endif

    return subA_select(subA);
  }


  ublas::matrix<int> subA_index_none(const vector<int>& b,const alignment& A,const Tree& T,
				     const vector<int>& nodes) 
  {
    vector<int> b2 = b;
    b2.push_back(-1);

    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index(b2,A,T);

    // select and order the columns we want to keep
    const int I = b.size();
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,I) = alphabet::gap;
      else
	subA(c,I) = l++;

    // return processed indices
    return subA_select(subA);
  }

  std::ostream& print_subA(std::ostream& o,const ublas::matrix<int>& I) 
  {
    o<<"["<<I.size1()<<","<<I.size2()<<"]\n";
    for(int j=0;j<I.size2();j++) 
      for(int i=0;i<I.size1();i++) {
	o<<I(i,j);
	if (i<I.size1()-1)
	  o<<"  ";
	else
	  o<<std::endl;
      }
    return o;
  }

  bool subA_identical(const ublas::matrix<int>& I1,const ublas::matrix<int>& I2) {
    bool error = false;
    if (I1.size1() != I2.size1()) error=true;
    if (I1.size2() != I2.size2()) error=true;
    
    if (not error) 
      for(int i=0;i<I1.size1() and not error;i++)
	for(int j=0;j<I1.size2() and not error;j++)
	  error = (I1(i,j) != I2(i,j));
    
    return not error;
  }

  void check_subA(const alignment& A1,const alignment& A2,const Tree& T) 
  {
    for(int b=T.n_leaves();b<2*T.n_branches();b++) 
    {
      if (not subA_index_valid(A1,b)) continue;
      if (not subA_index_valid(A2,b)) continue;

      // compute branches-in
      vector<int> branches;
      for(const_in_edges_iterator e = T.directed_branch(b).branches_before();e;e++)
	branches.push_back(*e);
      assert(branches.size() == 2);
      
      vector<int> b2 = branches;
      b2.push_back(b);
	
      ublas::matrix<int> I1 = substitution::subA_index_select(b2,A1,T);
      ublas::matrix<int> I2 = substitution::subA_index_select(b2,A2,T);

      if (not subA_identical(I1,I2)) 
      {
	// print subAs alignments
	print_subA(std::cerr,I1)<<std::endl;
	std::cerr<<std::endl;
	print_subA(std::cerr,I2)<<std::endl;

	// print leaf sets in each subA
	for(int k=0;k<branches.size();k++) {
	  std::cerr<<"leaf set #"<<k+1<<" = ";
	  int lb = T.directed_branch(branches[k]).reverse();
	    vector<int> leaves = T.leaf_partition_set(lb);
	    for(int l=0;l<leaves.size();l++)
	      std::cerr<<leaves[l]<<" ";
	    std::cerr<<std::endl;
	}
	
	// print alignments
	std::cerr<<A1<<std::endl;
	std::cerr<<A2<<std::endl;

	// recompute subAs so we can enter w/ debugger
	I1 = substitution::subA_index_select(b2,A1,T);
	I2 = substitution::subA_index_select(b2,A2,T);

	std::abort();
      }
    }
  }
} // end namespace substitition

/// create a note with leaf sequences ...
int add_leaf_seq_note(alignment& A,int n) 
{
  assert(n < A.n_sequences());

  int index = A.add_note(n);

  for(int i=0;i<n;i++) {
    int l=0;
    for(int c=0;c<A.length();c++)
      if (not A.gap(c,i)) {
	A.note(index,l+1,i) = A(c,i);
	l++;
      }
    A.note(index,0,i) = l;
    assert(l == A.seqlength(i));
  }

  return index;
}

/// create a note with leaf sequences ...
int add_leaf_seq_note(alignment& A,const ublas::matrix<int>& M) 
{
  int n = M.size2();

  assert(n < A.n_sequences());

  int index = A.add_note(n);

  for(int i=0;i<n;i++) {
    const int l = M(0,i);
    assert(l == A.seq(i).size()/A.get_alphabet().width());
    for(int j=0;j<l;j++)
      A.note(index,j+1,i) = M(j+1,i);
    A.note(index,0,i) = l;
  }

  return index;
}

inline void invalidate_subA_index_one(const alignment& A,int b) {
  A.note(1,0,b) = -1;
}

void invalidate_subA_index_all(const alignment& A) 
{
  for(int i=0;i<A.note(1).size2();i++)
    invalidate_subA_index_one(A,i);
}

void invalidate_subA_index_directed_branch(const alignment& A,const Tree& T,int b) {
  vector<const_branchview> branches = branches_after(T,b);

  for(int i=0;i<branches.size();i++)
    invalidate_subA_index_one(A,branches[i]);
}


void invalidate_subA_index_branch(const alignment& A,const Tree& T,int b) {
  invalidate_subA_index_directed_branch(A,T,b);
  invalidate_subA_index_directed_branch(A,T,T.directed_branch(b).reverse());
}



/// create a note with leaf sequences ...
int add_subA_index_note(const alignment& A,int b) 

{
  int index = A.add_note(2*b);

  invalidate_subA_index_all(A);

  return index;
}

/// return index of lowest-numbered node behind b
int rank(const Tree& T,int b) {
  return T.leaf_partition_set(T.directed_branch(b).reverse())[0];
}


void update_subA_index_single(const alignment& A,const Tree& T,int b) {
  // notes for leaf sequences
  if (b < T.n_leaves()) {
    int l=0;
    for(int c=0;c<A.length();c++) {
      if (A.gap(c,b))
	A.note(1,c+1,b) = alphabet::gap;
      else
	A.note(1,c+1,b) = l++;
    }
    assert(l == leaf_seq_length(A,b));
    A.note(1,0,b) = l;
  }
  else {
    // get 2 branches leading into this one
    vector<const_branchview> prev;
    append(T.directed_branch(b).branches_before(),prev);
    assert(prev.size() == 2);

    // sort branches by rank
    if (rank(T,prev[0]) > rank(T,prev[1]))
      std::swap(prev[0],prev[1]);

    // get mappings of previous subA indices into alignment
    vector<vector<int> > mappings;
    for(int i=0;i<prev.size();i++) {
      assert(subA_index_valid(A,prev[i]));
      mappings.push_back(vector<int>(subA_length(A,prev[i]),-1));
    }

    int l=0;
    for(int c=0;c<A.length();c++) {
      bool present = false;
      for(int i=0;i<mappings.size();i++) {
	int index = A.note(1,c+1,prev[i]);
	assert(index < (int)mappings[i].size());

	if (index != -1) {
	  mappings[i][index] = c;
	  present = true;
	}
      }
      if (present) {
	l++;
	A.note(1,c+1,b) = -2;
      }
      else
	A.note(1,c+1,b) = -1;
    }

    // create subA index for this branch
    A.note(1,0,b) = l;
    l = 0;
    for(int i=0;i<mappings.size();i++) {
      for(int j=0;j<mappings[i].size();j++) {
	int c = mappings[i][j];

	// all the subA columns should map to an existing, unique columns of A
	assert(c != -1);

	// subA for b should be present here
	assert(A.note(1,c+1,b) != -1);

	if (A.note(1,c+1,b) == -2)
	  A.note(1,c+1,b) = l++;
      }
    }
    assert(l == A.note(1,0,b));
  }
}

void update_subA_index_branch(const alignment& A,const Tree& T,int b) 
{
#ifndef NDEBUG  
  subA_index_check_footprint(A,T);
#endif

  // get ordered list of branches to process before this one
  vector<const_branchview> branches; branches.reserve(T.n_branches());
  branches.push_back(T.directed_branch(b));
  
  for(int i=0;i<branches.size();i++) {
    const const_branchview& db = branches[i];
    if (not subA_index_valid(A,db))
      append(db.branches_before(),branches);
  }
  
  std::reverse(branches.begin(),branches.end());

  // update the branches in order 
  for(int i=0;i<branches.size();i++)
    update_subA_index_single(A,T,branches[i]);

#ifndef NDEBUG  
  subA_index_check_footprint(A,T);
  subA_index_check_regenerate(A,T);
#endif
}

void recompute_subA_notes(const alignment& A,const Tree& T) 
{
  invalidate_subA_index_all(A);
  vector<const_branchview> branches = branches_from_leaves(T);

  for(int i=0;i<branches.size();i++) {
    const const_branchview& b = branches[i];

    update_subA_index_single(A,T,b);
  }
}

void subA_index_check_regenerate(const alignment& A1,const Tree& T) 
{

  // compare against calculation from scratch
  alignment A2 = A1;
  recompute_subA_notes(A2,T);

  for(int b=0;b<T.n_branches()*2;b++) {
    if (subA_index_valid(A1,b)) {
      assert(subA_length(A1,b) == subA_length(A2,b));
      for(int c=0;c<A1.length();c++)
	assert(A1.note(1,c+1,b) == A1.note(1,c+1,b));
    }
  }
}

void subA_index_check_footprint(const alignment& A,const Tree& T) 
{
  for(int b=0;b<T.n_branches()*2;b++) {
    if (not subA_index_valid(A,b)) continue;

    for(int c=0;c<A.length();c++) {

      bool leaf_present = false;
      vector<int> leaves = T.leaf_partition_set(T.directed_branch(b).reverse());
      for(int i=0;i<leaves.size();i++)
	if (not A.gap(c,leaves[i]))
	  leaf_present=true;

      if (leaf_present)
	assert(A.note(1,c+1,b) != -1);
      else
	assert(A.note(1,c+1,b) == -1);
    }
  }
}


alignment blank_copy(const alignment& A1,int length) 
{
  alignment A2;

  // make an array w/ the same alphabet & sequences
  A2.a = A1.a;
  A2.sequences = A1.sequences;

  // make a blank array
  A2.array.resize(length, A1.array.size2());

  // make blank notes
  A2.notes.reserve(A1.notes.size());

  if (A1.notes.size() >= 1)
    add_leaf_seq_note(A2,A1.note(0));

  if (A1.notes.size() >= 2) {
    A2.add_note(A1.note(1).size2());
    invalidate_subA_index_all(A2);
  }

  return A2;
}


