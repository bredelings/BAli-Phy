#include "substitution-index.H"
#include "util.H"

using std::valarray;
using std::vector;

/* The algorithms in this file provide a method of computing column numbers
 * for sub-alignments so that column numbers don't change unless it is necessary.
 * Each sub-alignment corresponds to a directed branch b on the tree, and contains
 * the leaf sequences BEHIND b. If alignments on branches behind b do not change,
 * then the sub-alignment b does not change either.
 *
 * As long as a sub-alignment does not change, it makes sense to preserve conditional
 * likelihoods at columns in the sub-alignment.  However, in order to do this, columns
 * names must be persistent.
 *
 * The first step (in subA_index_simple) is to ignore columns which do not have any
 * leaf characters behind b.  Columns which contain internal nodes behind b, but not
 * leaf nodes, have no letters and do not contribute to the likelihood, and so can be
 * ignored. 
 *
 * The second step is to sort the columns of the sub-alignment.  Each sub-alignment
 * that corresponds to an internal branch has child sub-alignments containing mutually
 * exclusive leaf sequences; the parent sub-alignment contains the union of the leaf 
 * sequences of the child sub-alignments.  If two adjacent columns are present in the 
 * parent sub-alignment, but present in only one of the child sub-alignments then the
 * ordering of the two columns is undefined.  The order of these two columns in the full
 * alignment might be fixed by alignment to a sequences not behind b.
 *
 * To solve this problem we sort the the columns in each sub-alignment to provide a stable
 * order.  However, the ordering must depend only on the leaf sequences in the
 * sub-alignment, and so can conflict with ordering in the full alignment, or with 
 * the order of sub-alignments which include this sub-alignment.  Choosing an ordering that
 * depends only on the leaf sequences gives the useful property that re-sampling internal
 * nodes does not invalidate any of the names.  
 *
 * Thus, our naming scheme for sub-alignment columns satisfies two important properties:
 * a) ordering depends only on pairwise alignments behind b and the topology of the subtree
 *    behind b
 * b) ordering depends only on the pairwise alignments PROJECTED TO THE LEAF SEQUENCES
 *    and does not change when the alignment of leaf sequences is unchanged.
 *
 * Note that changing the topology behind b would necessitate invalidating the cached 
 * conditional likelihoods anyway, so we lose nothing by not preserving column numbers.
 */


namespace substitution {

  ublas::matrix<int> leaf_index(int b,const alignment& A) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),1);
    int l=0;
    for(int c=0;c<A.length();c++) {
      if (not A.gap(c,b))
	subA(l++,0) = A(c,b);
    }

    assert(l == A.seqlength(b));

    //resize the matrix, and send it back...
    ublas::matrix<int> temp(l,1);
    for(int i=0;i<temp.size1();i++)
      temp(i,0) = subA(i,0);
    
    return temp;
  }

  static inline bool any_shared(const alignment& A,int c1,int c2,const vector<int>& leaves) {
    for(int i=0;i<leaves.size();i++)
      if (not A.gap(c1,leaves[i]) and not A.gap(c2,leaves[i]))
	return true;
    return false;
  }

  // the column with the first present leaf character wins
  static inline bool after(const alignment& A,int c1,int c2,const vector<int>& leaves) {
    for(int i=0;i<leaves.size();i++) {
      assert(A.gap(c1,leaves[i]) or A.gap(c2,leaves[i]));
      if (not A.gap(c1,leaves[i]))
	return true;
      else if (not A.gap(c2,leaves[i]))
	return false;
    }
    std::abort(); // each column must have at least one character to call this function
  }

  vector<int> sort_subtree_columns(const alignment& A,const vector<int>& columns,const vector<int>& leaves) 
  {
    // start out with the alignment order
    vector<int> order(columns.size());
    for(int i=0;i<order.size();i++)
      order[i] = i;
    
    // add one column at a time to the sorted section
    for(int i=1;i<columns.size();i++) {
      for(int j=i;j>1;j--)
	// if the previous column shares a residue, then stop moving back
	if (any_shared(A,columns[order[j-1]],columns[order[j]],leaves)) break;
	// if the previous column is less than us, then stop moving back
	else if (after(A,columns[order[j]],columns[order[j-1]],leaves)) break;
        // otherwise we have to move back
	else
	  std::swap(order[j-1],order[j]);
    }
    
    return order;
  }

  static vector<vector<int> > get_subtree_leaves(const vector<int>& b,const Tree& T) 
  {
    // mask for being in ANY of the sub-A's
    valarray<bool> all(false,T.n_nodes());

    // get criteria for being in a sub-A
    vector<vector<int> > leaves(b.size()+1);
    for(int i=0;i<b.size();i++) {
      leaves[i].reserve(T.n_leaves());
      valarray<bool> p = T.partition(T.directed_branch(b[i]).reverse());
      all = all or p;
      for(int j=0;j<T.n_leaves();j++)
	if (p[j]) leaves[i].push_back(j);
    }

    //get criteria for being in ANY of the sub-A's
    leaves.back().reserve(T.n_leaves());
    for(int j=0;j<T.n_leaves();j++)
      if (all[j]) leaves.back().push_back(j);

    return leaves;
  }

  static inline bool any_present(const alignment& A,int c, const vector<int>& nodes) {
    for(int i=0;i<nodes.size();i++)
      if (not A.gap(c,nodes[i])) 
	return true;
    return false;
  }

  /// increment and return the index if any nodes in 'mask' are present in column 'c'
  inline int inc(int& index,const vector<int>& mask, 
		 const alignment& A,int c) 
  {
    if (any_present(A,c,mask))
      return ++index;
    else
      return alphabet::gap;
  }

  static ublas::matrix<int> subA_index_simple(const alignment& A, 
					      const vector<vector<int> >& leaves) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA(A.length(),leaves.size());

    // declare the index for the nodes
    vector<int> index(leaves.size(),-1);

    // calculate the index of each sub-A in each column
    for(int c=0;c<A.length();c++) {
      // get the child subA indices
      for(int j=0;j<leaves.size();j++) 
	subA(c,j) = inc(index[j],leaves[j],A,c);
    }

    return subA;
  }

  /// find the columns in which the i-th subA is live
  vector<int> get_columns(const ublas::matrix<int>& subA,int i) {
    vector<int> columns;
    columns.reserve(subA.size1());
    for(int c=0;c<subA.size1();c++) 
      if (subA(c,i) != alphabet::gap)
	columns.push_back(c);

    return columns;
  }

  //FIXME - decrease memory allocation...
  // decrease number of passes?
  //  - order columns "on-line" as they are found?
  static ublas::matrix<int> subA_index_sort(const alignment& A, 
					    const vector<vector<int> >& leaves) 
  {
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_simple(A,leaves);
    
    vector<int> columns; columns.reserve(A.length());
    vector<int> mapping; mapping.reserve(A.length());
    for(int i=0;i<subA.size2();i++) {
      // get the mapping from the alignment order to the sorted order of the columns
      columns = get_columns(subA,i);
      mapping = sort_subtree_columns(A,columns,leaves[i]);
      mapping = invert(mapping);

      int l=0;
      for(int c=0;c<subA.size1();c++) 
	if (subA(c,i) != alphabet::gap)
	  subA(c,i) = mapping[l++];

      assert(l == columns.size());
    }

    return subA;
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

  ublas::matrix<int> subA_index(const vector<int>& b,const alignment& A,const Tree& T) 
  {
    // get ordered ist of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_sort(A,leaves);

    // return processed indices
    return subA_select(subA);
  }


  ublas::matrix<int> subA_index_any(const vector<int>& b,const alignment& A,const Tree& T,
				    const vector<int>& nodes) 
  {
    // get ordered list of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_sort(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size()-1;
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
    // get ordered list of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_sort(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size()-1;
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
    // get ordered list of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_sort(A,leaves);

    // select and order the columns we want to keep
    const int I = leaves.size()-1;
    int l=0;
    for(int c=0;c<subA.size1();c++)
      if (any_present(A,c,nodes))
	subA(c,I) = alphabet::gap;
      else
	subA(c,I) = l++;

    // return processed indices
    return subA_select(subA);
  }

  ublas::matrix<int> subA_index_columns(int root,const alignment& A,const Tree& T) 
  {
    // compute root branches
    vector<int> b;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      b.push_back(*i);

    // get ordered ist of leaves in each subtree
    vector<vector<int> > leaves = get_subtree_leaves(b,T);
      
    // the alignment of sub alignments
    ublas::matrix<int> subA = subA_index_sort(A,leaves);

    assert(subA.size1() == A.length());

    const int I = leaves.size()-1;
    for(int c=0;c<subA.size1();c++)
      subA(c,I) = c;

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
      // compute branches-in
      vector<int> branches;
      for(const_in_edges_iterator e = T.directed_branch(b).branches_before();e;e++)
	branches.push_back(*e);
      
      assert(branches.size() == 2);
	
      ublas::matrix<int> I1 = substitution::subA_index(branches,A1,T);
      ublas::matrix<int> I2 = substitution::subA_index(branches,A2,T);

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
	I1 = substitution::subA_index(branches,A1,T);
	I2 = substitution::subA_index(branches,A2,T);

	std::abort();
      }
    }
  }


}

