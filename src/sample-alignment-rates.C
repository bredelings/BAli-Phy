#include "sample.H"

#include "parameters.H"


/*

We should base everything on the graph:

1. To build the graph:

For each column c
  For each branch b that is active in c
    o find the previous active column p for branch b
    o if state(c,b) == I and state(p,b) != I then
        open a new insertion section w/ PREV=p, BRANCH=b, and starting with p->c
    o if state(c,b) == I and state(p,b) == I then
        extend the insertion of the previous column with p->c
    o if state(c,b) != I and state(p,b) == I then
        end the insertion of column p with NEXT=c, and closing with p->c
  Assert(there is only one branch in this column that has an insertion.)
ALSO: Pretend that -1 is the Start column, and L is the End column.
    o So, if there is no prev active column, then -1 is the prev active column,
      and the value is S
    o And, after all the normal columns, we consider a column L in which the
      state for each branch is E.

2. compute the joint sum over the various states in the graph:

For each insertion (PREV->..->NEXT) in order of decreasing branch depth
  assert(PREV->NEXT occurs in a higher-order insertion, 
         unless BRANCH==-1 and PREV=start and NEXT=end)
  assert(only one ACTIVE predecessor)
    o i.e. ???
  For each position c from the 2nd position to the 2nd-to-last position of the insertion
    Consider the previous site @p
      Compute F[PREV][c][NEXT] as the product of 
  

How about 1->2->3, where the insertion is deleted and another sequence is inserted?

1 +----+
2 +++--+
3 +--+++
  123456

When does the first insertion end?  I think it ends in column 5, AS DOES THE SECOND ONE.

Therefore, when we accumulate the final A[1][6] probability conditional on 1 and 6, 
we may need some way of keeping track of things PER INSERTION?

1  P(S->M_1)*P(S->M_1)*T(S,1)
2  * P(M_1->I_2,6)*P(M_1->D_2)
3  * P(I_2,6->I_3,6) * P(D_2->D_3)
4  * P(D_3->I_4,6)
5  * P(I_4,6->I_5,6)
6  * P(I_5,6->M_6)*T(1,6)
E  * P(M_6->E)*P(M_6->E)*T(6,E)

1  P(S->M_1)*P(S->M_1)*T(S,1)
sum[2,3]_1,6
 2  * P(M_1->I_2,6)*P(M_1->D_2)
 3  * P(I_2,6->I_3,6) * P(D_2->D_3)
 sum[4,5]_3,6
  4  * P(D_3->I_4,6)
  5  * P(I_4,6->I_5,6)
6  * P(I_5,6->M_6)*T(1,6)
E  * P(M_6->E)*P(M_6->E)*T(6,E)

Q: Perhaps we need E columns for EVERY insertion, as well as the entire sequence!
A1: Actually, we don't.  Insertions on the end like this can be handled separately.
A2: Err.... can they?  Because we have a transition from D_3 -> I_4,6

 */


// Despite the fact that I allocated very little time to actually design
//  the algorithm for this, attempts to wrap my head around the algorithm
//  that I think I almost already know are proving rather tricky.  So
//  this is something that is worth accomplishing and stuff, when I have to write
//  about how this is a substantial accomplishment, later.

// If the columns are are i,j,k,l then we process i->j, j->k, k->l,
// accumulating things indexed by [i,j],[i,k],[i,l]

using ublas::matrix;

struct insertion 
{
  /// On which branch does this insertion happen?
  int b;

  /// Which columns of the alignment are in this insertion? (first and last column are PREV and NEXT)
  vector<int> columns;

  /// What column is before the insertion, whose rate must condition on?
  int prev() const {return columns[0];}

  /// What column is after the insertion, whose rate must condition on?
  int next() const {return columns.back();}

  insertion(int b_)
    :b(b_)
  { }
  
  insertion(int b_, const vector<int>& c)
    :b(b_), columns(c)
  { }
};

struct insertion_graph
{
  vector<int> insertion_for_column;

  vector<insertion> insertions;

  vector< vector<int> > insertions_ending_in_column;

  insertion_graph(const alignment& A, const Tree& T, int root);
};




/** Create the graph.

    Each insertion is a list of columns, where the first and last are conditioned on.

    The -1 column is the "start" column, and the A.length() column is the "end" column.
**/

insertion_graph::insertion_graph(const alignment& A, const Tree& T,int root)
  :insertion_for_column(A.length()+1),
   insertions_ending_in_column(A.length()+1)
{
  // Which insertion (if any) is active for each branch?
  vector<int> active_insertions(T.n_branches(), -1);

  // What is the previous active column for this branch?
  vector<int> prev_column(T.n_branches(),-1);

  // List of correctly oriented branches
  vector<const_branchview> branches = branches_from_node(T,root);

  // FIXME - what about the DEFAULT insertion? (on "branch -1")

  // For each column ...
  for(int c=0; c<=A.length(); c++)
  {
    // which branch has an insertion (if any) in this column?
    insertion_for_column[c] = -1;

    for(int j=0;j<branches.size();j++)
    {
      int n1 = branches[j].source();
      int n2 = branches[j].target();
      int b =  branches[j].undirected_name();

      int p  = prev_column[b];

      // Skip column==c/branch==j if branch j is not active in column c
      if (c != A.length() and not A.character(c,n1) and not A.character(c,n2)) continue;

      // Does column==c/branch==j have an insertion?
      bool ins = (c != A.length() and not A.character(c,n1) and A.character(c,n2));

      if (ins) 
      {
	// There can be at most one insertion in each column;
	assert(insertion_for_column[c] == -1);

	// Record which branch the insertion happens on, for this column.
	insertion_for_column[c] = b;

	// Initialize a new insertion, and record its PREV column
	if (active_insertions[b] == -1) 
	{
	  // create a new insertion
	  insertions.push_back(insertion(b));

	  // add its PREV column
	  insertions.back().columns.push_back(prev_column[b]);

	  // record an active insertion occuring on branch b
	  active_insertions[b] = insertions.size()-1;
	}

	// record the index of the insertion that is active in this column
	insertion_for_column[c] = active_insertions[b];

	// extend an existing insertion
	insertions[insertion_for_column[c]].columns.push_back(c);
      }
      else {
	// close off an active insertion in branch b with its end (post) column
	if (active_insertions[b] != -1)
	{
	  // the previous column should be an insertion column if the insertions are active.
	  assert(p == -1 or (not A.character(p,n1) and A.character(p,n2)));

	  // add the closing column
	  insertions[active_insertions[b]].columns.push_back(c);
	  insertions_ending_in_column[c].push_back(active_insertions[b]);

	  // record the fact that there are no active insertions for this branch
	  active_insertions[b] = -1;
	}
      }

      prev_column[b] = c;
    }
  }
}

// PROBLEM! Each insertion then needs to know which sub-insertions fit inside it.
//          e.g., between column i and i+1 we have not only the transition 
//          probability Q(i,i+1), but also the 


struct DP_insertion: public insertion
{
  /// The number of rates
  int R;

  /// Forward[p][n](c,S) = Pr(state S in column c| given PREV=p and NEXT=n)
  vector< vector< matrix<double> > > Forward;
  
  /// Allocate memory for the Dynamic Programming Table
  void initialize_dp() 
  {
    int n = columns.size();
    assert(n != 0);
    matrix<double> M(n,R);
    Forward = vector< vector<matrix<double> > > (R,vector<matrix<double> >(R,M));
  }
};

using std::cerr;

void sample_alignment_rates(Parameters& P, MCMC::MoveStats& Stats)
{
  // handle the first partition ONLY
  const alignment& A = *P[0].A;
  const SequenceTree& T = *P.T;
  int root = T.n_nodes()-1;           //FIXME - this is a bit of a hack.
  int R = 2;                          //FIXME - so is this
  
  insertion_graph G(A,T,root);

  cerr<<"got here"<<endl;

  for(int i=0;i<G.insertions.size();i++) {
    const insertion& I = G.insertions[i];
    cerr<<i<<" ("<<I.prev()<<" [";
    for(int j=1;j<I.columns.size()-1;j++) {
      cerr<<I.columns[j];
      if (j<I.columns.size()-2)
	cerr<<",";
    }
    cerr<<"] "<<I.next()<<" )"<<endl;
  }

  
  // go through insertions
  for(int c=0; c<A.length(); c++)
  {
  }


  // Forward probabilities F[i][j][k]:
    // these are the probabilities of the insertion we are in, up to the current point.
    // for each column, where 
    // * the current column has rate=j, 
    // * the column BEFORE the insertion has rate=i
    //   [colum C<current than emits a letter in 
    // * the column AFTER the insertion has rate=k

  // for each insertion - from deepest to highest
  //   for each position Y from first to last+1
  //     Consider all previous columns X
  //     * ONE of them is on the same level as this one.
  //     * all others Q are completed insertions into this insertion.
  //     All the others should have P as their front-ancestor and J as their back-ancestor

  //     Therefore we multiply by (sum[Q] F[X][Q][Y] * P(s[Q].s[X]) ) for each sub-insertion
  //     We also multiply by P(s[X],s[Y]) for the one insertion on the same level.

}
  

  // Go through each insertion, 

  // OK, we must
  // 1. Compute the forward matrix for each rate option
  
  // 2. Sample the states for each column
  // 3. Find the new column types from the new states
  //    (We aren't actually change the ALIGNMENT yet.
  // 4. 


/*
 * Go through columns in order.  
   For each column, process 
   * each 
   */
