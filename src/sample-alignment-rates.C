#include "sample.H"

#include "parameters.H"

#include "choose.H"

int accept_multiple_try_MH(efloat_t p1, const vector<efloat_t>& p2,efloat_t rho)
{
  const int n = p2.size();

  int c = choose<efloat_t>(p2);

  efloat_t ratio_bottom = p1*efloat_t(n);

  efloat_t ratio_top = 0;
  for(int i=0;i<n;i++)
    ratio_top    += p2[i];

  efloat_t ratio = ratio_top/ratio_bottom*rho;

  if (ratio >= 1.0 or uniform() < ratio) 
    return c;
  else
    return -1;
}

int accept_multiple_try_MH(efloat_t p1, const vector<efloat_t>& p2,
			   const vector<efloat_t>& rho_12, const vector<efloat_t>& rho_21)
{
  const int n = p2.size();

  int c = choose<efloat_t>(p2);

  efloat_t ratio_top    = 0;
  efloat_t ratio_bottom = 0;
  for(int i=0;i<n;i++) {
    ratio_top    += p2[i]*rho_21[i];
    ratio_bottom += p1   *rho_12[i];
  }

  efloat_t ratio = ratio_top/ratio_bottom;

  if (ratio >= 1.0 or uniform() < ratio) 
    return c;
  else
    return -1;
}

int accept_multiple_try_MH(const Parameters& P1,const vector<Parameters>& P2,
			    const vector<efloat_t>& rho_12, const vector<efloat_t>& rho_21)
{
  const int n = P2.size();

  efloat_t p1 = P1.heated_probability();

  vector<efloat_t> p2(n);
  for(int i=0;i<n;i++)
    p2[i] = P2[i].heated_probability();

  return accept_multiple_try_MH(p1,p2,rho_12,rho_21);
}

int accept_multiple_try_symmetric(efloat_t p1, const vector<efloat_t>& p2)
{
  const int n = p2.size();

  int c = choose<efloat_t>(p2);

  efloat_t ratio_bottom = p1*efloat_t(n);

  efloat_t ratio_top = 0;
  for(int i=0;i<n;i++)
    ratio_top    += p2[i];

  efloat_t ratio = ratio_top/ratio_bottom;

  if (ratio >= 1.0 or uniform() < ratio) 
    return c;
  else
    return -1;
}


int accept_multiple_try_symmetric(const Parameters& P1,const vector<Parameters>& P2)
{
  const int n = P2.size();

  efloat_t p1 = P1.heated_probability();

  vector<efloat_t> p2(n);
  for(int i=0;i<n;i++)
    p2[i] = P2[i].heated_probability();

  return accept_multiple_try_symmetric(p1,p2);
}


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

  // the 0th insertion is the top sequence.
  vector<insertion> insertions;

  // for each column, including the 'E' column
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

  insertions.push_back(insertion(-1));
  insertions[0].columns.push_back(-1);

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

    if (insertion_for_column[c] == -1 and c < A.length()) 
    {
      insertion_for_column[c] = 0;
      insertions[0].columns.push_back(c);
    }
  }

  insertions[0].columns.push_back(A.length());
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

int get_prev_column(const alignment& A,int n1, int n2, int c)
{
  for(int i=c-1;i>=0;i--)
    if (A.character(i,n1) or A.character(i,n2))
      return i;
  return -1;
}

int get_prev_column(const alignment& A,const Tree& T, int b, int c)
{
  int n1 = T.branch(b).source();
  int n2 = T.branch(b).target();
  return get_prev_column(A,n1,n2,c);
}


void sample_alignment_rates(Parameters& P, MCMC::MoveStats& Stats)
{
  // handle the first partition ONLY
  const alignment& A = *P[0].A;
  const SequenceTree& T = *P.T;
  int root = T.n_nodes()-1;           //FIXME - this is a bit of a hack.
  int R = 2;                          //FIXME - so is this
  
  insertion_graph G(A,T,root);

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


  cerr<<join(G.insertion_for_column," ")<<endl;

  vector<int> index_into_insertion(G.insertions.size(),0);
  
  // Allocate space for calculating forward probabilities Forward[pS][nS](c,cS)
  //   where we are conditioning of PREV-state (pS) and NEXT-state (nS)

  // We are not reserving space for 
  matrix<double> M(A.length(),R);
  vector< vector<matrix<double> > > Forward(R,vector<matrix<double> >(R,M));

  // List of correctly oriented branches
  vector<const_branchview> branches = branches_from_node(T,root);

  // go through insertions
  for(int c=0; c<A.length(); c++)
  {
    int which_ins = G.insertion_for_column[c];
    
    insertion& current_insertion = G.insertions[which_ins];

    index_into_insertion[which_ins]++;

    assert(current_insertion.columns[index_into_insertion[which_ins]] == c);

    int prev_column = current_insertion.columns[index_into_insertion[which_ins]-1];

    cerr<<"\ncolumn "<<c<<":     which_ins = "<<which_ins<<"       prev_column = "<<prev_column<<endl;

    vector<int> prev_columns;
    prev_columns.push_back(prev_column);
    for(int j=0;j<branches.size();j++) 
    {
      int n1 = branches[j].source();
      int n2 = branches[j].target();
      int b =  branches[j].undirected_name();

      if (not A.character(c,n1) and not A.character(c,n2))
	continue;

      int pc = get_prev_column(A,n1,n2,c);

      //      if (b == current_insertion.b)
      //	assert(pc == prev_column);

      if (pc < prev_column) {
	if (not includes(prev_columns,pc))
	  prev_columns.push_back(pc);
	cerr<<"    branch "<<b<<":  pc = "<<pc<<endl;
      }
      else if (pc > prev_column) {
	// This must be an insertion nested in to this one...
	int prev_ins = G.insertion_for_column[pc];
	insertion& nested_insertion = G.insertions[prev_ins];

	if (nested_insertion.next() != c) {
	  cerr<<"    nested insertion? "<<prev_ins<<endl;
	  cerr<<"    branch "<<nested_insertion.b<<": pc = "<<pc<<" NEXT = "<<nested_insertion.next()<<endl;
	}
	// We can handle these directly.
	if (nested_insertion.prev() == prev_column) {
	  cerr<<"    PROCESS = "<<prev_ins<<"  ["<<nested_insertion.prev()<<"-"<<nested_insertion.next()<<"]   branch = "<<nested_insertion.b<<endl;
	}
	// Don't handle these, because these are included in the ones we DO process.
	else {
	  cerr<<"        SUB = "<<prev_ins<<"  ["<<nested_insertion.prev()<<"-"<<nested_insertion.next()<<"]   branch = "<<nested_insertion.b<<endl;
	}
      }
    }

    if (prev_columns.size() > 1)
      cerr<<" column "<<c<<":  "<<prev_columns.size()<<" in-edges."<<endl;
    
    

    // previous column in insertion is conditioned on (as PREV column), not summed over
    if (current_insertion.prev() == prev_column)
    {
      for(int pS=0;pS<R;pS++)
	for(int nS=0;nS<R;nS++)
	  for(int cS=0;cS<R;cS++)
	    Forward[pS][nS](c,cS) = 1; //transition_p(G.b, 
    }
    else {
      for(int pS=0;pS<R;pS++)
	for(int nS=0;nS<R;nS++)
	  for(int cS=0;cS<R;cS++)
	  {
	    double total = 1;
	    
	    Forward[pS][nS](c,cS) = total; //transition_p(G.b, 	  
	  }
    }
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

struct column_adjacency_graph 
{
  vector< vector<int> > neighbors;

  int size() const {return neighbors.size();}

  void add_edge(int i,int j) 
  {
    if (not includes(neighbors[i],j)) 
    {
      neighbors[i].push_back(j);
      neighbors[j].push_back(i);
    }
  }

  column_adjacency_graph(int L)
    :neighbors(L)
  { }
};

column_adjacency_graph get_adjacency_graph(const alignment& A, const Tree& T, int root)
{

  column_adjacency_graph G(A.length());

  // add edges for the root sequence
  int last_c = -1;
  for(int c=0;c<A.length();c++) 
  {
    if (A.character(c,root)) {
      if (last_c != -1)
	G.add_edge(last_c,c);
      last_c = c;
    }
  }

  vector<const_branchview> branches = branches_from_node(T,root);

  // add edges for each branch sequence
  for(int j=0;j<branches.size();j++)
  {
    int n1 = branches[j].source();
    int n2 = branches[j].target();
    int last_c = -1;

    // add edges for branch b
    for(int c=0;c<A.length();c++) {
      if (A.character(c,n1) or A.character(c,n2)) {
	if (last_c != -1)
	  G.add_edge(last_c,c);
	last_c = c;
      }
    }
  }

  return G;
}
    
void cluster_proposal_probs(const column_adjacency_graph& G, double p, const alignment& A, 
			    const vector<int>& cluster, efloat_t rho_12,efloat_t rho_21)
{
  ublas::matrix<int>& type_note = A.note(2);
  int current_spin = type_note(cluster[0],0);

  // mark nodes that are part of the cluster, to avoid searching every time.
  vector<int> part_of_cluster(G.size(),0);
  for(int i=0;i<cluster.size();i++)
    part_of_cluster[cluster[i]] = 1;

  // find out how many bordering nodes have the same or different spins as the cluster
  int n_same = 0;
  int n_diff = 0;

  for(int i=0;i<cluster.size();i++) 
  {
    int c = cluster[i];

    // scan the columns next to c
    for(int j=0;j<G.neighbors[c].size();j++) 
    {
      int c2 = G.neighbors[c][j];

      // only consider columns that are not in the cluster
      if (part_of_cluster[c2]) continue;

      if (type_note(c2,0) == current_spin)
	n_same++;

      if (type_note(c2,0) != current_spin)
	n_diff++;
    }


  }

  //  std::cerr<<"    spin = "<<current_spin<<"   n_same = "<<n_same<<"  n_diff = "<<n_diff<<endl;
  efloat_t R = (1-p);
  rho_12 = pow<efloat_t>(R,n_same);
  rho_21 = pow<efloat_t>(R,n_diff);
}


// compute to proposal ration (rho_ji/rho_ij) for finding same-spin clusters.
efloat_t cluster_proposal_ratio(const column_adjacency_graph& G, double p, const alignment& A, 
				const vector<int>& cluster)
{
  efloat_t rho_12=1;
  efloat_t rho_21=1;
  cluster_proposal_probs(G,p,A,cluster,rho_12,rho_21);
  return rho_21/rho_12;
}

// to consider edges only once... only consider edges to a non-visted node?
vector<int> get_cluster(const column_adjacency_graph& G, double p, const alignment& A, int c)
{
  ublas::matrix<int>& type_note = A.note(2);
  int current_spin = type_note(c,0);

  vector<int> part_of_cluster(G.size(),0);

  vector<int> columns;
  columns.push_back(c);
  part_of_cluster[c] = 1;

  for(int i=0;i<columns.size();i++)
  {
    int c = columns[i];
    
    // scan the columns next to c
    for(int j=0;j<G.neighbors[c].size();j++) 
    {
      int c2 = G.neighbors[c][j];

      // ignore columns we've already added, or disallowed columns
      if (part_of_cluster[c2] or type_note(c2,0) != current_spin)
	continue;

      if (uniform() < p) {
	columns.push_back(c2);
	part_of_cluster[c2] = 1;
      }
    }
  }

  return columns;
}

/*
 * Go through columns in order.  
   For each column, process 
   * each 
   */
void sample_alignment_rates_flip_column(Parameters& P, MCMC::MoveStats& Stats)
{
  int root = P.T->n_nodes()-1;

  for(int p=0;p<P.n_data_partitions();p++) 
  {
    int L = P[p].A->length();

    // do all single flips
    for(int c=0;c<L;c++) 
    {
      MCMC::Result result(2);

      Parameters P2 = P;
      alignment& A2 = *P2[p].A;
      ublas::matrix<int>& type_note = A2.note(2);
      type_note(c,0) = 1-type_note(c,0);
      P2[p].note_column_label_changed();
      
      bool success = accept_MH(P,P2,1.0);
      
      if (success) {
	P=P2;
	//    std::cerr<<"accepted\n";
	result.totals[0] = 1; // number of successful moves
	result.totals[1] = 1; // number of columns flipped
      }
      else {
	//    std::cerr<<"rejected\n";
      }
    
      Stats.inc("flip-single-column",result);

    }

    column_adjacency_graph G = get_adjacency_graph(*P[p].A,*P.T,root);

    int LS_index = find_parameter(P,"lambda_s");
    int LF_index = find_parameter(P,"lambda_f");
    vector<int> L_indices;
    L_indices.push_back(LS_index);    
    L_indices.push_back(LF_index);    

    vector<double> L_values(2);

    if (LS_index == -1)
      throw myexception()<<"Can't find parameter lambda_s!";
    if (LF_index == -1)
      throw myexception()<<"Can't find parameter lambda_s!";

    // Do a Wolff-stype algorithm that flips neighboring groups with the same spin
    for(int i=0;i<L;i++) 
    {
      MCMC::Result result(6);

      Parameters P2 = P;
      alignment& A2 = *P2[p].A;
      ublas::matrix<int>& type_note = A2.note(2);

      // Average size will be 1+2/(1-r)   
      double p = 0.8;
      if (uniform() < 0.5)
	p = 0.6;
      vector<int> cluster = get_cluster(G, p, A2, i);
      efloat_t rho = cluster_proposal_ratio(G, p, A2, cluster);

      //      cerr<<"column "<<i<<": size = "<<cluster.size()<<"     [ "<<join(cluster,' ')<<" ]"<<endl;

      for(int j=0;j<cluster.size();j++) {
	int c = cluster[j];
	type_note(c,0) = 1-type_note(c,0);
      }
      P2[p].note_column_label_changed();
	
      string name = "flip-multiple-column-";
      if (p < 0.7)
	name += "0.6";
      else
	name += "0.8";

      // Also 
      if (uniform() < 0.25) 
      {
	bool s = (uniform()<0.5);
	if (s) {
	  P2.parameter(LS_index,P.parameter(LS_index)+gaussian(0,1.0));
	  name += "-LS";
	}
	else {
	  P2.parameter(LF_index,P.parameter(LF_index)+gaussian(0,0.25));
	  name += "-LF";
	}
      }

      bool success = accept_MH(P,P2,rho);
      
      result.totals[2] = cluster.size(); // number of columns flipped
      if (success) {
	//    std::cerr<<"accepted\n";
	result.totals[0] = 1;              // number of successful moves
	result.totals[1] = cluster.size(); // number of columns flipped
	result.totals[3] = cluster.size(); // number of columns flipped
	result.totals[4] = std::abs(P2.parameter(LS_index)-P.parameter(LS_index));
	result.totals[5] = std::abs(P2.parameter(LF_index)-P.parameter(LF_index));
	P=P2;
      }
      else {
	result.counts[3] = 0;
	result.counts[4] = 0;
	result.counts[5] = 0;
	//    std::cerr<<"rejected\n";
      }

      Stats.inc(name,result);
    }
      
  }
}
