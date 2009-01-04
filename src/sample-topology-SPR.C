#include <cmath>
#include <valarray>
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "choose.H"
#include "likelihood.H"
#include "probability.H"

#include "3way.H"
#include "tree-util.H"
#include "alignment-sums.H"
#include "alignment-constraint.H"
#include "substitution-index.H"

using MCMC::MoveStats;

int topology_sample_SPR(vector<Parameters>& p,const vector<efloat_t>& rho,int n1, int n2) 
{
  assert(p[0].n_imodels() == p[1].n_imodels());

  //----------- Generate the Different node lists ---------//
  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(*p[0].T, n1, n2);
  nodes[1] = A3::get_nodes_branch_random(*p[1].T, n1, n2);

  return sample_tri_multi(p,nodes,rho,true,true);
}

#include "slice-sampling.H"

int topology_sample_SPR_slice_connecting_branch(vector<Parameters>& p,int b) 
{
  int b_ = p[0].T->directed_branch(b).undirected_name();

  branch_length_slice_function logp1(p[0],b_);
  branch_length_slice_function logp2(p[1],b_);

  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  //  double Pr1 = log(p[0].probability());
  //  double Pr2 = log(p[1].probability());

  vector<slice_function*> logp;
  logp.push_back(&logp1);
  logp.push_back(&logp2);

  double w = p[0].branch_mean();

  std::pair<int,double> choice = slice_sample_multi(logp,w,-1);

  return choice.first;
}

int topology_sample_SPR_slice_slide_node(vector<Parameters>& p,int b) 
{
  int b_ = p[0].T->directed_branch(b).undirected_name();

  slide_node_slice_function logp1(p[0],b);
  slide_node_slice_function logp2(p[1],b);

  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  //  double Pr1 = log(p[0].probability());
  //  double Pr2 = log(p[1].probability());

  vector<slice_function*> logp;
  logp.push_back(&logp1);
  logp.push_back(&logp2);

  double w = p[0].branch_mean();

  vector<double> X0(2);
  X0[0] = logp1.current_value();
  X0[1] = uniform()*(logp2.upper_bound);

  std::pair<int,double> choice = slice_sample_multi(X0,logp,w,-1);

  return choice.first;
}

/// Do a SPR move on T1, moving the subtree behind b1_ to branch b2
double do_SPR(SequenceTree& T1, int b1_,int b2) 
{
  const_branchview b1 = T1.directed_branch(b1_);

  SequenceTree T2 = T1;

  //------ Generate the new topology ------//
  if (T2.directed_branch(b2).target() == b1.target() or 
      T2.directed_branch(b2).source() == b1.target()) 
    ;
  else
    SPR(T2,b1.reverse(),b2);

  //------ Find the two new branches ------//
  vector<const_branchview> connected1;
  append(T1.directed_branch(b1.source(),b1.target()).branches_after(),connected1);

  vector<const_branchview> connected2;
  append(T2.directed_branch(b1.source(),b1.target()).branches_after(),connected2);

  assert(connected1.size() == 2);
  assert(connected2.size() == 2);

  //------- Place the split randomly -------//
  double L1 = connected1[0].length() + connected1[1].length();
  double L2 = connected2[0].length() + connected2[1].length();

  T2.directed_branch(connected2[0]).set_length( myrandomf() * L2 );
  T2.directed_branch(connected2[1]).set_length( L2 - T2.directed_branch(connected2[0]).length() );

  T1 = T2;

  return L2/L1;
}


double do_SPR(Parameters& P, int b1, int b2)
{
  double ratio = do_SPR(*P.T, b1, b2);
  P.tree_propagate();
  return ratio;
}

int choose_SPR_target(SequenceTree& T1, int b1_) 
{
  const_branchview b1 = T1.directed_branch(b1_);

  //----- Select the branch to move to ------//
  valarray<bool> subtree_nodes = T1.partition(b1.reverse());
  subtree_nodes[b1.target()] = true;

  vector<int> branches;
  vector<double> lengths;

  for(int i=0;i<T1.n_branches();i++) 
  {
    const_branchview bi = T1.branch(i);

    // skip branch if its contained in the subtree
    if (subtree_nodes[bi.target()] and 
	subtree_nodes[bi.source()])
      continue;

    double L = 1.0;

    // down-weight branch if it is one of the subtree's 2 neighbors
    if (subtree_nodes[bi.target()] or 
	subtree_nodes[bi.source()])
      L = 0.5;

    branches.push_back(i);
    lengths.push_back(L);
  }

  int b2 = branches[ choose(lengths) ];

  return b2;
}

void remove_duplicates(vector<int>& v) {
  for(int i=v.size()-1;i>=0;i--) {
    bool dup=false;
    for(int j=0;j<i and not dup;j++)
      if (v[j] == v[i]) dup=true;
    if (dup)
      v.erase(v.begin()+i);
  }
}

MCMC::Result sample_SPR(Parameters& P,int b1,int b2,bool slice=false) 
{
  const int bins = 4;

  int n1 = P.T->directed_branch(b1).target();
  int n2 = P.T->directed_branch(b1).source();
  assert(P.T->partition(b1)[P.T->branch(b2).target()]);
  assert(P.T->partition(b1)[P.T->branch(b2).source()]);

  //----- Generate the Different Topologies ----//
  P.set_root(n1);
  vector<Parameters> p(2,P);

  //---------------- find the changed branches ------------------//
  vector<int> branches;
  for(edges_after_iterator i=p[1].T->directed_branch(n2,n1).branches_after();i;i++)
    branches.push_back((*i).undirected_name());
  //  std::cerr<<"before = "<<p[1].T<<endl;

  double ratio = do_SPR(p[1],b1,b2);
  if (not extends(*p[1].T, *P.TC))
    return MCMC::Result(2+bins,0);

  //  std::cerr<<"after = "<<p[1].T<<endl;
  for(edges_after_iterator i=p[1].T->directed_branch(n2,n1).branches_after();i;i++)
    branches.push_back((*i).undirected_name());

  remove_duplicates(branches);
    
  //----------- invalidate caches for changed branches -----------//
  assert(branches.size() <= 3);
  for(int i=0;i<branches.size();i++) {
    int bi = branches[i];
    p[1].setlength(bi,p[1].T->directed_branch(bi).length());
    p[1].invalidate_subA_index_branch(branches[i]);
    p[1].note_alignment_changed_on_branch(bi);
  }

  int C;
  if (slice)
  {
    C = topology_sample_SPR_slice_slide_node(p,b1);
    P = p[C];
  }
  else {

    //------------- change connecting branch length ----------------//
    vector<efloat_t> rho(2,1);

    //----------- sample alignments and choose topology -----------//
    C = topology_sample_SPR(p,rho,n1,n2);
    
    if (C != -1) 
    {
      for(int i=0;i<P.n_data_partitions();i++) {
	valarray<bool> s1 = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
	valarray<bool> s2 = constraint_satisfied(p[C][i].alignment_constraint, *p[C][i].A);
	
	report_constraints(s1,s2);
      }
      P = p[C];

    // If the new topology conflicts with the constraints, then it should have P=0
    // and therefore not be chosen.  So the following SHOULD be safe!
    }
  }


  //---------------- Check if topology changed ----------------//
  vector<const_branchview> connected1;
  append(p[0].T->directed_branch(n2,n1).branches_after(),connected1);

  vector<const_branchview> connected2;
  append(p[1].T->directed_branch(n2,n1).branches_after(),connected2);
 
  bool same_topology = (
			(connected1[0] == connected2[0] and connected1[1] == connected2[1]) or
			(connected1[0] == connected2[1] and connected1[1] == connected2[0])
			);

  MCMC::Result result(2+bins,0);

  result.counts[0] = 1;
  if (C>0) result.totals[0] = 1;

  int dist = topology_distance(*p[0].T, *p[1].T)/2;

  if (same_topology)
    assert(dist == 0);
  else
    assert(dist > 0);

  // count dist in [0,bins)
  if (dist > bins) dist = bins;
  for(int i=0;i<=bins;i++) {
    if (dist == i) {
      result.counts[1+i] = 1;
      if (C>0) result.totals[1+i] = 1;
    }
  }

  return result;
}

int choose_subtree_branch_uniform(const Tree& T) {
  int b1 = -1;
  do {
    b1 = myrandom(T.n_branches()*2);
  }
  while (T.directed_branch(b1).target().is_leaf_node());
  return b1;
}


void sample_SPR_flat(Parameters& P,MoveStats& Stats) 
{
  double f = loadvalue(P.keys,"SPR_amount",0.1);
  int n = poisson(P.T->n_branches()*f);

  double p = loadvalue(P.keys,"SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) 
  {
    int b1 = choose_subtree_branch_uniform(*P.T);

    int b2 = choose_SPR_target(*P.T,b1);

    if (P.n_imodels() == 0 and uniform() < p) {
      MCMC::Result result = sample_SPR(P,b1,b2,true);
      Stats.inc("SPR (flat/slice)", result);
    }
    else {
      MCMC::Result result = sample_SPR(P,b1,b2);
      Stats.inc("SPR (flat)", result);
    }
  }
}


vector<int> path_to(const Tree& T,int n1, int n2) 
{
  assert(0 <= n1 and n1 < T.n_leaves());
  assert(0 <= n2 and n2 < T.n_leaves());
  assert(n1 != n2);

  vector<int> path; 
  path.push_back(n1);
  path.push_back(T.branch(n1).target());

  while(path.back() != n2) 
  {
    const_branchview b = T.directed_branch(path[path.size()-2], path[path.size()-1]);

    for(const_edges_after_iterator i=b.branches_after();i;i++) 
    {
      if (T.partition(*i)[n2]) {
	path.push_back((*i).target());
	break;
      }
    }
  }

  return path;
}

int jump() {
  double delta = 0;

  double U = uniform();
  if (U < 0.05)
    delta = 0;
  else if (U < 0.15)
    delta = 1;
  else {
    double a = 1;
    double b = 4;
    if (uniform() < 0.5) {
      a = 0.5;
      b = 4;
    }
    
    delta = 2+gamma(a,b);// mean = 2.5, shape = 2
  }
  if (uniform() < 0.5) delta = -delta;

  // round to the nearest integer
  return (int)floor(delta+0.5);
}



void choose_subtree_branch_nodes(const Tree& T,int & b1, int& b2) 
{
  //------------------- Choose nodes --------------------//
  int n1 = myrandom(T.n_leaves());
  int n2 = -1;
  do { n2 = myrandom(T.n_leaves());} while (n2 == n1);

  vector<int> path = path_to(T,n1,n2);
  assert(path.size() >= 3);

  //-------------- Choose subtree on path ----------------//
  int N = path.size() - 2;
  int A = 1+myrandom(N);

  b1 = -1;
  for(const_neighbors_iterator i=T[path[A]].neighbors();i;i++) {
    if (*i == path[A-1]) continue;
    if (*i == path[A+1]) continue;

    b1 = T.directed_branch(*i,path[A]);
    break;
  }
  assert(b1 != -1);

  //-------------- Choose branch on path ----------------//

  // The allowed branches are [Nl,Nr]
  int Nl = -(A-1) , Nr = N-A;

  // Jump and then reflect so that we stay in the interval
  int delta = wrap(jump(), Nl, Nr);
  assert(Nl <= delta and delta <= Nr);

  // walk 'delta' way from A
  int C2 = delta + A;

  assert(1 <= C2 and C2 <= path.size()-2);

  int C3 = C2;
  if (delta < 0) C3--;
  else C3++;

  assert(0 <= C3 and C3 <= path.size()-1);

  b2 = T.branch(path[C2],path[C3]);
}

void sample_SPR_nodes(Parameters& P,MoveStats& Stats) 
{
  double f = loadvalue(P.keys,"SPR_amount",0.1);
  int n = poisson(P.T->n_branches()*f);

  double p = loadvalue(P.keys,"SPR_slice_fraction",-0.25);

  for(int i=0;i<n;i++) {

    int b1=-1, b2=-1;
    choose_subtree_branch_nodes(*P.T, b1, b2);

    if (P.n_imodels() == 0 and uniform()< p) {
      MCMC::Result result = sample_SPR(P,b1,b2,true);
      Stats.inc("SPR (path/slice)", result);
    }
    else {
      MCMC::Result result = sample_SPR(P,b1,b2);
      Stats.inc("SPR (path)", result);
    }
  }
}
