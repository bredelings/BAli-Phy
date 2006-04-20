#include "rng.H"
#include "sample.H"
#include "mcmc.H"
#include "util.H"
#include "5way.H"
#include "substitution-cache.H"
#include "substitution-index.H"
#include "substitution.H"
#include "likelihood.H"
#include "proposals.H"

using MCMC::MoveStats;

bool do_MH_move(const alignment& A,Parameters& P,const Parameters& P2,double rho) {
  if (P.accept_MH(A,P,A,P2,rho)) {
    P=P2;
    //    std::cerr<<"accepted\n";
    return true;
  }
  else {
    //    std::cerr<<"rejected\n";
    return false;
  }
}

double branch_twiddle(double& T,double sigma) {
  T += gaussian(0,sigma);
  return 1;
}

double branch_twiddle_positive(double& T,double sigma) {
  double ratio = branch_twiddle(T,sigma);
  T = std::abs(T);
  return ratio;
}

MCMC::Result change_branch_length_(const alignment& A, Parameters& P,int b,
				   double sigma,double (*twiddle)(double&,double)) 
{
  MCMC::Result result(2);
  
  //------------ Propose new length -------------//
  const double length = P.T.branch(b).length();
  double newlength = length;

  double ratio = twiddle(newlength,sigma);
  
  //---------- Construct proposed Tree ----------//
  select_root(P.T, b, P.LC);

  Parameters P2 = P;
  P2.setlength(b,newlength);

  //--------- Do the M-H step if OK--------------//
  if (do_MH_move(A,P,P2,ratio)) {
    result.totals[0] = 1;
    result.totals[1] = std::abs(length - newlength);
  }

  return result;
}

void change_branch_length_flat(const alignment& A, Parameters& P,MoveStats& Stats,int b,double sigma)
{
  MCMC::Result result = change_branch_length_(A, P, b, sigma*P.branch_mean(), branch_twiddle_positive);

  Stats.inc("branch-length",result);
}

void change_branch_length_log_scale(const alignment& A, Parameters& P,MoveStats& Stats,int b,double sigma)
{
  MCMC::Result result = change_branch_length_(A, P, b, sigma, scale_gaussian );

  Stats.inc("branch-length (log)",result);
}

void change_branch_length(const alignment& A, Parameters& P,MoveStats& Stats,int b) {

  double r = loadvalue(P.keys,"log_branch_fraction",0.75);

  if (myrandomf() < r) {
    double sigma = loadvalue(P.keys,"log_branch_sigma",0.6);
    change_branch_length_log_scale(A, P, Stats, b, sigma);
  }
  else {
    double sigma = loadvalue(P.keys,"branch_sigma",0.6);
    change_branch_length_flat(A, P, Stats, b, sigma);
  }
}

void change_branch_length_multi(const alignment& A, Parameters& P,MoveStats& Stats,int b) 
{
  const int n=3;

  for(int i=1;i<n;i++)
    change_branch_length(A,P,Stats,b);
}

void change_branch_length_and_T(alignment& A, Parameters& P,MoveStats& Stats,int b) 
{
  MCMC::Result result(5,0);

  result.counts[0] = 1;

  //------------- Propose new length --------------//
  const double length = P.T.branch(b).length();
  double newlength = length;
  double ratio = branch_twiddle(newlength,P.branch_mean()*0.6);

  //----- positive  =>  propose length change -----//
  if (newlength >= 0) 
  {
    result.counts[1] = 1;
    result.counts[3] = 1;

    //---------- Construct proposed Tree ----------//
    select_root(P.T, b, P.LC);

    Parameters P2 = P;
    P2.setlength(b,newlength);

    //--------- Do the M-H step if OK--------------//
    if (do_MH_move(A,P,P2,ratio)) {
      result.totals[0] = 1;
      result.totals[1] = 1;
      result.totals[3] = std::abs(newlength - length);
    }
  }

  //----- negative  => propose topology ---------//
  else 
  {
    result.counts[2] = 1;
    result.counts[4] = 1;

    //----- Generate the Different Topologies ------//
    vector<alignment> a(2,A);
    vector<Parameters> p(2,P);
    
    SequenceTree& T2 = p[1].T;
    
    vector<int> nodes = A5::get_nodes_random(T2,b);
    int b1 = T2.directed_branch(nodes[4],nodes[1]);
    int b2 = T2.directed_branch(nodes[5],nodes[2]);
    T2.exchange_subtrees(b1,b2);

    invalidate_subA_index_branch(a[1],T2,b);

    p[1].setlength(b,-newlength);
    
    vector<efloat_t> rho(2,1);
    rho[1] = ratio;

    //------ Sample the Different Topologies ------//
    int C = two_way_topology_sample(a,p,rho,b);

    if (C != -1) {
      A = a[C];
      P = p[C];
    }

    if (C > 0) {
      result.totals[0] = 1;
      result.totals[2] = 1;
      result.totals[4] = std::abs(length - newlength);
    }
  }

  Stats.inc("change_branch_length_and_T",result);
}

double slide_node_no_expand_branch(vector<double>& lengths,double) 
{
  double L = lengths[0] + lengths[1];

  lengths[0] = L * uniform();
  lengths[1] = L - lengths[0];

  return 1;
}


double slide_node_expand_branch(vector<double>& lengths,double sigma) 
{
  double ratio = exp( gaussian(0,sigma) );

  double L = (lengths[0] + lengths[1]) * ratio;

  lengths[0] = L * uniform();
  lengths[1] = L - lengths[0];

  return ratio*ratio;
}


bool slide_node(const alignment& A, Parameters& P,
		const vector<const_branchview>& b,
		double (*slide)(vector<double>&,double)
		) 
{
  // check that we've got three branches
  assert(b.size() == 3);

  // check that the last two are after the first one
  assert(b[0].target() == b[1].source() and
	 b[0].target() == b[2].source());

  P.LC.root = b[0].target();

  //---------------- Propose new lengths ---------------//
  vector<double> lengths(2);
  lengths[0] = b[1].length();
  lengths[1] = b[2].length();

  double sigma = loadvalue(P.keys,"slide_node_sigma",0.3);
  double ratio = slide(lengths,sigma);

  //---------------- Propose new lengths ---------------//
  Parameters P2 = P;

  P2.setlength(b[1].undirected_name(), lengths[0]);
  P2.setlength(b[2].undirected_name(), lengths[1]);
    
  bool success = do_MH_move(A,P,P2,ratio);

  return success;
}

void slide_node(const alignment& A, Parameters& P,MoveStats& Stats,int b0)
{
  vector<const_branchview> b;
  b.push_back( P.T.directed_branch(b0) );

  // choose branches to alter
  if (uniform() < 0.5)
    b[0] = b[0].reverse();
  if (b[0].target().is_leaf_node())
    b[0] = b[0].reverse();
  append(b[0].branches_after(),b);

  if (uniform() < 0.5) {
    bool success = slide_node(A, P, b, slide_node_no_expand_branch);
    Stats.inc("slide_node",success);
  }
  else {
    bool success = slide_node(A, P, b, slide_node_expand_branch);
    Stats.inc("slide_node_expand_branch",success);
  }
}

void scale_branch_lengths_and_mean(alignment& A, Parameters& P,MoveStats& Stats) 
{
  if (P.fixed(0))
    return;

  MCMC::Result result(2);

  //------------ Propose scaling ratio ------------//
  const double sigma = loadvalue(P.keys,"log_branch_mean_sigma",0.6);
  double scale = exp( gaussian(0,sigma) );

  //------- Propose branch lengths and mean -------//
  Parameters P2 = P;
  const SequenceTree& T = P2.T;

  for(int b=0;b<T.n_branches();b++) {
    const double length = P.T.branch(b).length();
    P2.setlength(b, length * scale);
  }

  P2.branch_mean(P2.branch_mean() * scale);

  double ratio =  pow(scale, 1 + T.n_branches() );

  //----------- Do the M-H step if OK--------------//
  if (do_MH_move(A,P,P2, ratio)) {
    result.totals[0] = 1;
    result.totals[1] = std::abs(log(scale));
  }

  Stats.inc("branch-mean",result);
}

