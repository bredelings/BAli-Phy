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
#include <gsl/gsl_cdf.h>

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
  MCMC::Result result(3);
  
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
    result.totals[2] = std::abs(log(length/newlength));
  }

  return result;
}


double logp_(const alignment& A, Parameters& P, int b,double l)
{
  P.setlength(b,l);
  return log(P.probability(A,P));
}

vector<double> fit_quadratic(double x1,double x2,double x3,double y1,double y2,double y3)
{
  double D = x1*x1*(x2-x3) - x2*x2*(x1-x3) + x3*x3*(x1-x2);
  vector<double> v(3);
  v[0] = y1*(x2-x3) - y2*(x1-x3) + y3*(x1-x2);
  v[1] = x1*x1*(y2-y3) - x2*x2*(y1-y3) + x3*x3*(y1-y2);
  v[2] = x1*x1*(x2*y3-x3*y2) - x2*x2*(x1*y3-x3*y1) + x3*x3*(x1*y2-x2*y1);
  for(int i=0;i<v.size();i++)
    v[i] /= D;

  return v;
}

vector<double> fit_gamma(double x1,double x2,double x3,double y1,double y2,double y3)
{
  double D = log(x1)*(x2-x3) - log(x2)*(x1-x3) + log(x3)*(x1-x2);
  vector<double> v(3);
  v[0] = y1*(x2-x3) - y2*(x1-x3) + y3*(x1-x2);
  v[1] = log(x1)*(y2-y3) - log(x2)*(y1-y3) + log(x3)*(y1-y2);
  v[2] = log(x1)*(x2*y3-x3*y2) - log(x2)*(x1*y3-x3*y1) + log(x3)*(x1*y2-x2*y1);
  for(int i=0;i<v.size();i++)
    v[i] /= D;

  //  std::cerr<<"x1 = "<<x1<<"\n";
  //  std::cerr<<"x2 = "<<x2<<"\n";
  //  std::cerr<<"x3 = "<<x3<<"\n";

  return v;
}

vector<double> gamma_approx(const alignment& A, const Parameters& P, int b)
{
  Parameters P2 = P;
  double w = 4;
  double x2 = P.branch_mean();
  double x1 = x2/w;
  double x3 = x2*w;

  double L1 = logp_(A,P2,b,x1);
  double L2 = logp_(A,P2,b,x2);
  double L3 = logp_(A,P2,b,x3);

  vector<double> v(3,0);
  v[1] = -1.0/P.branch_mean();

  for(int i=0;i<5;i++) 
  {
    vector<double> v2 = fit_gamma(x1,x2,x3,L1,L2,L3);
    double a = v2[0]+1.0;
    double B = -1.0/v2[1];

    //    std::cerr<<"i = "<<i<<"  L = "<<P.T.branch(b).length()<<"  a = "<<a<<"  b = "<<B<<"  mu = "<<a*B<<"  s = "<<1.0/sqrt(a)<<"  w = "<<w<<"\n";

    bool success = false;
    if (a > 0 and B > 0) {
      double xm = -v2[0]/v2[1];
      if (xm < 0) xm = 1.0e-6;
      double Lm = logp_(A,P2,b,xm);
      double max = std::max(L1,std::max(L2,L3));

      if (Lm < max) {
	//	std::cerr<<"predicted max at "<<xm<<" but "<<Lm<<" < "<<max<<"\n";
	if (v[0] == 0) v=v2;
      }
      else {
	success = true;
	x2 = xm;
	L2 = Lm;
	double a_old = v[0]+1.0;
	double B_old = -1.0/v[1];
	double mu_old = a_old * B_old;
	double sd_old = B_old*sqrt(a_old);
	double mu = a*B;
	double sd = B*sqrt(a);

	v = v2;
	if (std::abs(log(sd/sd_old)) < 0.05 and std::abs((mu-mu_old)/sd)< 0.05) {
	  //	  std::cerr<<"done!\n";
	  break;
	}


	x1 = gsl_cdf_gamma_Pinv(0.05,a,B);
	if (x2 < x1)
	  x1 = gsl_cdf_gamma_Pinv(0.25,a,B);
	else
	  x1 = gsl_cdf_gamma_Pinv(0.01,a,B);
	  
	L1 = logp_(A,P2,b,x1);
	
	x3 = gsl_cdf_gamma_Pinv(0.99,a,B);
	L3 = logp_(A,P2,b,x3);

	w = 2.0*B*sqrt(a);
      }
    }

    if (not success) {
      if (L2 > L1 and L2> L3) {
	w = sqrt(w);
	x1 = x2/w;
	x3 = x2*w;
	L1 = logp_(A,P2,b,x1);
	L3 = logp_(A,P2,b,x3);
      }
      else if (L1 > L2 and L1 > L3) {
	x3 = x2;L3 = L2;
	x2 = x1;L2 = L1;
	x1 = x2/w;
	L1 = logp_(A,P2,b,x1);
      }
      else {
	x1 = x2; L1 = L2;
	x2 = x3; L2 = L3;
	x3 = x2*w;
	L3 = logp_(A,P2,b,x3);
      }
    }
  }

  return v;
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

void change_branch_length_fit_gamma(const alignment& A, Parameters& P,MoveStats& Stats,int b)
{
  select_root(P.T, b, P.LC);

  vector<double> v = gamma_approx(A,P,b);

  double a = v[0]+1.0;
  double B = -1.0/v[1];
  if (a < 0 or B<0)
    return;
  
  MCMC::Result result(3);
  
  //------------ Propose new length -------------//
  const double length = P.T.branch(b).length();
  double newlength = gamma(a,B);
  
  double ratio = gsl_ran_gamma_pdf(length,a,B)/gsl_ran_gamma_pdf(newlength,a,B);
  
  //---------- Construct proposed Tree ----------//
  
  Parameters P2 = P;
  P2.setlength(b,newlength);
  
  //--------- Do the M-H step if OK--------------//
  if (do_MH_move(A,P,P2,ratio)) {
    result.totals[0] = 1;
    result.totals[1] = std::abs(length - newlength);
    result.totals[2] = std::abs(log(newlength/length));
  }
  Stats.inc("branch-length (gamma)",result);
}

void change_branch_length(const alignment& A, Parameters& P,MoveStats& Stats,int b)
{
  double p = loadvalue(P.keys,"fraction_fit_gamma",0.01);
  if (myrandomf() < p)
    change_branch_length_fit_gamma(A, P, Stats, b);
  else if (myrandomf() < 0.5)
  {
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

