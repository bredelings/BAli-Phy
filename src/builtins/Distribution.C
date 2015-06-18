#include <vector>
#include <valarray>
#include <string>
#include "computation/operation.H"
#include "computation/computation.H"
#include "computation/module.H"
#include "probability/probability.H"
#include "bounds.H"
#include "rng.H"
#include "vector_from_list.H"

using std::vector;
using std::string;
using std::valarray;

extern "C" closure builtin_function_exponential_density(OperationArgs& Args)
{
  double mu = Args.evaluate(0).as_double();
  double x = Args.evaluate(1).as_double();
  
  return { exponential_pdf(x,mu) };
}

extern "C" closure builtin_function_sample_exponential(OperationArgs& Args)
{
  double mu = Args.evaluate_(0).as_double();
  
  return { exponential(mu) };
}

extern "C" closure builtin_function_gamma_density(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double x = Args.evaluate(2).as_double();
  
  return { gamma_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_gamma(OperationArgs& Args)
{
  double a1 = Args.evaluate_(0).as_double();
  double a2 = Args.evaluate_(1).as_double();
  
  return { gamma(a1, a2) };
}

extern "C" closure builtin_function_gamma_quantile(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double p  = Args.evaluate(2).as_double();

  return { gamma_quantile(p, a1, a2) };
}

extern "C" closure builtin_function_beta_density(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double x  = Args.evaluate(2).as_double();
  
  return { beta_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_beta(OperationArgs& Args)
{
  double a1 = Args.evaluate_(0).as_double();
  double a2 = Args.evaluate_(1).as_double();
  
  return { beta(a1, a2) };
}

extern "C" closure builtin_function_beta_quantile(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double p  = Args.evaluate(2).as_double();
  
  return { beta_quantile(p, a1, a2) };
}

extern "C" closure builtin_function_normal_density(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double x  = Args.evaluate(2).as_double();
  
  return { normal_pdf(x, a1, a2) };
}
 
extern "C" closure builtin_function_sample_normal(OperationArgs& Args)
{
  double a1 = Args.evaluate_(0).as_double();
  double a2 = Args.evaluate_(1).as_double();
  
  return { gaussian(a1, a2) };
}
 
extern "C" closure builtin_function_normal_quantile(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double p  = Args.evaluate(2).as_double();

  return { normal_quantile(p, a1 ,a2) };
}

extern "C" closure builtin_function_cauchy_density(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double x  = Args.evaluate(2).as_double();

  return { cauchy_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_cauchy(OperationArgs& Args)
{
  double a1 = Args.evaluate_(0).as_double();
  double a2 = Args.evaluate_(1).as_double();

  return { cauchy(a1, a2) };
}

// First convert N from tuple to list...
// Second convert this builtin routine to just take two Vector<double> arguments.
// Third convert all the expression_ref's here to "var" and use Distribution_Functions()
extern "C" closure builtin_function_dirichlet_density(OperationArgs& Args)
{
  auto n = Args.evaluate(0);
  auto x = Args.evaluate(1);
  
  return { ::dirichlet_pdf(x.as_<Vector<double>>(), n.as_<Vector<double>>()) };
}

extern "C" closure builtin_function_laplace_density(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double x  = Args.evaluate(2).as_double();
  
  return { ::laplace_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_laplace(OperationArgs& Args)
{
  double m = Args.evaluate_(0).as_double();
  double s = Args.evaluate_(1).as_double();
  
  return { laplace(m,s) };
}

extern "C" closure builtin_function_uniform_density(OperationArgs& Args)
{
  double a1 = Args.evaluate(0).as_double();
  double a2 = Args.evaluate(1).as_double();
  double x  = Args.evaluate(2).as_double();
  
  return { ::uniform_pdf(x,a1,a2) };
}

extern "C" closure builtin_function_sample_uniform(OperationArgs& Args)
{
  double a1 = Args.evaluate_(0).as_double();
  double a2 = Args.evaluate_(1).as_double();

  assert(a1 < a2);

  return { a1 + (a2-a1)*uniform() };
}

extern "C" closure builtin_function_binomial_density(OperationArgs& Args)
{
  int n = Args.evaluate(0).as_int();
  double p = Args.evaluate(1).as_double();
  int k = Args.evaluate(2).as_int();
  
  return { ::binomial_pdf(n,p,k) };
}

extern "C" closure builtin_function_sample_binomial(OperationArgs& Args)
{
  int n = Args.evaluate_(0).as_int();
  double p = Args.evaluate_(1).as_double();

  return { (int)binomial(n,p) };
}

extern "C" closure builtin_function_sample_bernoulli(OperationArgs& Args)
{
  double p = Args.evaluate_(0).as_double();

  return { (int)bernoulli(p) };
}

extern "C" closure builtin_function_geometric_density(OperationArgs& Args)
{
  double p_fail = Args.evaluate(0).as_double();
  double p_success = Args.evaluate(1).as_double();
  int n = Args.evaluate(2).as_int();
  
  return { ::geometric_pdf(p_fail, p_success, n) };
}

extern "C" closure builtin_function_sample_geometric(OperationArgs& Args)
{
  double p = Args.evaluate_(0).as_double();

  return { (int)geometric(p) };
}

extern "C" closure builtin_function_poisson_density(OperationArgs& Args)
{
  double mu = Args.evaluate(0).as_double();
  int n = Args.evaluate(1).as_int();
  
  return { poisson_pdf(mu,n) };
}

extern "C" closure builtin_function_sample_poisson(OperationArgs& Args)
{
  double mu = Args.evaluate_(0).as_double();

  return { (int)poisson(mu) };
}


log_double_t CRP_pdf(const double alpha, int N, int D, const vector<int>& z)
{
  if (z.size() != N) return 0.0;

  log_double_t Pr = 1;

  // 1. Determine probability of the unlabelled pattern
  vector<int> counts(N+D,0);
  int n_types = 0;
  for(int i=0;i<z.size();i++)
  {
    assert(z[i] >=0 and z[i] < N+D);
    int& count = counts[z[i]];
    if (count > 0)
      Pr *= double(count)/(i+alpha);
    else
    {
      if (i > 0)
	Pr *= (alpha/(i+alpha));
      n_types++;
    }
    count++;
  }

  // 2. Determine the probability of the labelling
  for(int i=0;i<n_types;i++)
    Pr /= double(N+D-i);

  return Pr;
}

// This is the Chinese Restaurant Process density for N observations, N+Delta values, and parameter alpha.
// CRP(alpha,N,Delta)
// The final argument is z, which is a list of N integers.

extern "C" closure builtin_function_CRP_density(OperationArgs& Args)
{
  // ?? assert(not Args.evaluate_changeables());

  //------------- 1. Get arguments alpha, N, D -----------------
  double alpha = Args.evaluate(0).as_double();
  int N = Args.evaluate(1).as_int();
  int D = Args.evaluate(2).as_int();

  //------------- 2. Get argument Z -----------------
  vector<int> z = get_vector_from_list<int,Int>(Args,3);

  return { ::CRP_pdf(alpha,N,D,z) };
}

#include "probability/choose.H"
#include "util.H"

extern "C" closure builtin_function_sample_CRP(OperationArgs& Args)
{
  // ?? assert(not Args.evaluate_changeables());

  //------------- 1. Get arguments alpha, N, D -----------------
  double alpha = Args.evaluate_(0).as_double();
  int N = Args.evaluate_(1).as_int();
  int D = Args.evaluate_(2).as_int();

  // The entries in [0,n_seen) are the categories we've seen
  vector<int> categories = iota(N+D);

  // These are the counts of the seen categories, followed by alpha
  vector<double> counts = {alpha};

  // The number of categories we've seen so far.
  int n_seen=0;

  // The series of sampled categories
  object_ptr<EVector> S (new EVector);

  for(int i=0;i<N;i++)
  {
    // Choose from the seen categories, or the possibilty we should pick a new category
    int index = choose(counts);

    // If we've chosen a category not seen before..
    if (index == n_seen)
    {
      // .. then select an unsend category.
      int a_index = n_seen + uniform(0,N+D-n_seen-1);

      // Put the chosen category next in our list of seen categories
      if (n_seen < a_index)
	std::swap(categories[n_seen], categories[a_index]);
      n_seen++;

      // Give the category 0 counts, and mark the next unseen one with weight alpha
      counts.back() = 0.0;
      counts.push_back(alpha);
    }

    assert(index < n_seen);

    // The index we've chosen is now valid.
    counts[index] += 1.0;
    S->push_back(categories[index]);
  }

  return S;
}

extern "C" closure builtin_function_sample_categorical(OperationArgs& Args)
{
  //------------- 1. Get argument p -----------------
  vector<double> z = get_vector_from_list_<double,Double>(Args,0);

  return { choose_scratch(z) };
}
