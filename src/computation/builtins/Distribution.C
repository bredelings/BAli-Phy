#include <vector>
#include <valarray>
#include <string>
#include "computation/operation.H"
#include "computation/computation.H"
#include "computation/module.H"
#include "probability/probability.H"
#include "bounds.H"
#include "rng.H"

using std::vector;
using std::string;
using std::valarray;

extern "C" closure builtin_function_exponential_density(OperationArgs& Args)
{
  double mu = *Args.evaluate_as<Double>(0);
  double x = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(x,mu);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_sample_exponential(OperationArgs& Args)
{
  double mu = *Args.evaluate_as_<Double>(0);
  
  Double result = exponential(mu);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_gamma_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x = *Args.evaluate_as<Double>(2);
  
  Log_Double result = gamma_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_sample_gamma(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  
  Double result = gamma(a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_gamma_quantile(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = gamma_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_beta_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = beta_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_sample_beta(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  
  Double result = beta(a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_beta_quantile(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);
  
  Double result = beta_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_normal_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = normal_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}
 
extern "C" closure builtin_function_sample_normal(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  
  Double result = gaussian(a1, a2);
  return object_ptr<const Object>(result.clone());
}
 
extern "C" closure builtin_function_normal_quantile(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = normal_quantile(p, a1 ,a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_cauchy_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  Log_Double result = cauchy_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_sample_cauchy(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);

  Double result = cauchy(a1, a2);
  return object_ptr<const Object>(result.clone());
}

// First convert N from tuple to list...
// Second convert this builtin routine to just take two Vector<double> arguments.
// Third convert all the expression_ref's here to "var" and use Distribution_Functions()
extern "C" closure builtin_function_dirichlet_density(OperationArgs& Args)
{
  object_ptr<const Vector<double>> n = Args.evaluate_as<Vector<double>>(0);
  object_ptr<const Vector<double>> x = Args.evaluate_as<Vector<double>>(1);
  
  object_ptr<Log_Double> R (new Log_Double( ::dirichlet_pdf(*x,*n) ) );
  
  return R;
}

extern "C" closure builtin_function_laplace_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x, a1, a2) ) );
}

extern "C" closure builtin_function_sample_laplace(OperationArgs& Args)
{
  double m = *Args.evaluate_as_<Double>(0);
  double s = *Args.evaluate_as_<Double>(1);
  
  Double result = laplace(m,s);
  return object_ptr<const Object> (result.clone());
}

extern "C" closure builtin_function_uniform_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::uniform_pdf(x,a1,a2) ) );
}

extern "C" closure builtin_function_sample_uniform(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as_<Double>(0);
  double a2 = *Args.evaluate_as_<Double>(1);

  assert(a1 < a2);

  Double result = a1 + (a2-a1)*uniform();
  return object_ptr<const Object> (result.clone());
}

extern "C" closure builtin_function_binomial_density(OperationArgs& Args)
{
  int n = *Args.evaluate_as<Int>(0);
  double p = *Args.evaluate_as<Double>(1);
  int k = *Args.evaluate_as<Int>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::binomial_pdf(n,p,k) ) );
}

extern "C" closure builtin_function_sample_binomial(OperationArgs& Args)
{
  int n = *Args.evaluate_as<Int>(0);
  double p = *Args.evaluate_as<Double>(1);

  Int result = binomial(n,p);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_sample_bernoulli(OperationArgs& Args)
{
  double p = *Args.evaluate_as<Double>(0);

  Int result = bernoulli(p);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_geometric_density(OperationArgs& Args)
{
  double p = *Args.evaluate_as<Double>(0);
  double n = *Args.evaluate_as<Int>(1);
  
  return object_ptr<Log_Double> (new Log_Double( ::geometric_pdf(p,n) ) );
}

extern "C" closure builtin_function_sample_geometric(OperationArgs& Args)
{
  double p = *Args.evaluate_as<Double>(0);

  Int result = geometric(p);
  return object_ptr<const Object>(result.clone());
}


extern "C" closure builtin_function_sample_poisson(OperationArgs& Args)
{
  double mu = *Args.evaluate_as<Double>(0);

  Int result = poisson(mu);
  return object_ptr<const Object>(result.clone());
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
  double alpha = *Args.evaluate_as<Double>(0);
  int N = *Args.evaluate_as<Int>(1);
  int D = *Args.evaluate_as<Int>(2);

  //------------- 2. Get argument Z -----------------
  vector<int> z;
  const closure* top = &Args.evaluate_slot_to_closure(3);
  while(top->exp->size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp->size() == 2);

    int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
    int next_reg = top->lookup_in_env( next_index );

    // Add the element to the list.
    z.push_back( *convert<const Int>(Args.evaluate_reg_to_object(element_reg)) );

    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));

  return object_ptr<Log_Double> (new Log_Double( ::CRP_pdf(alpha,N,D,z) ) );
}
