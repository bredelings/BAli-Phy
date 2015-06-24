#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "rng.H"
#include "util.H"
#include "probability/choose.H"

using boost::dynamic_pointer_cast;
using namespace std;

// The idea here is to propose new values of X, and evaluate them by summing over each Y_i \in {True,False}.
// Then the Y_i are resampled.  

double log1pexp(double x)
{
  if (x < 18.0)
    return log1p(exp(x));
  else if (x < 33.3)
    return x + exp(-x);
  else
    return x;
}

extern "C" closure builtin_function_sum_out_coals(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  //------------- 1a. Get argument X -----------------
  int R_X = Args.evaluate_slot_to_reg(0);

  int c = Args.evaluate(2).as_int();

  //------------- 1b. Get arguments Y_i  -----------------
  vector<int> M_Y;

  int next_reg = Args.reg_for_slot(1);
  const closure* top = &M.lazy_evaluate(next_reg, c);
  while(top->exp.size())
  {
    assert(has_constructor(top->exp,":"));
    assert(top->exp.size() == 2);

    int element_index = top->exp.sub()[0].as_<index_var>().index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = top->exp.sub()[1].as_<index_var>().index;
    next_reg = top->lookup_in_env( next_index );

    // evaluate the list element in token 0
    element_reg = Args.evaluate_reg_to_reg(element_reg);

    // Add the element to the list.
    M_Y.push_back( element_reg );
    // Move to the next element or end
    top = &M.lazy_evaluate(next_reg, c);
  }
  assert(has_constructor(top->exp,"[]"));

  //------------- 2. Figure out t and the next t ------------//

  int x1 = M.lazy_evaluate(R_X, c).exp.as_int();
  int x2 = x1 + 1;
  if (uniform() < 0.5)
  {
    x2 = x1 - 1;
    if (x2 < 0)
      x2 = 0;
  }

  //------------- 3. Record base probability and relative probability for x1
  
  for(int R: M_Y)
    M.set_reg_value_in_context(R, expression_ref(0), c);

  log_double_t pr_base_1 = M.probability_for_context(c);

  log_double_t pr_total_1 = pr_base_1;
  vector<log_double_t> pr_y_1(M_Y.size());
  for(int i=0;i<M_Y.size();i++)
  {
    int R = M_Y[i];
    M.set_reg_value_in_context(R, expression_ref(1), c);
    log_double_t pr_offset = M.probability_for_context(c);
    M.set_reg_value_in_context(R, expression_ref(0), c);
    double delta = log(pr_offset/pr_base_1);
    pr_y_1[i] = exp<log_double_t>(-log1pexp(delta));
    
    pr_total_1 /= pr_y_1[i];
  }

  //------------- 4. Record base probability and relative probability for x2

  M.set_reg_value_in_context(R_X, expression_ref(x2), c);

  log_double_t pr_base_2 = M.probability_for_context(c);

  log_double_t pr_total_2 = pr_base_2;
  vector<log_double_t> pr_y_2(M_Y.size());
  for(int i=0;i<M_Y.size();i++)
  {
    int R = M_Y[i];
    M.set_reg_value_in_context(R, expression_ref(1), c);
    log_double_t pr_offset = M.probability_for_context(c);
    M.set_reg_value_in_context(R, expression_ref(0), c);
    double delta = log(pr_offset/pr_base_2);
    pr_y_2[i] = exp<log_double_t>(-log1pexp(delta));
    
    pr_total_2 /= pr_y_2[i];
  }

  //------------- 5. Choose to accept or not, depending on the relative probabilities.
  int choice = choose2(pr_total_1, pr_total_2);

  //------------- 6. Set x depending on the choice
  if (choice == 0)
    M.set_reg_value_in_context(R_X, expression_ref(x1), c);
    
  //------------- 7. Sample the Y[i] depending on the choice.
  vector<log_double_t> pr_y = choice?pr_y_2:pr_y_1;

  for(int i=0;i<M_Y.size();i++)
  {
    int R = M_Y[i];
    double pr = 1.0 - double(pr_y[i]);
    if (uniform() < pr)
      M.set_reg_value_in_context(R, expression_ref(1), c);
  }

  return constructor("()",0);
}

// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_gibbs_sample_categorical(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  //------------- 1a. Get argument X -----------------
  int R_X = Args.evaluate_slot_to_reg(0);

  //------------- 1b. Get range [0,n) for X ----------
  int n = Args.evaluate(1).as_int();

  //------------- 1c. Get context index --------------
  int c = Args.evaluate(2).as_int();

  //------------- 2. Figure out probability of each value of x ------------//
  vector<log_double_t> pr_x(n);
  for(int i=0;i<pr_x.size();i++)
  {
    M.set_reg_value_in_context(R_X, expression_ref(i), c);

    pr_x[i] = M.probability_for_context(c);
  }

  //------------- 4. Record base probability and relative probability for x2

  int x2 = choose(pr_x);

  M.set_reg_value_in_context(R_X, expression_ref(x2), c);

  return constructor("()",0);
}

extern "C" closure builtin_function_register_transition_kernel(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int R = Args.reg_for_slot(0);

  auto& M = Args.memory();

  M.add_transition_kernel(R);

  return constructor("()",0);
}

