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

  int token = Args.current_token();

  const reg_heap& M = Args.memory();

  //------------- 1a. Get argument X -----------------
  int R_X = Args.evaluate_slot_to_reg(0);

  //------------- 1b. Get arguments Y_i  -----------------
  vector<int> M_Y;

  const closure* top = &Args.evaluate_slot_to_closure(1);
  while(top->exp->size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp->size() == 2);

    int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
    int next_reg = top->lookup_in_env( next_index );

    // evaluate the list element
    element_reg = Args.evaluate_reg_to_reg(element_reg);

    // Add the element to the list.
    M_Y.push_back( element_reg );
    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));

  //------------- 1c. Get index for probability expression -----------------
  int H_Pr = *Args.evaluate_as<Int>(2);
  int R_Pr = M.get_heads()[H_Pr];

  //------------- 2. Figure out t and the next t ------------//

  int x1 = *convert<const Int>(Args.evaluate_reg_to_closure(R_X,true).exp->head);
  int x2 = x1 + 1;
  if (uniform() < 0.5)
  {
    x2 = x1 - 1;
    if (x2 < 0)
      x2 = 0;
  }

  //------------- 3. Record base probability and relative probability for x1
  
  for(int R: M_Y)
    Args.memory().set_reg_value(R, {constructor("Prelude.False",0),{}}, token);

  log_double_t pr_base_1 = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);

  log_double_t pr_total_1 = pr_base_1;
  vector<log_double_t> pr_y_1(M_Y.size());
  for(int i=0;i<M_Y.size();i++)
  {
    int R = M_Y[i];
    Args.memory().set_reg_value(R, {constructor("Prelude.True",0),{}}, token);
    log_double_t pr_offset = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);
    Args.memory().set_reg_value(R, {constructor("Prelude.False",0),{}}, token);
    double delta = log(pr_offset/pr_base_1);
    pr_y_1[i] = exp<log_double_t>(-log1pexp(delta));
    
    pr_total_1 /= pr_y_1[i];
  }

  //------------- 4. Record base probability and relative probability for x2

  Args.memory().set_reg_value(R_X, Int(x2), token);

  log_double_t pr_base_2 = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);

  log_double_t pr_total_2 = pr_base_2;
  vector<log_double_t> pr_y_2(M_Y.size());
  for(int i=0;i<M_Y.size();i++)
  {
    int R = M_Y[i];
    Args.memory().set_reg_value(R, {constructor("Prelude.True",0),{}}, token);
    log_double_t pr_offset = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);
    Args.memory().set_reg_value(R, {constructor("Prelude.False",0),{}}, token);
    double delta = log(pr_offset/pr_base_2);
    pr_y_2[i] = exp<log_double_t>(-log1pexp(delta));
    
    pr_total_2 /= pr_y_2[i];
  }

  //------------- 5. Choose to accept or not, depending on the relative probabilities.
  int choice = choose2(pr_total_1, pr_total_2);

  //------------- 6. Set x depending on the choice
  if (choice == 0)
    Args.memory().set_reg_value(R_X, Int(x1), token);
    
  //------------- 7. Sample the Y[i] depending on the choice.
  vector<log_double_t> pr_y = choice?pr_y_2:pr_y_1;

  for(int i=0;i<M_Y.size();i++)
  {
    int R = M_Y[i];
    double pr = 1.0 - double(pr_y[i]);
    if (uniform() < pr)
      Args.memory().set_reg_value(R, {constructor("Prelude.True",0),{}}, token);
  }

  return constructor("()",0);
}

// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_gibbs_sample_categorical(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  int token = Args.current_token();

  const reg_heap& M = Args.memory();

  //------------- 1a. Get argument X -----------------
  int R_X = Args.evaluate_slot_to_reg(0);

  //------------- 1b. Get arguments Y_i  -----------------
  int n = *Args.evaluate_as<Int>(1);

  //------------- 1c. Get index for probability expression -----------------
  int H_Pr = *Args.evaluate_as<Int>(2);
  int R_Pr = M.get_heads()[H_Pr];

  //------------- 2. Figure out probability of each value of x ------------//
  int x1 = *convert<const Int>(Args.evaluate_reg_to_closure(R_X,true).exp->head);

  vector<log_double_t> pr_x(n);
  for(int i=0;i<pr_x.size();i++)
  {
    Args.memory().set_reg_value(R_X, Int(i), token);
    log_double_t pr = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);
    pr_x[i] = pr;
  }

  //------------- 4. Record base probability and relative probability for x2

  int x2 = choose(pr_x);

  Args.memory().set_reg_value(R_X, Int(x2), token);

  return constructor("()",0);
}
