#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "rng.H"
#include "util.H"

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
  int M_X = is_a<modifiable>(M.access(R_X).C.exp)->index;

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

    int m_index = is_a<modifiable>(M.access(element_reg).C.exp)->index;

    // Add the element to the list.
    M_Y.push_back( m_index );
    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));

  //------------- 1c. Get index for probability expression -----------------
  int H_Pr = *convert<const Int>(Args.evaluate_reg_to_closure(R_X,true).exp->head);
  int R_Pr = M.get_heads_for_context(token)[H_Pr];

  //------------- 2. Figure out t and the next t ------------//

  double x1 = *convert<const Double>(Args.evaluate_reg_to_closure(R_X,true).exp->head);
  double x2 = x1 + gaussian(0,1.0);
  x2 = reflect_more_than(x2, 0.0);

  //------------- 3. Record base probability and relative probability for x
  
  for(int m: M_Y)
  {
    int R = M.get_modifiable_regs_for_context(token)[m];
    Args.memory().set_reg_value(R, {constructor("Prelude.False",0),{}}, token);
  }

  R_Pr = M.get_heads_for_context(token)[H_Pr];
  log_double_t pr_base_1 = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);

  log_double_t pr_total_1 = pr_base_1;
  vector<log_double_t> pr_y_0(M_Y.size());
  for(int i=0;i<M_Y.size();i++)
  {
    int R = M.get_modifiable_regs_for_context(token)[M_Y[i]];
    Args.memory().set_reg_value(R, {constructor("Prelude.True",0),{}}, token);
    R_Pr = M.get_heads_for_context(token)[H_Pr];
    log_double_t pr_offset = *convert<const Log_Double>(Args.evaluate_reg_to_closure(R_Pr,true).exp->head);
    Args.memory().set_reg_value(R, {constructor("Prelude.False",0),{}}, token);
    double delta = log(pr_offset/pr_base_1);
    pr_y_0[i] = exp<log_double_t>(-log1pexp(delta));
    
    pr_total_1 /= pr_y_0[i];
  }

  return constructor("()",0);
}
