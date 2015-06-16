#include "graph_register.H"

bool reg_heap::inc_probability(int rc)
{
  assert(rc > 0);
  int r2 = computations[rc].result;
  assert(r2 > 0);
  log_double_t pr = access(r2).C.exp.as_log_double();

  log_double_t new_total;
  double error;
  if (std::abs(pr.log()) > std::abs(variable_pr.log()))
  {
    new_total = (variable_pr / error_pr) * pr;
    double a1 = new_total.log() - pr.log();
    double a2 = a1 - variable_pr.log();
    error = a2 + error_pr.log();
  }
  else
  {
    new_total = variable_pr * (pr / error_pr);
    double a1 = new_total.log() - variable_pr.log();
    double a2 = a1 - pr.log();
    error = a2 + error_pr.log();
  }
    
  if (std::abs(error) > 1.0e-9)
  {
    unhandled_pr *= pr;
    return false;
  }

  total_error += std::abs(error);
  error_pr.log() = error;
  variable_pr = new_total;
  computations[rc].flags = 1;
  return true;
}

log_double_t reg_heap::probability_for_context_full(int c)
{
  /*
    This version doesn't really change the amount of time in incremental_evaluate.
    However, it drastically increases the amount of time spent in reg_has_result( 30% ),
    get_reg_value_in_context( 13% ), and probability_for_context( 3% ).

    With those removed, this could be comparable, or even faster.
  */

  double log_pr = 0.0;
  double C = 0.0;
  for(int r: probability_heads)
  {
    const auto& x = get_reg_value_in_context(r, c);
    log_double_t X = x.as_log_double();

    double t;
    if (std::abs(X.log()) > std::abs(log_pr))
    {
      t = (log_pr - C) + X.log();
      double a1 = t - X.log();
      double a2 = a1 - log_pr;
      C = a2 + C;
    }
    else
    {
      t = log_pr + (X.log() - C);
      double a1 = t - log_pr;
      double a2 = a1 - X.log();
      C = a2 + C;
    }
    log_pr = t;
  }
  log_double_t Pr;
  Pr.log() = log_pr;
  return Pr;
}

