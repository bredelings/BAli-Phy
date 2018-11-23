#include "graph_register.H"
#include "kahan.H"

inline bool kahan_add(double x, double& y, double& C, double& total_error)
{
    double new_y = 0;
    double error = 0;
    if (std::abs(x) > std::abs(y))
    {
	new_y = (y - C) + x;
	double a1 = new_y - x;
	double a2 = a1 - y;
	error = a2 + C;
    }
    else
    {
	new_y = y + (x - C);
	double a1 = new_y - y;
	double a2 = a1 - x;
	error = a2 + C;
    }

    if (std::abs(error) > 1.0e-9)
	return false;
    else
    {
	C = error;
	y = new_y;
	total_error += std::abs(error);
	return true;
    }
}

bool kahan_adder::operator+=(double x)
{
    if (kahan_add(x, value, delta, total_error))
	return true;
    else
    {
	unhandled += x;
	return false;
    }
}

bool reg_heap::inc_probability(int rc)
{
  assert(rc > 0);
  int r2 = results[rc].value;
  assert(r2 > 0);
  log_double_t pr = regs.access(r2).C.exp.as_log_double();

  if (kahan_add(pr.log(), prior.data.value, prior.data.delta, prior.data.total_error))
  {
      results[rc].flags = 1;
      return true;
  }
  else
  {
      prior.data.unhandled += pr.log();
      return false;
  }
}

log_double_t reg_heap::probability_for_context_full(int c)
{
  /*
    This version doesn't really change the amount of time in incremental_evaluate.
    However, it drastically increases the amount of time spent in reg_has_value( 30% ),
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

