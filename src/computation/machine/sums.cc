#include "graph_register.H"
#include "effects.H"
#include "kahan.H"

long total_context_prior = 0;
long total_context_like = 0;

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

    // What if error is NaN?
    if (std::abs(error) < 1.0e-9)
    {
	C = error;
	y = new_y;
	total_error += std::abs(error);
	return true;
    }
    else
	return false;
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

log_double_t reg_heap::prior_for_context(int c)
{
    total_context_prior++;

    evaluate_program(c);
    reroot_at_context(c);

    /*
      This version doesn't really change the amount of time in incremental_evaluate.
      However, it drastically increases the amount of time spent in reg_has_value( 30% ),
      get_reg_value_in_context( 13% ), and prior_for_context( 3% ).

      With those removed, this could be comparable, or even faster.
    */

    double log_pr = 0.0;
    double C = 0.0;

    /*
     * NOTE: Evaluating a PDF should NOT cause the creation of any new
     *       random variables, since all the parameters should be forced by the sampling
     *       operation.
     *
     *       This ensures that a range-for is OK. (Range-for's assume that begin() and end() do not change).
     */

    for(auto [s, _]: prior_terms)
    {
        auto r_pdf_effect = steps[s].call;
	auto X = value_for_precomputed_reg(r_pdf_effect).exp.as_<::register_prior>().pdf;

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

log_double_t reg_heap::likelihood_for_context(int c)
{
    total_context_like++;

    /*
      This version doesn't really change the amount of time in incremental_evaluate.
      However, it drastically increases the amount of time spent in reg_has_value( 30% ),
      get_reg_value_in_context( 13% ), and likelihood_for_context( 3% ).

      With those removed, this could be comparable, or even faster.
    */
    evaluate_program(c);
    reroot_at_context(c);

    double log_pr = 0.0;
    double C = 0.0;
    for(auto [s,_]: likelihood_heads)
    {
        int r_likelihood_effect = steps[s].call;
	auto X = value_for_precomputed_reg(r_likelihood_effect).exp.as_<register_likelihood>().likelihood;

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

