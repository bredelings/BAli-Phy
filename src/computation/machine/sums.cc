#include "graph_register.H"
#include "effects.H"
#include "util/math/neumaier.H"

long total_context_prior = 0;
long total_context_like = 0;

log_double_t reg_heap::prior_for_context(int c)
{
    total_context_prior++;

    evaluate_program(c);
    reroot_at_context(c);

    NeumaierMultiplier Pr;

    for(auto& [s, E]: prior_terms)
	Pr *= E.prob;

    return (log_double_t)Pr;
}

log_double_t reg_heap::likelihood_for_context(int c)
{
    total_context_like++;

    evaluate_program(c);
    reroot_at_context(c);

    NeumaierMultiplier Pr;
    for(auto& [s,E]: likelihood_terms)
	Pr *= E.prob;

    return (log_double_t)Pr;
}

