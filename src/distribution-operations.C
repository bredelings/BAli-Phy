#include "distribution-operations.H"

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( data_function("prob_density",2) );

expression_ref exponential_dist = prob_density("Exponential", exponential_density());

expression_ref gamma_dist       = prob_density("Gamma", gamma_density());

expression_ref beta_dist        = prob_density("Beta", beta_density());

expression_ref normal_dist      = prob_density("Normal", normal_density());

expression_ref log_normal_dist  = prob_density("LogNormal", log_normal_density());

expression_ref cauchy_dist      = prob_density("Cauchy", cauchy_density());

expression_ref dirichlet_dist   = prob_density("Dirichlet", dirichlet_density());

expression_ref laplace_dist     = prob_density("Laplace", laplace_density());

expression_ref log_laplace_dist = prob_density("LogLaplace", log_laplace_density());

expression_ref uniform_dist     = prob_density("Uniform", uniform_density());

