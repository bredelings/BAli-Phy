#include "distribution-operations.H"

expression_ref exponential_dist = prob_density("Exponential",log_laplace_density());

expression_ref gamma_dist       = prob_density("Gamma",log_laplace_density());

expression_ref beta_dist        = prob_density("Beta",log_laplace_density());

expression_ref normal_dist      = prob_density("Normal",log_laplace_density());

expression_ref log_normal_dist  = prob_density("LogNormal",log_laplace_density());

expression_ref cauchy_dist      = prob_density("Cauchy",log_laplace_density());

expression_ref dirichlet_dist   = prob_density("Dirichlet",log_laplace_density());

expression_ref laplace_dist     = prob_density("Laplace",log_laplace_density());

expression_ref log_laplace_dist = prob_density("LogLaplace",log_laplace_density());

expression_ref uniform_dist     = prob_density("Uniform",log_laplace_density());

