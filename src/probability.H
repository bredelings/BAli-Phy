#ifndef PROBABILITY_H
#define PROBABILITY_H

#include <valarray>

double dirichlet_log_pdf(const std::valarray<double>& p,const std::valarray<double>& n);
double dirichlet_log_pdf(const std::valarray<double>& p,const std::valarray<double>& q
			 ,double N);

double exp_exponential_log_pdf(double y,double mu,double delta=0.0);
double exp_exponential_pdf(double y,double mu,double delta=0.0);

double exponential_log_pdf(double x, double mu);
double exponential_pdf(double x, double mu);

double shift_laplace_pdf(double x, double mu, double sigma);

// -------- Topology counting functions ------ //
double log_num_branches(int n);
double log_num_topologies(int n);
double log_num_topologies_in_partition(int n1, int n2);

#endif
