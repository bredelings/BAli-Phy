#ifndef PROBABILITY_H
#define PROBABILITY_H

#include <valarray>

double dirichlet_log_pdf(const std::valarray<double>& p,const std::valarray<double>& n);
double dirichlet_log_pdf(const std::valarray<double>& p,const std::valarray<double>& q
			 ,double N);

#endif
