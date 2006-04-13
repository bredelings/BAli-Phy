#ifndef PROPOSALS_H
#define PROPOSALS_H

#include <valarray>

double dirichlet_fiddle(std::valarray<double>& p,double N);

double dirichlet_fiddle(std::valarray<double>& p,double N,const std::valarray<bool>& mask);

double dirichlet_fiddle_old(std::valarray<double>& p,double sigma);

double dirichlet_fiddle_old(std::valarray<double>& p,double sigma,const std::valarray<bool>& mask);

double scale_gaussian(double& x,double sigma);

#endif
