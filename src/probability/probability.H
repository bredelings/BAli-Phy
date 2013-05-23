/*
   Copyright (C) 2004-2006,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef PROBABILITY_H
#define PROBABILITY_H

#include <vector>
#include <valarray>
#include "math/log-double.H"

log_double_t dirichlet_pdf(const std::vector<double>& p,const std::vector<double>& n);

log_double_t dirichlet_pdf(const std::valarray<double>& p,const std::valarray<double>& n);
log_double_t dirichlet_pdf(const std::valarray<double>& p,double N,const std::valarray<double>& q);
log_double_t dirichlet_pdf(const std::valarray<double>& p,double N);

std::valarray<double> safe_count(std::valarray<double> n);

log_double_t dirichlet_safe_pdf(const std::valarray<double>& p,const std::valarray<double>& n);
log_double_t dirichlet_safe_pdf(const std::valarray<double>& p,double N,const std::valarray<double>& q);

log_double_t exp_exponential_pdf(double y,double mu,double delta=0.0);

log_double_t uniform_pdf(double x, double a, double b);

log_double_t exponential_pdf(double x, double mu);

log_double_t laplace_pdf(double x, double mu, double sigma);

log_double_t cauchy_pdf(double x, double l=0.0, double s=1.0);

log_double_t beta_pdf(double p,double a, double b);

double beta_quantile(double p, double a, double b);

log_double_t gamma_pdf(double y, double a, double b);

double gamma_quantile(double p, double a, double b);

double gamma_quantile_no_approx(double y, double a, double b);

log_double_t normal_pdf(double y, double mu, double sigma);

double normal_quantile(double p, double mu, double sigma);

log_double_t log_normal_pdf(double y, double mu, double sigma);

double log_normal_quantile(double p, double mu, double sigma);

log_double_t cauchy_pdf(double y, double a, double b);

log_double_t binomial_pdf(int total, int n, double p);

log_double_t geometric_pdf(double p, int n);

// -------- Topology counting functions ------ //
log_double_t num_branches(int n);
log_double_t num_topologies(int n);
log_double_t num_topologies_in_partition(int n1, int n2);

#endif
