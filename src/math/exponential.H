/*
   Copyright (C) 2004-2007, 2010, 2012, 2014 Benjamin Redelings

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

///
/// \file exponential.H
///
/// \brief Defines function for computing matrix exponentials.
///



#ifndef EXPONENTIAL_H
#define EXPONENTIAL_H
#include "util/matrix.H"
#include <Eigen/Dense>


typedef Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> EigenValues;

Matrix exp(const EigenValues& eigensystem,const std::vector<double>& D,double t);
Eigen::MatrixXd exp(const Eigen::MatrixXd& Q, double t);

double rate_away(const std::vector<double>& pi, const Eigen::MatrixXd& Q);

double positivize_and_renormalize_matrix(Matrix& E);
double positivize_and_renormalize_matrix(Eigen::MatrixXd& E);

Eigen::MatrixXd toEigen(const Matrix& Q);
Matrix fromEigen(const Eigen::MatrixXd& EM);

std::vector<double> compute_stationary_freqs(const Matrix& Q);
bool checkStationary(const Eigen::MatrixXd& Q, const Eigen::VectorXd& pi, double tol = 1.0e-9);
bool checkStationary(const Matrix& Q, const std::vector<double>& pi, double tol = 1.0e-9);

bool checkReversible(const Eigen::MatrixXd& Q, const Eigen::VectorXd& pi, double tol = 1.0e-9);
bool checkReversible(const Matrix& Q, const std::vector<double>& pi, double tol = 1.0e-9);

Eigen::VectorXd     equilibriumLimit(const Eigen::VectorXd& pi0, const Eigen::MatrixXd& Q);
std::vector<double> equilibriumLimit(const std::vector<double>& pi0, const Matrix& Q);

#endif
