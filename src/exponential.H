#ifndef EXPONENTIAL_H
#define EXPONENTIAL_H
#include "mytypes.H"
#include "eigenvalue.H"

Matrix exp(const EigenValues& eigensystem,const BMatrix& D,const double t);
Matrix exp(const SMatrix& S,const BMatrix& D,const double t=1.0);
Matrix exp(const SMatrix& M,const double t=1.0);

Matrix gamma_exp(const SMatrix& S,const BMatrix& D,double alpha,double beta);
Matrix gamma_exp(const SMatrix& M,double alpha,double beta);

#endif
