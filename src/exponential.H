#ifndef EXPONENTIAL_H
#define EXPONENTIAL_H
#include "mytypes.H"
#include "eigenvalue.H"

Matrix exp(const EigenValues& eigensystem,const BMatrix& D,double t,double f=1.0);
Matrix exp(const SMatrix& S,const BMatrix& D,double t=1.0,double f=1.0);
Matrix exp(const SMatrix& M,const double t=1.0);

Matrix gamma_exp(const SMatrix& S,const BMatrix& D,double alpha,double beta);
Matrix gamma_exp(const SMatrix& M,double alpha,double beta);

#endif
