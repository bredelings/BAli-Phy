#ifndef EXPONENTIAL_H
#define EXPONENTIAL_H
#include "mytypes.H"

Matrix exp(const SMatrix& S,const BMatrix& D,const double t=1.0);
Matrix exp(const SMatrix& M,const double t=1.0);
#endif
