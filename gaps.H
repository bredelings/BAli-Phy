#ifndef GAPS_H
#define GAPS_H

#include "mytypes.H"
#include "etree.H"
#include "alignment.H"

const double lambda_O = -16;
const double lambda_E = -4;

double prior(const alignment&,const tree&);
double prior_no_tree(const alignment&);

double prior_internal(const alignment&,const tree&);
#endif
