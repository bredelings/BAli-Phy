#ifndef ML_H
#define ML_H

#include "alignment.H"
#include "parameters.H"

void ML(alignment&,Parameters&,int max,
	double probability(const alignment&,const Parameters&) );
#endif
