#ifndef MCMC_H
#define MCMC_H

#include "alignment.H"
#include "parameters.H"

void MCMC(alignment&,Parameters&,int max,
	  double probability(const alignment&,const Parameters&) );
#endif
