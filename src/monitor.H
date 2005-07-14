#ifndef MONITOR_H
#define MONITOR_H

#include <iostream>
#include "alignment.H"
#include "parameters.H"
#include <string>
#include "model.H"
#include "smodel.H"

void print_stats(std::ostream& o,std::ostream& trees,std::ostream& pS,std::ostream& pI,
		 const alignment& A,const Parameters& P,const std::string& tag,bool=true);

void show_frequencies(std::ostream& o,const alphabet& a,const std::valarray<double>&);
void show_frequencies(std::ostream& o,const substitution::MultiModel& MModel);
void show_parameters(std::ostream& o,const Model& M);
void report_mem();

#endif
