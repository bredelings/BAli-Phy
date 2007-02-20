#ifndef MONITOR_H
#define MONITOR_H

#include <iostream>
#include "parameters.H"
#include <string>
#include "model.H"
#include "smodel.H"

void print_stats(std::ostream& o,std::ostream& trees,const Parameters& P,bool=true);

void show_frequencies(std::ostream& o,const alphabet& a,const std::valarray<double>&);
void show_frequencies(std::ostream& o,const substitution::MultiModel& MModel);
void report_mem();

#endif
