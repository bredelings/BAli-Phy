#ifndef CHOOSE_H
#define CHOOSE_H

#include <vector>

int choose2(double, double, double T=1.0);
int choose3(double, double, double,double T=1.0);

int choose(const std::vector<double>&, double T=1.0);
// int choose(std::vector<double>::const_iterator,int);


double choose3_P(int,double, double, double, double T=1.0);

double choose_P(int, const std::vector<double>&, double T=1.0);

int choose_nonlog(const std::vector<double>&);

#include "log-double.H"
#include "le-double.H"

int choose(const std::vector<log_double_t>&,double T=1.0);
log_double_t choose_P(int,const std::vector<log_double_t>&,double T=1.0);

int choose(const std::vector<le_double_t>&);
int choose(const std::vector<le_double_t>&,double T);

le_double_t choose_P(int,const std::vector<le_double_t>&);
le_double_t choose_P(int,const std::vector<le_double_t>&,double T);

#endif

