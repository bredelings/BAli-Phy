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
#endif

