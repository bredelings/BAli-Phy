#ifndef DISTANCE_REPORT_H
#define DISTANCE_REPORT_H

#include <boost/program_options.hpp>
#include <string>
#include <valarray>
#include "matrix.H"

void diameter(const matrix<double>& D,const std::string& name,boost::program_options::variables_map& args);
void report_distances(const std::valarray<double>& distances, const std::string& name, boost::program_options::variables_map& args);
void report_compare(boost::program_options::variables_map& args, const matrix<double>& D, int N1, int N2);

#endif
