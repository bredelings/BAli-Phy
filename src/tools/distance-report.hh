#ifndef DISTANCE_REPORT_H
#define DISTANCE_REPORT_H

#include <string>
#include <valarray>
#include "util/matrix.hh"

void diameter(const matrix<double>& D, const std::string& name, double interval_probability,
              bool show_mean, bool show_median, bool show_minmax, bool directed = false);
void report_distances(const std::valarray<double>& distances, const std::string& name,
                      double interval_probability, bool show_mean, bool show_median, bool show_minmax);
void report_compare(const matrix<double>& D, int N1, int N2, double interval_probability,
                    bool show_mean, bool show_median, bool show_minmax, bool directed = false);

#endif
