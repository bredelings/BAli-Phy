#ifndef STATISTICS_H
#define STATISTICS_H

#include <cmath>
#include <valarray>
#include <vector>

namespace statistics {

  inline double moment(const std::valarray<double>& v,int m) {
    double total = 0;
    for(int i=0;i<v.size();i++)
      total += std::pow(v[i],m);
    return total/v.size();
  }

  inline double average(const std::valarray<double>& v) {
    return moment(v,1);
  }

  double Var(const std::valarray<double>& v);

  unsigned count(const std::valarray<bool>& v);

  double fraction(const std::valarray<bool>& v);

  double Pr(const std::valarray<bool>& v);

  double odds(const std::valarray<bool>& v);

  inline double odds(double P) {
    return P/(1.0-P);
  }

  double log_odds(const std::valarray<bool>& v);

  std::vector<double> confidence_interval(const std::valarray<double>& values, double P);
  double lower_confidence_bound(const std::valarray<double>& values, double P);
  double upper_confidence_bound(const std::valarray<double>& values, double P);
}
#endif
