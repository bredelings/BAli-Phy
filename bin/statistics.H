#ifndef STATISTICS_H
#define STATISTICS_H

#include <cmath>
#include <valarray>

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

  double Pr(const std::valarray<bool>& v);

}
#endif
