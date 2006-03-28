#ifndef STATISTICS_H
#define STATISTICS_H

#include <cmath>
#include <valarray>
#include <vector>

namespace statistics {

  inline double moment(const std::vector<double>& v,int m) {
    double total = 0;
    for(int i=0;i<v.size();i++)
      total += std::pow(v[i],m);
    return total/v.size();
  }

  inline double moment(const std::valarray<double>& v,int m) {
    double total = 0;
    for(int i=0;i<v.size();i++)
      total += std::pow(v[i],m);
    return total/v.size();
  }

  inline double average(const std::vector<double>& v) {
    return moment(v,1);
  }

  inline double average(const std::valarray<double>& v) {
    return moment(v,1);
  }

  inline double Var(const std::vector<double>& v) {
    double m1 = moment(v,1);
    double m2 = moment(v,2);
    return m2 - m1*m1;
  }

  inline double Var(const std::valarray<double>& v) {
    double m1 = moment(v,1);
    double m2 = moment(v,2);
    return m2 - m1*m1;
  }

  inline unsigned count(const std::valarray<bool>& v) {
    unsigned sum=0;
    for(unsigned i=0;i<v.size();i++)
      if (v[i]) sum++;
    return sum;
  }

  double fraction(const std::valarray<bool>& v);

  inline double fraction(unsigned n, unsigned N, unsigned pseudocount)
  {
    return double(n+pseudocount)/(N+2*pseudocount);
  }

  double Pr(const std::valarray<bool>& v);

  double odds(const std::valarray<bool>& v);

  inline double odds(double P) {
    return P/(1.0-P);
  }

  inline double odds(unsigned n, unsigned N, unsigned pseudocount)
  {
    return double(n+pseudocount)/(N-n+pseudocount);
  }

  double log_odds(const std::valarray<bool>& v);

  double median(const std::valarray<double>& values);
  double quantile(const std::valarray<double>& values, double Q);

  std::pair<double,double> confidence_interval(const std::valarray<double>& values, double P);

  std::valarray<bool> add_pseudocount(const std::valarray<bool>& sample1,int pseudocount);

  std::vector<int> total_times(const std::valarray<bool>& v);
  std::vector<int> regeneration_times(const std::valarray<bool>& v);

  std::vector<double> autocovariance(const std::valarray<double>& x,unsigned max);

  std::vector<double> autocorrelation(const std::valarray<double>& x,unsigned max);
}
#endif
