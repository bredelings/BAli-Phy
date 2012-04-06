/*
   Copyright (C) 2004-2007,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

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

  inline double odds_ratio(unsigned n1,unsigned n2,unsigned N,unsigned pseudocount) 
  {
    double o1 = odds(n1,N,pseudocount);
    double o2 = odds(n2,N,pseudocount);
    
    return o2/o1;
  }

  double log_odds(const std::valarray<bool>& v);

  double median(const std::vector<double>& values);
  double median(const std::valarray<double>& values);
  double quantile(const std::valarray<double>& values, double Q);
  double quantile(std::vector<double> values, double Q);
  double quantile_sorted(const std::vector<double>& values, double Q);
  double fraction_in_interval(std::vector<double> values,double L, double R);

  std::pair<double,double> confidence_interval(const std::valarray<double>& values, double P);
  std::pair<double,double> confidence_interval(std::vector<double> values, double P);

  std::valarray<bool> add_pseudocount(const std::valarray<bool>& sample1,int pseudocount);

  std::vector<int> total_times(const std::valarray<bool>& v);
  std::vector<int> regeneration_times(const std::valarray<bool>& v);

  std::vector<double> autocovariance(const std::vector<double>& x,unsigned max=0);

  std::vector<double> autocorrelation(const std::vector<double>& x,unsigned max=0);

  double autocorrelation_time(const std::vector<double>& x, unsigned max=0);

  double probability_x_less_than_y(const std::valarray<double>& x, const std::valarray<double>& y);

  inline double max(const std::valarray<double>& v)
  {
    double m=v[0];
    for(int i=1;i<v.size();i++)
      m = std::max(m,v[i]);
    return m;
  }

  inline double min(const std::valarray<double>& v)
  {
    double m=v[0];
    for(int i=1;i<v.size();i++)
      m = std::min(m,v[i]);
    return m;
  }

}
#endif
