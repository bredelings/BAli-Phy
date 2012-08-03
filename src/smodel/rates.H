/*
   Copyright (C) 2004-2007,2010 Benjamin Redelings

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

#ifndef RATES_H
#define RATES_H

#include <vector>
#include <cassert>
#include <cmath>
#include "model.H"
#include "distribution.H"

namespace substitution {

  using probability::DistributionWrapper;
  using probability::Distribution;

  /// A Gamma distribution for rate variation
  class Gamma: public DistributionWrapper<probability::Gamma>
  {
    void recalc(const std::vector<int>&);
  public:
    Gamma* clone() const {return new Gamma(*this);}
    efloat_t prior() const;
    Gamma();
  };

  /// A Beta distribution for rate variation
  class Beta: public DistributionWrapper<probability::Beta>
  {
    void recalc(const std::vector<int>&);
  public:
    Beta* clone() const {return new Beta(*this);}
    efloat_t prior() const;
    Beta();
  };


  class LogNormal: public DistributionWrapper<probability::LogNormal>
  {
    void recalc(const std::vector<int>&);
  public:
    LogNormal* clone() const {return new LogNormal(*this);}
    efloat_t prior() const;
    LogNormal();
  };


  /// A Uniform distribution for rate variation
  class Uniform: public DistributionWrapper<probability::Uniform>
  {
    void recalc(const std::vector<int>&);
  public:
    Uniform* clone() const {return new Uniform(*this);}
    Uniform();
  };

  /// A Discretization of a Distribution
  struct Discretization
  {
    std::vector<double> p;
    std::vector<double> r;
    
    std::vector<double> b;
    std::vector<double> f;
    
    double A;
    
    double operator()(double x) const;
    
    int size() const {return f.size();}
    
    double error(double (*f)(double x),const Distribution& D) const;
    double error2(double (*f)(double x),const Distribution& D) const;
    
    double moment(double p) const;
    
    double scale() const; 
    void scale(double r);
    
    Discretization(int N);
    
    Discretization(int N,const Distribution& D,double a=2);
  };

  /// A Discretization with equal bin probabilities
  struct UniformDiscretization: public Discretization
  {
    UniformDiscretization(int N);
    
    UniformDiscretization(int N, const Distribution& D);
  };

}
#endif
