/*
  Copyright (C) 2017 Benjamin Redelings

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

#ifndef LOGPROD_H
#define LOGPROD_H

#include <cmath>
#include <cassert>
#include <iostream>

struct log_prod_underflow
{
    log_double_t accum = 1.0;
    double prod = 1.0;
    log_prod_underflow& operator*=(double x)
    {
#ifndef NDEBUG
	auto old = *this;
#endif	
	double mult = prod * x;
	if (mult >= std::numeric_limits<double>::min())
	    prod = mult;
	else if (prod < x)
	{
	    accum *= prod;
	    prod = x;
	}
	else
	    accum *= x;
	assert(not this->is_zero() or x == 0 or old.is_zero());
	return *this;
	    
    }
    log_double_t value() const {return accum * prod;}
    operator log_double_t () const {return value();}
    bool is_zero() const {return value() <= 0.0;}
    constexpr log_prod_underflow() {}
};

struct log_prod_rescale
{
    static constexpr double scale_factor = 115792089237316195423570985008687907853269984665640564039457584007913129639936e0;
    static constexpr double scale_min = 1.0/scale_factor;
    static constexpr double log_scale_min = log(scale_min);
    
    double prod = 1.0;
    int scale = 0;

    log_prod_rescale& operator*=(double x)
    {
#ifndef NDEBUG
	auto old = *this;
#endif	
	prod *= x;

	if (prod < scale_min)
	{
	    prod *= scale_factor;
	    scale++;
	}

#ifndef NDEBUG
	if (this->is_zero() and x != 0 and not old.is_zero())
	{
	    std::cerr<<"prod = "<<old.prod<<"    x = "<<x<<std::endl;
	}
#endif
	assert(not this->is_zero() or x == 0 or old.is_zero());
	return *this;
	    
    }
    log_double_t value() const {
	log_double_t Pr = prod;
	Pr.log() += log_scale_min * scale;
	return Pr;
    }

    operator log_double_t () const {return value();}

    bool is_zero() const {return value() <= 0.0;}
    constexpr log_prod_rescale() {}
};

struct log_prod_logsum
{
    log_double_t accum = 1.0;
    constexpr log_prod_logsum& operator*=(double x)
    {
#ifndef NDEBUG
	auto old = *this;
#endif	
	accum *= x;
	assert(not this->is_zero() or x == 0 or old.is_zero());
	return *this;
    }
    constexpr log_double_t value() const {return accum;}
    constexpr operator log_double_t () const {return value();}

    bool is_zero() const {return value() <= 0.0;}
    constexpr log_prod_logsum() {}
};

typedef log_prod_underflow log_prod;

#endif

