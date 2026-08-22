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

#include <iostream>
#include <cmath>
#include "util/math/ProbDensity.hh"
#include "util/math/neumaier.hh"
#include "util/assert.hh"

template <typename T = double>
struct log_prod_underflow
{
    NeumaierMultiplier<T> accum;
    double prod = 1.0;

    log_prod_underflow& operator*=(LogNum<T> x)
    {
	accum *= x;
	return *this;
    }

    log_prod_underflow& operator*=(double x)
    {
	assert(not (x < 0));

        // Keep defects out of the finite product so that operations such as 0*Inf
        // cannot erase their separate multiplicities before they reach LogDensity.
        if (x == 0 or not std::isfinite(x))
        {
            accum *= LogNum<T>(x);
            return *this;
        }

	double mult = prod * x;
	if (mult >= std::numeric_limits<double>::min())
	    prod = mult;
	// keep the larger of {prod,x} in the "prod" slot.
	else if (prod < x)
	{
	    accum *= LogNum<T>(prod);
	    prod = x;
	}
	else
	    accum *= LogNum<T>(x);

	assert(prod != 0);
	return *this;
    }

    log_prod_underflow& mult_with_count(double x, int c)
    {
	assert(c >= 0);
	assert(not (x < 0));

        if (c == 0)
            return *this;

        if (x == 0 or not std::isfinite(x))
        {
            accum *= repeat_product(LogNum<T>(x), c);
            return *this;
        }

	constexpr int count_limit = 4;

	if (c >= count_limit)
	    accum *= repeat_product(LogNum<T>(x), c);
	else
	{
	    double f = x;
	    for(int i=1;i<c;i++)
		f *= x;

	    double mult = prod * f;
	    if (mult >= std::numeric_limits<double>::min()) // multi > 0
		prod = mult;
	    // keep the larger of {prod,f} in the "prod" slot, if f > 0
	    else if (prod < f) // f > 0
	    {
		accum *= LogNum<T>(prod);
		prod = f;
	    }
	    else
		accum *= repeat_product(LogNum<T>(x), c);
	}

	assert(prod != 0);
	return *this;
    }

    LogNum<T> value() const
    {
        auto result = accum.result();
        if (prod != 1)
            result *= LogNum<T>(prod);
        return result;
    }

    operator LogNum<T>() const {return value();}
    bool is_zero() const {return value().is_zero();}
    constexpr log_prod_underflow() {}
};

struct log_prod_underoverflow
{
    log_double_t accum = 1.0;
    double prod = 1.0;

    log_prod_underoverflow& operator*=(log_double_t x)
    {
	accum *= x;
	return *this;
    }

    // NOTE: by not using the smallest/largest possible values, we avoid FPE exceptions on underflow / overflow (mostly).
    static constexpr double max_value = 115792089237316195423570985008687907853269984665640564039457584007913129639936e0;
    // static constexpr double min_value = std::numeric_limits<double>::min();
    static constexpr double min_value = 1.0/max_value;

    log_prod_underoverflow& operator*=(double x)
    {
	bool was_zero = is_zero();

	double mult = prod * x;
        if (mult < min_value)
        {
            // keep the larger of {prod,x} in the "prod" slot.
            if (prod < x)
            {
                accum *= prod;
                prod = x;
            }
            else
                accum *= x;
        }
        else if (mult > max_value)
        {
            // keep the smaller of {prod,x} in the "prod" slot.
            if (prod > x)
            {
                accum *= prod;
                prod = x;
            }
            else
                accum *= x;
        }
        else
	    prod = mult;


	assert(not this->is_zero() or x == 0 or was_zero);
	return *this;
    }

    log_prod_underoverflow& mult_with_count(double x, int c)
    {
	assert(c >= 0);
	assert(prod >= 0);

	constexpr int count_limit = 4;

	bool was_zero = is_zero();

	if (c >= count_limit)
	    accum *= pow(log_double_t(x),c);
	else
	{
	    double f = x;
	    for(int i=1;i<c;i++)
		f *= x;

	    double mult = prod * f;
            if (mult < min_value)
            {
                // keep the larger of {prod,x} in the "prod" slot.
                if (prod < x)
                {
                    accum *= prod;
                    prod = f;
                }
                else
                    accum *= pow(log_double_t(x),c);
            }
            else if (mult > max_value)
            {
                if (prod > x)
                {
                    accum *= prod;
                    prod = f;
                }
                else
                    accum *= pow(log_double_t(x),c);
            }
            else
                prod = mult;
	}

	assert(not this->is_zero() or x == 0 or was_zero);
	return *this;
    }

    log_double_t value() const {return accum * prod;}
    operator log_double_t () const {return value();}
    bool is_zero() const {return value() <= 0.0;}
    constexpr log_prod_underoverflow() {}
};

struct log_prod_rescale
{
    static constexpr double scale_factor = 115792089237316195423570985008687907853269984665640564039457584007913129639936e0;
    static constexpr double scale_min = 1.0/scale_factor;
    static constexpr double log_scale_min = -177.445678223345999210811423093293201427328034396225345054e0;
    
    double prod = 1.0;
    int scale = 0;

    log_prod_rescale& operator*=(double x)
    {
	bool was_zero = is_zero();

	prod *= x;

	if (prod < scale_min)
	{
	    prod *= scale_factor;
	    scale++;
	}
        else if (prod > scale_factor)
        {
            prod *= scale_min;
            scale--;
        }

	assert(not this->is_zero() or x == 0 or was_zero);
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

    log_prod_logsum& operator*=(log_double_t x)
    {
	accum *= x;
	return *this;
    }

    log_prod_logsum& operator*=(double x)
    {
	bool was_zero = is_zero();

	accum *= x;
	assert(not this->is_zero() or x == 0 or was_zero);
	return *this;
    }
    constexpr log_double_t value() const {return accum;}
    constexpr operator log_double_t () const {return value();}

    bool is_zero() const {return value() <= 0.0;}
    constexpr log_prod_logsum() {}
};

using log_prod = log_prod_underflow<double>;
using prob_density_prod = log_prod_underflow<LogDensity>;

#endif
