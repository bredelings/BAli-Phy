/*
  Copyright (C) 2004-2005, 2010, 2014, 2016-17 Benjamin Redelings

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

#ifndef LOG_DOUBLE_H
#define LOG_DOUBLE_H

#include "util/assert.hh"
#include <iostream>
#include "util/math/logsum.H"

/// A class for handling positive real number in terms of their natural log.
class log_double_t {
    /// Natural log of the number.
    double value = -std::numeric_limits<double>::infinity();
public:

    /// Access the log of the number.
    constexpr double  log() const {return value;}
    /// Access the log of the number.
    constexpr double& log()       {return value;}

    log_double_t& operator +=(const log_double_t& y) {value = logsum(value,y.log()); return *this;}
    log_double_t& operator -=(const log_double_t& y) {value = logdiff(log(),y.log()); return *this;}
    constexpr log_double_t& operator *=(const log_double_t& y) {value += y.log(); return *this;}
    constexpr log_double_t& operator /=(const log_double_t& y) {value -= y.log(); return *this;}

    explicit operator double() const {return exp(value);}

    constexpr log_double_t& operator=(double y)
    {
	assert(not (y<0));

        // It seems that log(0) may raise FE_DIVBYZERO.
        // Normally this is silent. But can be made trapping with feenableexcept( ).
	if (y == 0)
	    value = -std::numeric_limits<double>::infinity();
        // This could help if we inline this function.
	else if (y == 1)
            value = 0;
	else
	    value = ::log(y);

	return *this;
    }

    constexpr bool is_zero() const  {return value == -std::numeric_limits<double>::infinity();}

    constexpr log_double_t() {}

    constexpr log_double_t(double x)
    {
	operator=(x);
    }
};

constexpr double log(log_double_t x) {
    return x.log();
}

inline log_double_t operator+(log_double_t x, log_double_t y) {
    log_double_t z = x;
    z += y;
    return z;
}

inline log_double_t operator-(log_double_t x, log_double_t y) {
    log_double_t z = x;
    z -= y;
    return z;
}

constexpr log_double_t operator*(log_double_t x, log_double_t y) {
    log_double_t z = x;
    z *= y;
    return z;
}

constexpr log_double_t operator/(log_double_t x, log_double_t y) {
    log_double_t z=x;
    z /= y;
    return z;
}

constexpr auto operator<=>(log_double_t x, log_double_t y) {
    return log(x) <=> log(y);
}

constexpr bool operator==(log_double_t x, log_double_t y) {
    return log(x) == log(y);
}

constexpr log_double_t pow(log_double_t x,double p)
{
    x.log() *= p;
    return x;
}

constexpr log_double_t sqrt(log_double_t x)
{
    return pow(x,0.5);
}

template<class T> T exp_to(double);

template<> constexpr log_double_t exp_to<log_double_t>(double x) {
    log_double_t y;
    y.log() = x;
    return y;
}

// Prints the log of the number, not the number itself
inline std::ostream& operator<<(std::ostream& o,const log_double_t& e) {
    return o<<log(e);
}

#endif
