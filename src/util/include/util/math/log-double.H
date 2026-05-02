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
template <typename T>
class LogNum {
    /// Natural log of the number.
    T value = -infinity<T>;
public:
    typedef T log_value_type;
    
    /// Access the log of the number.
    constexpr T  log() const {return value;}

    /// Access the log of the number.
    /// Possibly replace this...
    constexpr T& log()       {return value;}

    LogNum& operator +=(const LogNum& y) {value = logsum(value,y.log()); return *this;}
    LogNum& operator -=(const LogNum& y) {value = logdiff(log(),y.log()); return *this;}
    constexpr LogNum& operator *=(const LogNum& y) {value += y.log(); return *this;}
    constexpr LogNum& operator /=(const LogNum& y) {value -= y.log(); return *this;}

    template <typename U> constexpr LogNum& operator *=(const LogNum<U>& y) {value += y.log(); return *this;}
    template <typename U> constexpr LogNum& operator /=(const LogNum<U>& y) {value -= y.log(); return *this;}

    // These all need to be in-class friends to allow conversion from double to LogNum

    friend constexpr LogNum operator+(const LogNum& x, const LogNum& y) {
        LogNum z = x;
        z += y;
        return z;
    }

    friend constexpr LogNum operator-(const LogNum& x, const LogNum& y) {
        LogNum z = x;
        z -= y;
        return z;
    }

    friend constexpr LogNum operator*(const LogNum& x, const LogNum& y) {
        LogNum z = x;
        z *= y;
        return z;
    }

    friend constexpr LogNum operator/(const LogNum& x, const LogNum& y) {
        LogNum z=x;
        z /= y;
        return z;
    }

    friend constexpr auto operator<=>(const LogNum& x, const LogNum& y) {
        return x.log() <=> y.log();
    }

    friend constexpr bool operator==(const LogNum& x, const LogNum& y) {
        return x.log() == y.log();
    }

    explicit operator T() const {return exp(value);}

    constexpr LogNum& operator=(double y)
    {
	assert(not (y<0));

        // It seems that log(0) may raise FE_DIVBYZERO.
        // Normally this is silent. But can be made trapping with feenableexcept( ).
	if (y == 0)
	    value = -infinity<T>;
        // This could help if we inline this function.
	else if (y == 1)
            value = T(0);
	else
	    value = T(::log(y));

	return *this;
    }

    LogNum& operator=(const LogNum&) = default;
    template <typename U>
    LogNum& operator=(const LogNum<U>& u) {value = T(u.log()); return *this;}

    constexpr bool is_zero() const  {return value == -infinity<T>;}

    constexpr LogNum() {}

    LogNum(const LogNum&) = default;
    template <typename U>
    LogNum(const LogNum<U>& u): value(u.log()) {}

    // Possibly change this to a function from_linear(T), or to_log(T).
    // That would require manual changing to-and-from log-space.
    constexpr LogNum(double x)
    {
	operator=(x);
    }
};

template <typename T>
constexpr T log(const LogNum<T>& x) {
    return x.log();
}

template <typename T>
constexpr LogNum<T> pow(LogNum<T> x, double p)
{
    x.log() *= p;
    return x;
}

template <typename T>
constexpr LogNum<T> sqrt(const LogNum<T>& x)
{
    return pow(x,0.5);
}

template<typename T> constexpr LogNum<T> exp_to_log_space(const T& x)
{
    LogNum<T> y;
    y.log() = x;
    return y;
}


// Prints the log of the number, not the number itself
template <typename T>
inline std::ostream& operator<<(std::ostream& o,const LogNum<T>& e) {
    return o<<log(e);
}

typedef LogNum<double> log_double_t;

#endif
