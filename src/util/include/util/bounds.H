/*
  Copyright (C) 2004-2006,2009-2010 Benjamin Redelings

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

///
/// \file bounds.H
///
/// \brief This file defines a generic bounds class for integer or real parameters.
///

#ifndef BOUNDS_H
#define BOUNDS_H

#include <iostream>
#include "util/string/convert.H"
#include <optional>
// FIXME
#include "../../computation/object.H"
#include "util/wrap.H"

template <typename T>
struct Bounds: public Object
{
    /// the value of the lower bound, if any
    std::optional<T> lower_bound;

    /// the value of the upper bound, if any
    std::optional<T> upper_bound;

    Bounds<T>* clone() const {return new Bounds<T>(*this);}

    bool operator==(const Object& O) const
	{
	    if (this == &O) return true;

	    const Bounds<T>* B = dynamic_cast<const Bounds<T>*>(&O);
	    if (not B) return false;

	    if (lower_bound != B->lower_bound) return false;
	    if (upper_bound != B->upper_bound) return false;

	    return true;
	}
    std::string print() const
	{
	    std::string output = "(";

	    if (lower_bound)
		output += convertToString(*lower_bound);
	    else
		output += "-Inf";

	    output += ", ";

	    if (upper_bound)
		output += convertToString(*upper_bound);
	    else
		output += "Inf";

	    output += ")";

	    return output;
	}

    /// check if a value is below the lower bound on the range
    bool below_lower_bound(double x) const { return lower_bound and x<*lower_bound; }
    /// check if a value is above the upper bound on the range
    bool above_upper_bound(double x) const { return upper_bound and x>*upper_bound; }
    /// check if a value is in the range or not
    bool in_range(double x) const  { return not below_lower_bound(x) and not above_upper_bound(x);}

    /// set the lower bound on the range
    void set_lower_bound(T);

    /// set the upper bound on the range
    void set_upper_bound(T);

    template <typename U>
    Bounds(const Bounds<U>& b)
    {
	if (b.lower_bound)
	    lower_bound = *b.lower_bound;
	if (b.upper_bound)
	    upper_bound = *b.upper_bound;
    }

    Bounds() = default;
    Bounds(const std::optional<T>&, const std::optional<T>&);
};


template <typename T>
void Bounds<T>::set_lower_bound(T x)
{
    lower_bound = x;
}

template <typename T>
void Bounds<T>::set_upper_bound(T x)
{
    upper_bound = x;
}

template <typename T>
Bounds<T>::Bounds(const std::optional<T>& l, const std::optional<T>& u)
    :lower_bound(l),upper_bound(u)
{ }

template <typename T>
Bounds<T> lower_bound(T l)
{
    return Bounds<T>(l,{});
}

template <typename T>
Bounds<T> upper_bound(T u)
{
    return Bounds<T>({},u);
}

template <typename T>
Bounds<T> between(T u, T l)
{
    return Bounds<T>(u,l);
}

template <typename T>
Bounds<T> operator&&(Bounds<T> b1, const Bounds<T>& b2)
{
    if (b2.lower_bound)
    {
	if (b1.lower_bound)
	    b1.set_lower_bound(std::max(*b1.lower_bound, *b2.lower_bound));
	else
	    b1.set_lower_bound(*b2.lower_bound);
    }

    if (b2.upper_bound)
    {
	if (b1.upper_bound)
	    b1.set_upper_bound(std::min(*b1.upper_bound, *b2.upper_bound));
	else
	    b1.set_upper_bound(*b2.upper_bound);
    }

    return b1;
}

template <typename T>
Bounds<T> operator||(Bounds<T> b1, const Bounds<T>& b2)
{
    if (b1.lower_bound and b2.lower_bound)
	b1.lower_bound = std::min(*b1.lower_bound, *b2.lower_bound);
    else
	b1.lower_bound = {};


    if (b1.upper_bound and b2.upper_bound)
	b1.upper_bound = std::max(*b1.upper_bound, *b2.upper_bound);
    else
	b1.upper_bound = {};
}

template <typename T>
std::ostream& operator<<(std::ostream& o, const Bounds<T>& b)
{
    o<<"[";
    if (b.lower_bound)
	o<<*b.lower_bound;
    else
	o<<"-Inf";
    o<<", ";
    if (b.upper_bound)
	o<<*b.upper_bound;
    else
	o<<"Inf";
    o<<"]";
    return o;
}

template <typename T>
inline T wrap(T x, const Bounds<T>& b)
{
    if (b.lower_bound and b.upper_bound)
	return wrap<T>(x, *b.lower_bound, *b.upper_bound);
    else if (b.lower_bound)
	return reflect_more_than(x, *b.lower_bound);
    else if (b.upper_bound)
	return reflect_less_than(x, *b.lower_bound);
    else
	return x;
}
#endif
