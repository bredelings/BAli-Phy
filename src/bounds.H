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
#include <util.H>

template <typename T>
struct Bounds
{
  /// is there a lower bound on the range?
  bool has_lower_bound;
  /// the value of the lower bound, if any
  T lower_bound;

  /// is there an upper bound on the range?
  bool has_upper_bound;
  /// the value of the upper bound, if any
  T upper_bound;

  /// check if a value is below the lower bound on the range
  bool below_lower_bound(double x) const { return (has_lower_bound and x<lower_bound); }
  /// check if a value is above the upper bound on the range
  bool above_upper_bound(double x) const { return (has_upper_bound and x>upper_bound); }
  /// check if a value is in the range or not
  bool in_range(double x) const  { return (not below_lower_bound(x) and not above_upper_bound(x));}

  /// set the lower bound on the range
  void set_lower_bound(T);

  /// set the upper bound on the range
  void set_upper_bound(T);

  template <typename U>
  Bounds(const Bounds<U>& b)
    :has_lower_bound(b.has_lower_bound),
    lower_bound(b.lower_bound),
    has_upper_bound(b.has_upper_bound),
    upper_bound(b.has_upper_bound)
  { }

  Bounds();
  Bounds(T,T);
  Bounds(bool,T,bool,T);
};


template <typename T>
void Bounds<T>::set_lower_bound(T x)
{
  has_lower_bound = true;
  lower_bound = x;
}

template <typename T>
void Bounds<T>::set_upper_bound(T x)
{
  has_upper_bound = true;
  upper_bound = x;
}

template <typename T>
Bounds<T>::Bounds()
  :has_lower_bound(false),lower_bound(0),has_upper_bound(false),upper_bound(0)
{ }

template <typename T>
Bounds<T>::Bounds(T l, T u)
  :has_lower_bound(true),lower_bound(l),has_upper_bound(true),upper_bound(u)
{ }

template <typename T>
Bounds<T>::Bounds(bool hl, T l, bool hu, T u)
  :has_lower_bound(hl),lower_bound(l),has_upper_bound(hu),upper_bound(u)
{ }

template <typename T>
Bounds<T> lower_bound(T l)
{
  return Bounds<T>(true,l,false,0);
}

template <typename T>
Bounds<T> upper_bound(T u)
{
  return Bounds<T>(false,0,true,u);
}

template <typename T>
Bounds<T> between(T u, T l)
{
  return Bounds<T>(u,l);
}

template <typename T>
Bounds<T> operator&&(Bounds<T> b1, const Bounds<T>& b2)
{
  if (b2.has_lower_bound)
  {
    if (b1.has_lower_bound)
      b1.set_lower_bound(std::max(b1.lower_bound, b2.lower_bound));
    else
      b1.set_lower_bound(b2.lower_bound);
  }

  if (b2.has_upper_bound)
  {
    if (b1.has_upper_bound)
      b1.set_upper_bound(std::min(b1.upper_bound, b2.upper_bound));
    else
      b1.set_upper_bound(b2.upper_bound);
  }

  return b1;
}

template <typename T>
Bounds<T> operator||(Bounds<T> b1, const Bounds<T>& b2)
{
  if (b1.has_lower_bound and b2.has_lower_bound)
    b1.lower_bound = std::min(b1.lower_bound, b2.lower_bound);
 else
    b1.has_lower_bound = false;


  if (b1.has_upper_bound and b2.has_upper_bound)
    b1.upper_bound = std::max(b1.upper_bound, b2.upper_bound);
 else
    b1.has_upper_bound = false;
}

template <typename T>
std::ostream& operator<<(std::ostream& o, const Bounds<T>& b)
{
  o<<"[";
  if (b.has_lower_bound)
    o<<b.lower_bound;
  else
    o<<"-Inf";
  o<<", ";
  if (b.has_upper_bound)
    o<<b.upper_bound;
  else
    o<<"Inf";
  o<<"]";
  return o;
}

template <typename T>
inline T wrap(T x, const Bounds<T>& b)
{
  if (b.has_lower_bound and b.has_upper_bound)
    return wrap<T>(x, b.lower_bound, b.upper_bound);
  else if (b.has_lower_bound)
    return reflect_more_than(x, b.lower_bound);
  else if (b.has_upper_bound)
    return reflect_less_than(x, b.lower_bound);
  else
    return x;
}
#endif
