/*
   Copyright (C) 2007 Benjamin Redelings

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

#ifndef CACHED_VALUE_H
#define CACHED_VALUE_H

template <typename T>
class cached_value 
{
  T value_;
  bool valid;
public:
  bool is_valid() const {return valid;}
  void invalidate() {valid=false;}
  void validate() {valid=true;}

  const T& value() const {return value_;}
        T& modify_value()       {return value_;}

  cached_value& operator=(const T& t) {value_ = t;valid=true;return *this;}

  operator const T&() const {assert(is_valid()); return value();}

  cached_value():valid(false) {}
  cached_value(const T& t):value_(t),valid(true) {}
};

#endif
