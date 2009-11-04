/*
   Copyright (C) 2004-2006 Benjamin Redelings

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

#ifndef MYEXCEPTION_H
#define MYEXCEPTION_H
#include <exception>
#include <string>
#include <sstream>

class myexception: public std::exception {
  std::string why;
public:
  virtual const char* what() const throw() {return why.c_str();}

  template <typename T> myexception& operator<<(const T&);

  void prepend(const std::string& s) {why = s + why;}

  myexception() throw() {}
  myexception(const std::string& s) throw() : why(s) {}
  virtual ~myexception() throw() {}
};

template <typename T>
myexception& myexception::operator<<(const T& t) {
  std::ostringstream oss;
  oss<<why<<t;
  why = oss.str();
  return *this;
}

std::string show_stack_trace(int ignore=1);

#endif
