/*
   Copyright (C) 2004-2005 Benjamin Redelings

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

#ifndef CLONE_H
#define CLONE_H

#include <cassert>

class Cloneable {
public:
  virtual Cloneable* clone() const =0;
  virtual ~Cloneable() {}
};


template <class T>
class OwnedPointer {
private:
  T* data;

public:

  // Get a copy of an object
  OwnedPointer() throw() 
    : data(0) 
  {}

  // Get a copy of an object
  OwnedPointer(const T& t) throw() 
    : data(t.clone()) 
  {}

  // Take ownership of object t
  explicit OwnedPointer(T* t) throw() 
    : data(t) 
  { }

  // Get a copy of an object
  OwnedPointer(const OwnedPointer& O) throw() 
    :data(NULL)
  {
    if (O)
      data = O->clone();
  }

  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer(const OwnedPointer<T2>& O) throw()
    :data(NULL)
  {
    if (O)
      data = O->clone();
  }

  template <typename T2>
  void attatch(T2* p) {
    if (data) delete data;

    data = p;
  }

  // Get a copy of an object
  OwnedPointer& operator=(const OwnedPointer& O) throw() {
    if (data != O.get() and data != NULL)
      delete data;

    if (O)
      data = O->clone();
    else
      data = NULL;

    return *this;
  }
  
  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer& operator=(const OwnedPointer<T2>& O) throw() {
    if (data != O.get() and data != NULL)
      delete data;

    if (O)
      data = O->clone();
    else
      data = NULL;

    return *this;
  }

  // Get a copy of an object
  OwnedPointer& operator=(const T* t) throw() {
    if (data != t and data != NULL)
      delete data;

    if (t)
      data = t->clone();
    else
      data = NULL;

    return *this;
  }
  
  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer& operator=(const T2* t) throw() {
    if ((T2*)data != t and data != NULL)
      delete data;

    if (t)
      data = t->clone();
    else
      data = NULL;

    return *this;
  }

  // Dispose of object when we are done
  ~OwnedPointer() { 
    if (data) delete data;
  }

  operator bool() const {return data;}

  T& operator*() const throw() {
    return *data;
  }

  T* operator->() const throw() {
    return data;
  }

  T* get() const throw() {
    return data;
  }
};


template <class T>
class OwnedRef {
private:
  OwnedPointer<T> data;

public:

  // Get a copy of an object
  OwnedRef(const T& t) throw() 
    : data(t) 
  {}

  // Take ownership of object t
  explicit OwnedRef(const T* t) throw() 
    : data(t->clone()) 
  {}

  operator T& () throw() {
    return *data;
  }

  operator const T& () const throw() {
    return *data;
  }

  T* operator &() throw() {
    return data.get();
  }

  const T* operator &() const throw() {
    return data.get();
  }
};


#endif
