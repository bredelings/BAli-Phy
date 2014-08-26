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

/*
 * Usage:
 *
 *  owned_ptr<X> p1 = claim(new X);
 *  owned_ptr<X> p2;
 *  p2 = claim(new X);
 *  p1 = p2;
 *  owned_ptr<Y> p3 = p1.as<Y>();
 *
 */

#ifndef OWNED_PTR
#define OWNED_PTR

#include <boost/scoped_ptr.hpp>
#include <boost/pointer_cast.hpp>

/// A wrapper class that indicates that a constructor may claim the object pointed to.
template <class X>
struct claim_obj 
{
  X* data;
  // Forbid implicit conversion to claim_obj
  explicit claim_obj(X* d):data(d) {}
};

/// A wrapper function that around claim_obj<X>; X is deduced.
template <typename X>
claim_obj<X> claim(X* x) {return claim_obj<X>(x);}

/// \brief A pointer class for handling polymorphic objects.
///
/// Each owned_ptr< > owns the object that it points to, and it responsible
/// for deleting it.
///
/// This class does not currently handle operator=() or copy constructing
/// from NULL objects.
///
template <class X> 
class owned_ptr
{
private:
  /// The actual (non-copyable) pointer.
  boost::scoped_ptr<X> data;

public:
  typedef X element_type;

  /// Perform a dynamic_cast
  template <typename Y> 
  const Y* as() const
  {
    return dynamic_cast<const Y*>(get());
  }

  /// Perform a dynamic_cast
  template <typename Y> 
  Y* as()
  {
    return dynamic_cast<Y*>(get());
  }

  const X* get()         const  {return data.get();}
  const X* operator->()  const  {return get();}
  const X& operator*()   const  {return *get();}
    
  X* get()         {return data.get();}
  X* operator->()  {return get();}
  X& operator*()   {return *get();}
    
  /// Destroy object (if any) and set to NULL
  void reset()
  {
    data.reset();
  }

  /// Check if non-NULL
  typedef owned_ptr<X> this_type;

  explicit operator bool() const // never throws
  {
    return (bool)data;
  }

  /// Check if NULL
  bool operator !() const 
  {
    return !data;
  }

  void swap(owned_ptr<X>& other) {data.swap(other.data);}

  // Take ownership of object pointed-to.
  owned_ptr& operator=(const claim_obj<X>& xp)
  {
    boost::scoped_ptr<X> temp(xp.data);
    data.swap(temp);
    return *this;
  }

  // Take ownership of object pointed-to.
  template <class Y> 
  owned_ptr& operator=(const claim_obj<Y>& yp)
  {
    boost::scoped_ptr<X> temp(yp.data);
    data.swap(temp);
    return *this;
  }

  template <class Y> 
  owned_ptr& operator=(const owned_ptr<Y>& yp)
  {
    if (data.get() != yp.get()) {
      boost::scoped_ptr<X> temp(yp?yp->clone():0);
      data.swap(temp);
    }
    return *this;
  }

  owned_ptr& operator=(const owned_ptr& xp)
  {
    if (data.get() != xp.get()) {
      boost::scoped_ptr<X> temp(xp?xp->clone():0);
      data.swap(temp);
    }
    return *this;
  }

  owned_ptr& operator=(const X* xp)
  {
    if (data.get() != xp) {
      boost::scoped_ptr<X> temp(xp?xp->clone():0);
      data.swap(temp);
    }
    return *this;
  }

  owned_ptr& operator=(const X& x)
  {
    if (data.get() != &x) {
      boost::scoped_ptr<X> temp(x.clone());
      data.swap(temp);
    }
    return *this;
  }

  owned_ptr()
  {}

  // Take ownership of object pointed-to.
  owned_ptr(const claim_obj<X>& xp)
    :data(xp.data)
  {}

  // Take ownership of object pointed-to.
  template <typename Y>
  owned_ptr(const claim_obj<Y>& yp)
    :data(yp.data)
  {}

  owned_ptr(X* xp)
    : data(xp?xp->clone():0) {}

  owned_ptr(const X& x)
    : data(x.clone()) {}

  owned_ptr(const owned_ptr& xp)
    : data(xp?xp->clone():0) {}

  template <class Y> 
  owned_ptr(const owned_ptr<Y>& yp)
    : data(yp?yp->clone():0) {}

  // no need for ~owned_ptr - the scoped_ptr takes care of everything.
};

template <class X>
inline void swap(owned_ptr<X>& p1,owned_ptr<X>& p2)
{
  p1.swap(p2);
}

#endif

