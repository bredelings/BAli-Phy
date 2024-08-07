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

/* To use cow-ptrs safely, you must remember the following things.
 *
 * 1. A cow-ptr that is set from an external pointer may still have its object 
 *    modifed out from under it through that pointer.  If the external pointer is
 *    also a cow-ptr, though, then you will be safe.
 *
 * 2. When dynamic_pointer_cast< >(p1) gives you a non-const pointer p2, it makes p1
 *    pointer point to a new object, and gives you a non-const pointer to that object.
 *    Be careful that p1 doesn't change again, thus losing any further changes you make
 *    through p2. (Hmm... make dynamic_pointer_cast< > give you a weak_ptr< >?)
 */

#ifndef COW_PTR_H
#define COW_PTR_H

#include <memory>
#include "util/assert.hh"
#include <cereal/types/memory.hpp>

using std::const_pointer_cast;

template <typename X>
struct constant_value: public std::shared_ptr<const X>
{
  explicit constant_value(const std::shared_ptr<const X>& x):std::shared_ptr<const X>(x) { }
};

/// A copy-on-write pointer class that stores X objects exactly: NOT objects derived from X
template <class X> class cow_ptr
{
private:
  std::shared_ptr<X> data;

  // create a new copy of data = make a direct copy otherwise
  void copy()
  {
    // Note that shared_ptr<X>::use_count() isn't thread-safe.
    if (data.use_count() > 1) {
      X* data_old = data.get();
      data = std::shared_ptr<X>(new X(*data_old));
    }
  }

public:
  typedef X element_type;

#ifndef NO_MEMBER_TEMPLATES
  template <class Y> friend class cow_ptr;
  template <class Y> cow_ptr(const cow_ptr<Y>& r) noexcept
    : data(r.data) {}
  template <class Y> cow_ptr& operator=(const cow_ptr<Y>& r)
  {
    data = r.data;
    return *this;
  }
#endif
  std::shared_ptr<const X> get_ptr()    const {        return data;}
  std::shared_ptr<X>       modify_ptr()       {copy(); return data;}

  const X& operator*()    const noexcept   {assert(data); return *data;}
  const X* operator->()   const noexcept   {assert(data); return data.get();}
  const X* get()          const noexcept   {              return data.get();}
    
  X* modify()                             {assert(data); copy(); return data.get();}

  void reset()
  {
    data.reset();
  }

  explicit operator bool() const // never throws
  {
    return (bool)data;
  }

  operator std::shared_ptr<const X>() const
  {
    return data;
  }

  long use_count() const
  {
    return data.use_count();
  }

  void swap(cow_ptr<X>& other) {std::swap(data,other.data);}

  cow_ptr& operator=(const cow_ptr& r)
  {
    data = r.data;
    return *this;
  }

  cow_ptr& operator=(const std::shared_ptr<const X>& r)
  {
    data = const_pointer_cast<X>(r);
    return *this;
  }

  cow_ptr& operator=(const std::shared_ptr<X>& r)
  {
    data = r;
    return *this;
  }

  template <class Archive>
  void serialize(Archive& ar)
  {
    ar(data);
  }

  cow_ptr(const cow_ptr& r) noexcept             : data(r.data) {}

  cow_ptr(const std::shared_ptr<const X>& p) : data(const_pointer_cast<X>(p)) { }

  cow_ptr(const std::shared_ptr<X>& p) : data(p) { }

  explicit cow_ptr(X* p = 0) noexcept            : data(p) {}
  // no need for ~cow_ptr - the counted_ptr takes care of everything.
};

template <typename X>
cow_ptr<X> make_cow_ptr(const X& x) { return cow_ptr<X>(std::make_shared<X>(x));}

template <typename X>
cow_ptr<X> make_cow_ptr(X&& x) { return cow_ptr<X>(std::make_shared<X>(std::move(x)));}

template <class X>
inline void swap(cow_ptr<X>& p1,cow_ptr<X>& p2)
{
  p1.swap(p2);
}

namespace _cow_ptr
{
  struct dynamic_cast_tag {};
}

/// A copy-on-write pointer class that stores X objects (OR objects derived from X) with a .clone() function
template <class X> class polymorphic_cow_ptr
{
private:
  std::shared_ptr<X> data;

  // create a new copy of data = make a direct copy otherwise
  void copy()
  {
    if (data.use_count() > 1) {
      X* data_old = data.get();
      data = std::shared_ptr<X>(data_old->clone());
    }
  }

public:
  typedef X element_type;

#ifndef NO_MEMBER_TEMPLATES
  template <class Y> friend class polymorphic_cow_ptr;
  template <class Y> polymorphic_cow_ptr(const polymorphic_cow_ptr<Y>& r) noexcept
    : data(r.data) {}
  template <class Y> polymorphic_cow_ptr& operator=(const polymorphic_cow_ptr<Y>& r)
  {
    data = r.data;
    return *this;
  }

#endif
  std::shared_ptr<const X> get_ptr()    const {        return data;}
  std::shared_ptr<X>       modify_ptr()       {copy(); return data;}

  const X& operator*()    const noexcept   {assert(data); return *data;}
  const X* operator->()   const noexcept   {assert(data); return data.get();}
  const X* get()          const noexcept   {              return data.get();}
    
  X* modify()                             {assert(data); copy(); return data.get();}

  void reset()
  {
    data.reset();
  }

  explicit operator bool() const // never throws
  {
    return (bool)data;
  }

  operator std::shared_ptr<const X>() const
  {
    return data;
  }

  long use_count() const
  {
    return data.use_count();
  }

  void swap(polymorphic_cow_ptr<X>& other) {std::swap(data,other.data);}

  polymorphic_cow_ptr& operator=(const polymorphic_cow_ptr& r)
  {
    data = r.data;
    return *this;
  }

  // Just because r is const, doesn't mean that there isn't a non-const pointer to it.
  polymorphic_cow_ptr& operator=(const std::shared_ptr<const X>& r)
  {
    data = const_pointer_cast<X>(r);
    return *this;
  }

  polymorphic_cow_ptr& operator=(const std::shared_ptr<X>& r)
  {
    data = r;
    return *this;
  }

  polymorphic_cow_ptr(const polymorphic_cow_ptr& r) noexcept             : data(r.data) {}

  polymorphic_cow_ptr(const constant_value<X>& p) : data(const_pointer_cast<X>(p)) { }

  // Just because p is const, doesn't mean that there isn't a non-const pointer to it.
  polymorphic_cow_ptr(const std::shared_ptr<const X>& p) : data(const_pointer_cast<X>(p)) { }

  polymorphic_cow_ptr(const std::shared_ptr<X>&       p) : data(p) { }

  template <typename Y> friend class polymorphic_cow_ptr;

  template <typename Y>
  polymorphic_cow_ptr(const polymorphic_cow_ptr<Y>& r,const _cow_ptr::dynamic_cast_tag&)
    :data(std::dynamic_pointer_cast<X>(r.data)) 
  {}

  explicit polymorphic_cow_ptr(X* p = 0) noexcept            : data(p) {}

  explicit polymorphic_cow_ptr(const X& x)                  : data(x.clone()) {}

  // no need for ~polymorphic_cow_ptr - the counted_ptr takes care of everything.
};

template <class X>
inline void swap(polymorphic_cow_ptr<X>& p1,polymorphic_cow_ptr<X>& p2)
{
  p1.swap(p2);
}

template <typename Y,typename Z>
std::shared_ptr<const Y> dynamic_pointer_cast(const polymorphic_cow_ptr<Z>& p)
{
  return std::dynamic_pointer_cast<const Y>(p.get_ptr());
}

template <typename Y,typename Z>
std::shared_ptr<Y> dynamic_pointer_cast(polymorphic_cow_ptr<Z>& p)
{
  // Don't call modify_ptr() if 
  if (dynamic_cast<const Y*>(&*p.get()))
    return std::dynamic_pointer_cast<Y>(p.modify_ptr());
  else
    return std::shared_ptr<Y>();
}

template <class T, class U> inline bool operator==(const polymorphic_cow_ptr<T>& t, const polymorphic_cow_ptr<U>& u)
{
  return t.get() == u.get();
}

template <class T, class U> inline bool operator<(const polymorphic_cow_ptr<T>& t, const polymorphic_cow_ptr<U>& u)
{
  return t.get() < u.get();
}

#endif // COW_PTR_H
