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

#ifndef COW_PTR_H
#define COW_PTR_H

#include <boost/shared_ptr.hpp>

template <class X> class cow_ptr
{
private:
  boost::shared_ptr<X> data;

  // create a new copy of data = make a direct copy otherwise
  void copy()                            
  {
    if (not data.unique()) {
      X* data_old = data.get();
      data = boost::shared_ptr<X>(new X(*data_old));
    }
  }

public:
  typedef X element_type;

#ifndef NO_MEMBER_TEMPLATES
  template <class Y> friend class cow_ptr;
  template <class Y> cow_ptr(const cow_ptr<Y>& r) throw()
    : data(r.data) {}
  template <class Y> cow_ptr& operator=(const cow_ptr<Y>& r)
  {
    if (this != &r)
      data = r.data;
    return *this;
  }
#endif

  const X& operator*()    const throw()   {return *data;}
  const X* operator->()   const throw()   {return data.get();}
  const X* get()          const throw()   {return data.get();}
    
  X& operator*()                          {copy(); return *data;}
  X* operator->()                         {copy(); return data.get();}
  X* get()                                {copy(); return data.get();}

  void reset()
  {
    data.reset();
  }

  operator bool() const
  {
    return data;
  }

  long use_count() const
  {
    return data.use_count();
  }

  bool unique() const {
    return data.unique(); 
  }

  void swap(cow_ptr<X>& other) {std::swap(data,other.data);}

  cow_ptr& operator=(const cow_ptr& r)
  {
    if (this != &r)
      data = r.data;
    return *this;
  }

  cow_ptr(const cow_ptr& r) throw()             : data(r.data) {}

  explicit cow_ptr(X* p = 0) throw()            : data(p) {}

  explicit cow_ptr(const X& x)                  : data(new X(x)) {}

  // no need for ~cow_ptr - the counted_ptr takes care of everything.
};

template <class X>
inline void swap(cow_ptr<X>& p1,cow_ptr<X>& p2)
{
  p1.swap(p2);
}

template <class X> class polymorphic_cow_ptr
{
private:
  boost::shared_ptr<X> data;

  // create a new copy of data = make a direct copy otherwise
  void copy()                            
  {
    if (not data.unique()) {
      X* data_old = data.get();
      data = boost::shared_ptr<X>(data_old->clone());
    }
  }

public:
  typedef X element_type;

#ifndef NO_MEMBER_TEMPLATES
  template <class Y> friend class polymorphic_cow_ptr;
  template <class Y> polymorphic_cow_ptr(const polymorphic_cow_ptr<Y>& r) throw()
    : data(r.data) {}
  template <class Y> polymorphic_cow_ptr& operator=(const polymorphic_cow_ptr<Y>& r)
  {
    if (this != &r)
      data = r.data;
    return *this;
  }
#endif

  const X& operator*()    const throw()   {return *data;}
  const X* operator->()   const throw()   {return data.get();}
  const X* get()          const throw()   {return data.get();}
    
  X& operator*()                          {copy(); return *data;}
  X* operator->()                         {copy(); return data.get();}
  X* get()                                {copy(); return data.get();}

  void reset()
  {
    data.reset();
  }

  operator bool() const
  {
    return data;
  }

  long use_count() const
  {
    return data.use_count();
  }

  bool unique() const {
    return data.unique(); 
  }

  void swap(polymorphic_cow_ptr<X>& other) {std::swap(data,other.data);}

  polymorphic_cow_ptr& operator=(const polymorphic_cow_ptr& r)
  {
    if (this != &r)
      data = r.data;
    return *this;
  }

  polymorphic_cow_ptr(const polymorphic_cow_ptr& r) throw()             : data(r.data) {}

  explicit polymorphic_cow_ptr(X* p = 0) throw()            : data(p) {}

  explicit polymorphic_cow_ptr(const X& x)                  : data(x.clone()) {}

  // no need for ~polymorphic_cow_ptr - the counted_ptr takes care of everything.
};

template <class X>
inline void swap(polymorphic_cow_ptr<X>& p1,polymorphic_cow_ptr<X>& p2)
{
  p1.swap(p2);
}


#endif // COW_PTR_H
