#ifndef OBJECT_H
#define OBJECT_H

#include <string>
#include <boost/intrusive_ptr.hpp>
#include <boost/logic/tribool.hpp>
#include <iostream>
#include "util.H"

#include "math/log-double.H"
#include "computation/type_constant.H"

using boost::logic::tribool;
using boost::logic::indeterminate;

class Object 
{
  mutable int _refs = 0; 

  friend inline void intrusive_ptr_release(Object* pThis)
  {
    if (--pThis->_refs == 0 ) { 
      delete pThis;
    }
  }
  
  friend inline void intrusive_ptr_add_ref(Object* pThis)
  {
    pThis->_refs++;
  }
  
  friend inline void intrusive_ptr_release(const Object* pThis)
  {
    if(--const_cast<Object*>(pThis)->_refs == 0 ) { 
      delete const_cast<Object*>(pThis);
    }
  }
  
  friend inline void intrusive_ptr_add_ref(const Object* pThis)
  {
    const_cast<Object*>(pThis)->_refs++;
  }

public:

  virtual Object* clone() const =0;

  virtual tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return indeterminate;
  }

  int ref_count() const {return _refs;}

  virtual type_constant type() const {return unknown_type;}

  virtual std::string print() const {return std::string("unprintable[")+demangle(typeid(*this).name())+"]";}

  Object& operator=(const Object&) {return *this;}

  Object() {}
  Object(const Object&):_refs(0) {}
  virtual ~Object() {}
  void swap(Object&) {}
};

// What type of smart pointer are we using to point to Objects?
template <typename T>
using object_ptr = boost::intrusive_ptr<T>;

template<typename T>
class Box: public Object, public T
{
public:
  Box<T>* clone() const {return new Box<T>(*this);}

  Box<T>& operator=(const Box<T>& t) = default;
  Box<T>& operator=(Box<T>&& t) = default;

  Box<T>& operator=(const T& t)
  {
    T::operator=(t);
    return *this;
  }

  Box<T>& operator=(T&& t)
  {
    T::operator=(std::move(t));
    return *this;
  }

  std::string print() const {return Object::print();}

  using T::T;

  Box() = default;
  Box(const Box<T>&) = default;
  Box(Box<T>&&) = default;
  Box(const T& t):T(t) {}
  Box(T&& t):T(std::move(t)) {}
};

/***************** Make log_double_t count as an arithmetic type **************/

typedef Box<std::string> String;

template<> 
inline std::string Box<std::string>::print() const
{
  return "\""+(*this)+"\"";
}

template <typename T>
bool unshare(object_ptr<T>& ptr)
{
  if (ptr.unique()) return false;

  ptr = object_ptr<T>(ptr->clone());

  return true;
}

template <typename T>
using Vector = Box<std::vector<T>>;

template <typename T1,typename T2>
using Pair = Box<std::pair<T1,T2>>;

template <typename T>
object_ptr<T> ptr(const T& t) {return object_ptr<T>(t.clone());}
template <typename T>
object_ptr<const T> const_ptr(const T& t) {return object_ptr<const T>(t.clone());}

template<typename T>
inline std::ostream& operator<<(std::ostream& o,const object_ptr<T>& R)
{
  if (R)
    return o<<R->print();
  else
    return o<<"[NULL]";
}

inline std::ostream& operator<<(std::ostream& o,const Object& R)
{
  return o<<R.print();
}

template <typename T>
const T* convert_and_check(const Object* o)
{
  assert(o);
  const T* converted = dynamic_cast<const T*>(o);
  if (not converted)
    throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(*o).name())<<" to type "<<demangle(typeid(T).name());
  return converted;
}

template <typename T>
object_ptr<T> convert_and_check(const object_ptr<const Object>& o)
{
  assert(o);
  object_ptr<T> converted =  boost::dynamic_pointer_cast<const T>(o);
  if (not converted)
  {
    const Object* oo = o.get();
    throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(*oo).name())<<" to type "<<demangle(typeid(T).name());
  }
  return converted;
}

template <typename T>
const T* convert(const Object* o)
{
#ifdef NDEBUG
  return static_cast<const T*>(o);
#else
  return convert_and_check<T>(o);
#endif
}

template <typename T>
object_ptr<T> convert(const object_ptr<const Object>& o)
{
#ifdef NDEBUG
  return boost::static_pointer_cast<T>(o);
#else
  return convert_and_check<T>(o);
#endif
}

#endif
