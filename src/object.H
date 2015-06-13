#ifndef OBJECT_H
#define OBJECT_H

#include <string>
#include <boost/intrusive_ptr.hpp>
#include <boost/logic/tribool.hpp>
#include <iostream>
#include "util.H"

#include "math/log-double.H"

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

  bool equals(const Object& O) const
  {
    return compare(O) == true;
  }

  bool not_equals(const Object& O) const
  {
    return compare(O) == false;
  }

  bool maybe_equals(const Object& O) const
  {
    tribool m = compare(O);
    return (m or indeterminate(m));
  }

  bool maybe_not_equals(const Object& O) const
  {
    tribool m = compare(O);
    return (not m or indeterminate(m));
  }

  int ref_count() const {return _refs;}

  virtual int type() const {return 0;}

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

struct object_ref: public object_ptr<const Object>
{
  bool is_int() const;
  int as_int() const;

  bool is_double() const;
  double as_double() const;

  bool is_char() const;
  char as_char() const;

  bool is_log_double() const;
  log_double_t as_log_double() const;

  object_ref()
  { }

  object_ref(Object *o)
  : object_ptr<const Object>(o)
  { }

  object_ref(const object_ptr<const Object>& o)
    :object_ptr<const Object>(o)
  { }

  object_ref(object_ptr<const Object>&& o)
    :object_ptr<const Object>(std::move(o))
  { }

  template <typename T>
  object_ref(const object_ptr<T>& o)
    :object_ptr<const Object>(boost::static_pointer_cast<const Object>(o))
  { }

  object_ref(const Object& o)
    :object_ptr<const Object>(o.clone())
  { }
};

template<typename T>
class Box: public Object, public T
{
public:
  Box<T>* clone() const {return new Box<T>(*this);}

  Box<T>& operator=(const Box<T>& t)
  {
    T::operator=(t);
    return *this;
  }

  Box<T>& operator=(const T& t)
  {
    T::operator=(t);
    return *this;
  }

  std::string print() const {return Object::print();}

  using T::T;

  Box() = default;
  Box(const Box<T>&) = default;
  explicit Box(const T& t):T(t) {}
};

template<typename T>
class POD: public Object 
{
public:
  T t;

  typedef T subtype;

  POD<T>* clone() const {return new POD<T>(*this);}

  operator T() const {return t;}

  //  template <typename U>
  //  operator POD<U>() const {return t;}

  POD<T>& operator=(const POD<T>& p2) {t = p2.t; return *this;}

  POD<T>& operator=(const T& t2) {t = t2; return *this;}

  POD() {}
  POD(const T& t2):t(t2) {}

  std::string print() const {return convertToString(t);}

  virtual tribool compare(const Object& O) const 
  {
    // The same object
    if (this == &O) return true;

    // The same value
    const POD<T>* o = dynamic_cast<const POD<T>*>(&O);
    if (o and t == o->t) return true;

    // Not the same
    return false;
  }

  // Doesn't seem to do ANYTHING?  Why not?
  //  template <typename U>
  //  POD(const boost::enable_if_c<boost::is_convertible<U,T>::type,U>& u):t(u) { }
};

template<> 
inline std::string Box<std::string>::print() const
{
  return "\""+(*this)+"\"";
}
template<> 
inline std::string POD<char>::print() const
{
  return std::string("'")+t+"'";
}
#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/utility/enable_if.hpp>

/***************** Make log_double_t count as an arithmetic type **************/

//typedef POD<bool> Bool;
typedef Box<std::string> String;

template <typename T>
class POD_math: public Object
{
protected:
  T t;
public:
  typedef T subtype;

  POD_math<T>* clone() const {return new POD_math<T>(*this);}

  operator T() const {return t;}

  //  template <typename U>
  //  operator POD_math<U>() const {return t;}

  POD_math<T>& operator=(const POD_math<T>& p2) {t = p2.t; return *this;}

  POD_math<T>& operator=(const T& t2) {t = t2; return *this;}

  POD_math() {}
  POD_math(const T& t2):t(t2) {}

  template <typename U>
  POD_math(const POD_math<U>& t2):t(t2) {}

  std::string print() const {return convertToString(t);}

  virtual tribool compare(const Object& O) const 
  {
    // The same object
    if (this == &O) return true;

    // The same value and type
    const POD_math<T>* o = dynamic_cast<const POD_math<T>*>(&O);
    if (o and t == o->t) return true;

    // Not the same
    return false;
  }

  POD_math<T>& operator+=(const POD_math<T>& t2) {t += t2; return *this;}
  POD_math<T>& operator+=(const T& t2)           {t += t2; return *this;}
  POD_math<T>& operator-=(const POD_math<T>& t2) {t -= t2; return *this;}
  POD_math<T>& operator-=(const T& t2)           {t -= t2; return *this;}
  POD_math<T>& operator*=(const POD_math<T>& t2) {t *= t2; return *this;}
  POD_math<T>& operator*=(const T& t2)           {t *= t2; return *this;}
  POD_math<T>& operator/=(const POD_math<T>& t2) {t /= t2; return *this;}
  POD_math<T>& operator/=(const T& t2)           {t /= t2; return *this;}

  POD_math<T> operator-() const {POD_math<T> t3 = *this; t3.t = -t3.t; return t3;}

  POD_math<T> operator+(const POD_math<T>& t2) const {POD_math<T> t3 = *this; t3+= t2; return t3;}
  POD_math<T> operator+(const T& t2)           const {POD_math<T> t3 = *this; t3+= t2; return t3;}
  POD_math<T> operator-(const POD_math<T>& t2) const {POD_math<T> t3 = *this; t3-= t2; return t3;}
  POD_math<T> operator-(const T& t2)           const {POD_math<T> t3 = *this; t3-= t2; return t3;}
  POD_math<T> operator*(const POD_math<T>& t2) const {POD_math<T> t3 = *this; t3*= t2; return t3;}
  POD_math<T> operator*(const T& t2)           const {POD_math<T> t3 = *this; t3*= t2; return t3;}
  POD_math<T> operator/(const POD_math<T>& t2) const {POD_math<T> t3 = *this; t3/= t2; return t3;}
  POD_math<T> operator/(const T& t2)           const {POD_math<T> t3 = *this; t3/= t2; return t3;}

  bool operator==(const POD_math<T>& t2) const {return t == T(t2);}
  bool operator==(const T& t2)           const {return t == T(t2);}
  bool operator!=(const POD_math<T>& t2) const {return t != T(t2);}
  bool operator!=(const T& t2)           const {return t != T(t2);}
  bool operator>(const POD_math<T>& t2)  const {return t > T(t2);}
  bool operator>(const T& t2)            const {return t > T(t2);}
  bool operator>=(const POD_math<T>& t2) const {return t >= T(t2);}
  bool operator>=(const T& t2)           const {return t >= T(t2);}
  bool operator<(const POD_math<T>& t2)  const {return t < T(t2);}
  bool operator<(const T& t2)            const {return t < T(t2);}
  bool operator<=(const POD_math<T>& t2) const {return t <= T(t2);}
  bool operator<=(const T& t2)           const {return t <= T(t2);}
};

template<> 
inline std::string POD_math<char>::print() const
{
  return std::string("'")+t+"'";
}

template<> 
inline std::string Box<std::vector<object_ref>>::print() const
{
  std::string s = "{";
  for(int i=0;i<size()-1;i++) {
    s += (*this)[i]->print();
    s += ",";
  }
  s += this->back()->print() + "}";
  return s;
}

template<> 
inline std::string Box<std::pair<object_ref,object_ref>>::print() const
{
  return std::string("(") + first->print() + "," + second->print() + ")";
}



template <typename T>
bool unshare(object_ptr<T>& ptr)
{
  if (ptr.unique()) return false;

  ptr = object_ptr<T>(ptr->clone());

  return true;
}

typedef POD_math<double> Double;
typedef POD_math<int> Int;
typedef POD_math<unsigned> Unsigned;
typedef POD_math<char> Char;
typedef POD_math<log_double_t> Log_Double;

template <typename T>
using Vector = Box<std::vector<T>>;

template <typename T1,typename T2>
using Pair = Box<std::pair<T1,T2>>;

typedef Vector<object_ref> OVector;
typedef std::pair<object_ref,object_ref> Opair;
typedef Pair<object_ref,object_ref> OPair;

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

object_ref parse_object(const std::string& s);

inline bool object_ref::is_int() const
{
  return (bool)boost::dynamic_pointer_cast<const Int>(*this);
}

inline int object_ref::as_int() const
{
  return *convert<Int>(get());
}

inline bool object_ref::is_double() const
{
  return (bool)boost::dynamic_pointer_cast<const Double>(*this);
}

inline double object_ref::as_double() const
{
  return *convert<Double>(get());
}

inline bool object_ref::is_char() const
{
  return (bool)boost::dynamic_pointer_cast<const Char>(*this);
}

inline char object_ref::as_char() const
{
  return *convert<Char>(get());
}

inline bool object_ref::is_log_double() const
{
  return (bool)boost::dynamic_pointer_cast<const Log_Double>(*this);
}

inline log_double_t object_ref::as_log_double() const
{
  return *convert<Log_Double>(get());
}

#endif
