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
  int _refs = 0; 

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

  virtual int type() const {return 0;}

  virtual std::string print() const {return std::string("unprintable[")+demangle(typeid(*this).name())+"]";}

  virtual ~Object() {}
};

// What type of smart pointer are we using to point to Objects?
template <typename T>
using object_ptr = boost::intrusive_ptr<T>;

struct object_ref: public object_ptr<const Object>
{
  object_ref()
  { }

  object_ref(Object *o)
  : object_ptr<const Object>(o)
  { }

  object_ref(const object_ptr<const Object>& o)
    :object_ptr<const Object>(o)
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
class Box: public Object 
{
public:
  T t;

  Box<T>* clone() const {return new Box<T>(*this);}

  operator const T&() const {return t;}

  //  template <typename U>
  //  operator Box<U>() const {return t;}

  Box<T>& operator=(const Box<T>& p2) {t = p2.t; return *this;}

  Box<T>& operator=(const T& t2) {t = t2; return *this;}

  Box() {}
  Box(const T& t2):t(t2) {}

  // Doesn't seem to do ANYTHING?  Why not?
  //  template <typename U>
  //  Box(const boost::enable_if_c<boost::is_convertible<U,T>::type,U>& u):t(u) { }
};

template<typename T>
class POD: public Object 
{
protected:
  T t;
public:
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
inline std::string POD<std::string>::print() const
{
  return "\""+t+"\"";
}
template<> 
inline std::string POD<char>::print() const
{
  return std::string("'")+t+"'";
}
#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/utility/enable_if.hpp>

/***************** Make log_double_t count as an arithmetic type **************/

typedef POD<bool> Bool;
typedef POD<std::string> String;

template <typename T>
class POD_math: public Object
{
protected:
  T t;
public:
  POD_math<T>* clone() const {return new POD_math<T>(*this);}

  operator T() const {return t;}

  //  template <typename U>
  //  operator POD_math<U>() const {return t;}

  POD_math<T>& operator=(const POD_math<T>& p2) {t = p2.t; return *this;}

  POD_math<T>& operator=(const T& t2) {t = t2; return *this;}

  POD_math() {}
  POD_math(const T& t2):t(t2) {}

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

  Bool operator==(const POD_math<T>& t2) const {return t == T(t2);}
  Bool operator==(const T& t2)           const {return t == T(t2);}
  Bool operator>(const POD_math<T>& t2)  const {return t > T(t2);}
  Bool operator>(const T& t2)            const {return t > T(t2);}
  Bool operator>=(const POD_math<T>& t2) const {return t >= T(t2);}
  Bool operator>=(const T& t2)           const {return t >= T(t2);}
  Bool operator<(const POD_math<T>& t2)  const {return t < T(t2);}
  Bool operator<(const T& t2)            const {return t < T(t2);}
  Bool operator<=(const POD_math<T>& t2) const {return t <= T(t2);}
  Bool operator<=(const T& t2)           const {return t <= T(t2);}
};

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
object_ptr<T> convert(const object_ptr<const Object>& o)
{
#ifdef NDEBUG
  return boost::static_pointer_cast<T>(o);
#else
  assert(o);
  object_ptr<T> converted =  boost::dynamic_pointer_cast<const T>(o);
  if (not converted)
    throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(*o.get()).name())<<" to type "<<demangle(typeid(T).name());
  return converted;
#endif
}

object_ref parse_object(const std::string& s);
#endif
