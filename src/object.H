#ifndef OBJECT_H
#define OBJECT_H

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/logic/tribool.hpp>

#include "util.H"

using boost::logic::tribool;
using boost::logic::indeterminate;

class Object {
public:
  virtual Object* clone() const =0;

  virtual tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    else
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
    return (m == true or m == indeterminate);
  }

  bool maybe_not_equals(const Object& O) const
  {
    tribool m = compare(O);
    return (m == false or m == indeterminate);
  }

  virtual std::string print() const {return "[unprintable]";}

  virtual ~Object() {}
};

struct object_ref: public boost::shared_ptr<const Object>
{
  explicit object_ref(Object *o)
    :boost::shared_ptr<const Object>(o)
  { }

  object_ref(const boost::shared_ptr<const Object>& o)
    :boost::shared_ptr<const Object>(o)
  { }

  object_ref(const Object& o)
    :boost::shared_ptr<const Object>(o.clone())
  { }
};

template<typename T>
class POD: virtual public Object 
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

template <typename T>
class POD_math: virtual public Object
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

  POD_math<T>& operator+=(const T& t2) {t += t2; return *this;}
  POD_math<T>& operator-=(const T& t2) {t -= t2; return *this;}
  POD_math<T>& operator*=(const T& t2) {t *= t2; return *this;}
  POD_math<T>& operator/=(const T& t2) {t /= t2; return *this;}

  POD_math<T> operator+(const T& t2) const {POD_math<T> t3 = *this; t3+= t2; return t3;}
  POD_math<T> operator-(const T& t2) const {POD_math<T> t3 = *this; t3-= t2; return t3;}
  POD_math<T> operator*(const T& t2) const {POD_math<T> t3 = *this; t3*= t2; return t3;}
  POD_math<T> operator/(const T& t2) const {POD_math<T> t3 = *this; t3/= t2; return t3;}
};

template <typename T>
bool unshare(boost::shared_ptr<T>& ptr)
{
  if (ptr.unique()) return false;

  ptr = boost::shared_ptr<T>(ptr->clone());

  return true;
}

typedef POD_math<double> Double;
typedef POD_math<int> Int;
typedef POD_math<unsigned> Unsigned;
typedef POD<bool> Bool;

template <typename T>
boost::shared_ptr<T> ptr(const T& t) {return boost::shared_ptr<T>(t.clone());}
template <typename T>
boost::shared_ptr<const T> const_ptr(const T& t) {return boost::shared_ptr<const T>(t.clone());}
#endif
