#ifndef OBJECT_H
#define OBJECT_H

#include <string>

#include <boost/shared_ptr.hpp>

#include "util.H"

enum maybe_t {maybe = -1, no = 0, yes = 1};

class Object {
public:
  virtual Object* clone() const =0;

  virtual maybe_t compare(const Object& O) const
  {
    if (this == &O) 
      return yes;
    else
      return maybe;
  }

  bool equals(const Object& O) const
  {
    return compare(O) == yes;
  }

  bool not_equals(const Object& O) const
  {
    return compare(O) == no;
  }

  bool maybe_equals(const Object& O) const
  {
    maybe_t m = compare(O);
    return (m == yes or m == maybe);
  }

  bool maybe_not_equals(const Object& O) const
  {
    maybe_t m = compare(O);
    return (m == no or m == maybe);
  }

  virtual std::string print() const {return "[unprintable]";}

  virtual ~Object() {}
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

  virtual maybe_t compare(const Object& O) const 
  {
    // The same object
    if (this == &O) return yes;

    // The same value
    const POD<T>* o = dynamic_cast<const POD<T>*>(&O);
    if (o and t == o->t) return yes;

    // Not the same
    return no;
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

  virtual maybe_t compare(const Object& O) const 
  {
    // The same object
    if (this == &O) return yes;

    // The same value and type
    const POD_math<T>* o = dynamic_cast<const POD_math<T>*>(&O);
    if (o and t == o->t) return yes;

    // Not the same
    return no;
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
