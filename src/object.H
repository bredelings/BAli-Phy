#ifndef OBJECT_H
#define OBJECT_H

#include <string>

enum maybe_t {maybe = -1, no = 0, yes = 1};

class Object {
public:
  virtual Object* clone() const =0;

  maybe_t compare(const Object& O) const
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
class POD: public Object 
{
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
bool unshare(boost::shared_ptr<T>& ptr)
{
  if (ptr.unique()) return false;

  ptr = boost::shared_ptr<T>(ptr->clone());

  return true;
}

typedef POD<double> Double;
typedef POD<int> Int;
typedef POD<unsigned> Unsigned;
typedef POD<bool> Bool;

#endif
