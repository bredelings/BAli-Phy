#ifndef OBJECT_H
#define OBJECT_H

class Object {
public:
  virtual Object* clone() const =0;

  virtual bool possibly_different_from(const Object& O) const 
  {
    if (this == &O) return false;
    return true;
  }

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

  // Doesn't seem to do ANYTHING?  Why not?
  //  template <typename U>
  //  POD(const boost::enable_if_c<boost::is_convertible<U,T>::type,U>& u):t(u) { }
};

typedef POD<double> Double;
typedef POD<int> Int;
typedef POD<unsigned> Unsigned;

#endif
