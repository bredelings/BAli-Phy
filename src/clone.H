#ifndef CLONE_H
#define CLONE_H

#include <cassert>

class Cloneable {
public:
  virtual Cloneable* clone() const =0;
  virtual ~Cloneable() {}
};


template <class T>
class OwnedPointer {
private:
  T* data;

public:

  // Get a copy of an object
  OwnedPointer() throw() 
    : data(0) 
  {}

  // Get a copy of an object
  OwnedPointer(const T& t) throw() 
    : data(t.clone()) 
  {}

  // Take ownership of object t
  explicit OwnedPointer(const T* t) throw() 
    : data(t->clone()) 
  {}

  // Get a copy of an object
  OwnedPointer(const OwnedPointer& O) throw() 
    :data( O->clone())
  {}

  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer(const OwnedPointer<T2>& O) throw()
    :data(O->clone())
  {}

  // Get a copy of an object
  OwnedPointer& operator=(const OwnedPointer& O) throw() {
    if (data != O.data and data != 0)
      delete data;

    data = O->clone();

    return *this;
  }
  
  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer& operator=(const OwnedPointer<T2>& O) throw() {
    if (data != O.data and data != 0)
      delete data;

    data = O->clone();

    return *this;
  }

  // Dispose of object when we are done
  ~OwnedPointer() { 
    delete data;
  }

  T& operator*() const throw() {
    assert(data != 0);
    return *data;
  }

  T* operator->() const throw() {
    return data;
  }

  T* get() const throw() {
    return data;
  }
};


template <class T>
class OwnedRef {
private:
  OwnedPointer<T> data;

public:

  // Get a copy of an object
  OwnedRef(const T& t) throw() 
    : data(t) 
  {}

  // Take ownership of object t
  explicit OwnedRef(const T* t) throw() 
    : data(t->clone()) 
  {}

  operator T& () throw() {
    return *data;
  }

  operator const T& () const throw() {
    return *data;
  }

  T* operator &() throw() {
    return data.get();
  }

  const T* operator &() const throw() {
    return data.get();
  }
};


#endif
