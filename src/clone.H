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
  explicit OwnedPointer(T* t) throw() 
    : data(t) 
  { }

  // Get a copy of an object
  OwnedPointer(const OwnedPointer& O) throw() 
    :data(NULL)
  {
    if (O)
      data = O->clone();
  }

  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer(const OwnedPointer<T2>& O) throw()
    :data(NULL)
  {
    if (O)
      data = O->clone();
  }

  template <typename T2>
  void attatch(T2* p) {
    if (data) delete data;

    data = p;
  }

  // Get a copy of an object
  OwnedPointer& operator=(const OwnedPointer& O) throw() {
    if (data != O.data and data != NULL)
      delete data;

    if (O)
      data = O->clone();
    else
      data = NULL;

    return *this;
  }
  
  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer& operator=(const OwnedPointer<T2>& O) throw() {
    if (data != O.data and data != NULL)
      delete data;

    if (O)
      data = O->clone();
    else
      data = NULL;

    return *this;
  }

  // Get a copy of an object
  OwnedPointer& operator=(const T* t) throw() {
    if (data != t and data != NULL)
      delete data;

    if (t)
      data = t->clone();
    else
      data = NULL;

    return *this;
  }
  
  // Get a copy of an object which is a SUB-CLASS of T
  template <typename T2>
  OwnedPointer& operator=(const T2* t) throw() {
    if ((T2*)data != t and data != NULL)
      delete data;

    if (t)
      data = t->clone();
    else
      data = NULL;

    return *this;
  }

  // Dispose of object when we are done
  ~OwnedPointer() { 
    if (data) delete data;
  }

  operator bool() const {return data;}

  T& operator*() const throw() {
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
