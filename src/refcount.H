#ifndef REFCOUNT_H
#define REFCOUNT_H

/// This is a base class for reference
class RefObject {
  int count;
  bool shareable;

public:

  int n_refs() const {return count;}

  void inc() { count++; }

  void dec() { if (--count == 0) delete this; }

  bool is_shared() const {return n_refs() > 1;}
  
  bool is_shareable() const {return shareable;}

  void mark_unshareable() {shareable=false;}

  // when copying, copy the super object but keep our references
  RefObject& operator=(const RefObject& RO) {return *this;}

  // start out with no references
  RefObject():count(0),shareable(true) {}

  // start out with no references
  RefObject(const RefObject& RO):count(0),shareable(true) {}
  
  virtual ~RefObject()=0;
};

inline RefObject::~RefObject() {}


template <class T>
class RefPtr {
  T* data;

  void init() {
    if (not data) return;

    if (not data->is_shareable())
      data = new T(*data);

    data->inc();
  }

public:

  void uniquify() {
    if (data and data->is_shared()) {
      T* old_data = data;

      data = new T(*old_data);
      data->inc();

      old_data->dec();
    }
  }

  RefPtr(T* data_in = NULL)
    :data(data_in)
  {
    init();
  }

  RefPtr(const RefPtr<T>& RP)
    :data(RP.data)
  {
    init();
  }

  ~RefPtr() {if (data) data->dec();}

  T* operator->() const {return data;}
  T& operator*() const {return *data;}

  RefPtr<T>& operator=(const RefPtr<T>& R) {

    if (data != R.data) {
      T* old_data = data;
      data = R.data;

      init();

      if (old_data) old_data->dec();
    }

    return *this;
  }
};



#endif
