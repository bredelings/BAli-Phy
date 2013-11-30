#ifndef POOL_H
#define POOL_H

#include <functional>

template <typename T>
class pool
{
  int n_null_ = 0;
public:
  enum States {none=0,free=1,used=2,marked=3};

  friend class weak_ref;

  class weak_ref
  {
    int r=0;
    unsigned version=0;
  public:

    int get(const pool<T>& p) const {
      const auto& record = p.lookup(r);
      if (record.state == free) return 0;
      if (record.version != version) return 0;
      return r;
    }

    int get_unsafe() const {return r;}

    void reset() {version = 1;}

    weak_ref() = default;
    weak_ref(int a, unsigned b):r(a),version(b) {}
  };

protected:

  unsigned version = 1;

  struct record
  {
    States state = none;
    unsigned version = 0;
    int prev = -1;
    int next = -1;
    T value;
  };

  mutable std::vector<record> memory;

  int first_free = -1;
  int first_used = -1;


  int size() const
  {
    return memory.size();
  }

  int n_free() const
  {
    int here = first_free;
    int count = 0;
    for(;here != -1;here = lookup(here).next)
      count++;
    return count;
  }

  int n_used() const
  {
    int here = first_used;
    int count = 0;
    for(;here != -1;here = lookup(here).next)
      count++;
    return count;
  }

  int n_null() const
  {
    return n_null_;
  }

  virtual void expand_memory(int s)
  {
    assert(memory.size() == n_used() + n_free() + n_null());
    
    int k = memory.size();
    memory.resize(memory.size()+s);
    for(int i=k;i<memory.size();i++)
      add_to_free_list(i);
    
    assert(memory.size() == n_used() + n_free() + n_null());
  }

  virtual int add_to_free_list(int r)
  {
    lookup(r).state = free;
    lookup(r).version = version;
    lookup(r).prev = -1;
    lookup(r).next = first_free;
    if (first_free != -1)
      lookup(first_free).prev = r;
    first_free = r;
    return r;
  }

  virtual int get_free_element()
  {
    if (first_free == -1) return -1;

    int r = first_free;

    assert(is_free(r));
    first_free = lookup(r).next;
    lookup(r).prev = -1;
    lookup(r).next = -1;
    lookup(r).state = none;

    return r;
  }

  int add_to_used_list(int r)
  {
    lookup(r).state = used;
    lookup(r).prev = -1;
    lookup(r).next = first_used;
    if (first_used != -1)
      lookup(first_used).prev = r;
    first_used = r;
    return r;
  }

  void remove_from_used_list(int r)
  {
    assert(is_used(r));

    int P = lookup(r).prev;
    int N = lookup(r).next;

    if (P == -1)
      first_used = N;
    else
      lookup(P).next = N;

    if (N == -1)
      ;
    else
      lookup(N).prev = P;

    lookup(r).state = none;
  }

  const record& lookup(int i) const {assert(is_valid_address(i)); return memory[i];}
        record& lookup(int i)       {assert(is_valid_address(i)); return memory[i];}

  void set_state(int r, States s) const {assert(is_valid_address(r)); memory[r].state = s;}

public:
  
  weak_ref get_weak_ref(int r) const {assert(is_used(r) or is_marked(r)); return {r,memory[r].version};}
  bool inc_version() {version++; return not version;}

  bool is_null(int r) const {assert(is_valid_address(r)); return memory[r].state == none;}
  bool is_free(int r) const {assert(is_valid_address(r)); return memory[r].state == free;}
  bool is_used(int r) const {assert(is_valid_address(r)); return memory[r].state == used;}
  bool is_marked(int r) const {assert(is_valid_address(r)); return memory[r].state == marked;}
  void set_mark(int r) const {assert(is_used(r));set_state(r, marked);}
  void unmark(int r) const {assert(is_marked(r));set_state(r, used);}
  States get_state(int r) const {assert(is_valid_address(r)); return memory[r].state;}

  class PoolIterator
  {
    pool<T> *mypool;
    int index;
  public:
    // etc
    PoolIterator(pool * p, int i)
      :mypool(p), index(i)
    { }

    PoolIterator operator ++() { index = mypool->lookup(index).next; return *this;}
    PoolIterator operator --() { index = mypool->lookup(index).prev; return *this;}
    PoolIterator operator ++(int) { PoolIterator i = *this; index = mypool->lookup(index).next; return i;}
    PoolIterator operator --(int) { PoolIterator i = *this; index = mypool->lookup(index).prev; return i;}

    int addr() const {return index;}

    T& operator *() const
    {
      return (*mypool)[index];
    }

    T* operator->() const
    {
      return &(*mypool)[index];
    }

    bool operator ==(const PoolIterator &i2) const {assert(mypool == i2.mypool); return index == i2.index;}
    bool operator !=(const PoolIterator &i2) const {return not (*this == i2);}
  };

  class constPoolIterator
  {
    const pool<T> *mypool;
    int index;
  public:
    // etc
    constPoolIterator(const pool * p, int i)
      :mypool(p), index(i)
    { }

    constPoolIterator operator ++() { index = mypool->lookup(index).next; return *this;}
    constPoolIterator operator --() { index = mypool->lookup(index).prev; return *this;}
    constPoolIterator operator ++(int) { constPoolIterator i = *this; index = mypool->lookup(index).next; return i;}
    constPoolIterator operator --(int) { constPoolIterator i = *this; index = mypool->lookup(index).prev; return i;}

    int addr() const {return index;}

    const T& operator *() const
    {
      return (*mypool)[index];
    }

    const T* operator->() const
    {
      return &(*mypool)[index];
    }

    bool operator ==(const constPoolIterator &i2) const {assert(mypool == i2.mypool); return index == i2.index;}
    bool operator !=(const constPoolIterator &i2) const {return not (*this == i2);}
  };

  PoolIterator begin() {return {this,first_used};}
  PoolIterator end()   {return {this,-1};}

  constPoolIterator begin() const {return {this,first_used};}
  constPoolIterator end()   const {return {this,-1};}

  constPoolIterator cbegin() const {return {this,first_used};}
  constPoolIterator cend()   const {return {this,-1};}

  void clear()
  {
    while (first_used != -1)
      reclaim_used(first_used);
  }

  virtual void reclaim_used(int r)
  {
    // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
    remove_from_used_list(r);
    
    // lookup(r).value.clear()
    
    add_to_free_list(r);
  }

  void reclaim_unmarked()
  {
    for(int here = first_used; here != -1;)
    {
      int next = memory[here].next;
      if (is_marked(here))
	set_state(here, used);
      else 
	reclaim_used(here);
      
      here = next;
    }
  }

  virtual void get_more_memory()
  {
    if (memory.size() < n_used()*2+10)
      expand_memory(memory.size()*2+10);
  }

  int allocate()
  {
    // SLOW! assert(memory.size() == n_used() + n_free() + n_null());

    int r = get_free_element();

    // allocation failed
    if (r == -1)
    {
      get_more_memory();
      r = get_free_element();
      assert(r != -1);
    }

    add_to_used_list(r);

    // SLOW! assert(memory.size() == n_used() + n_free() + n_null());

    return r;
  }

  // These checks the VALUES.
  //  void check_used(int) const;
  //  void check_all_used() const;

  bool is_valid_address(int i) const {return n_null() <= i and i < size();}

  const T& access_unused(int i) const {return lookup(i).value;}
        T& access_unused(int i)       {return lookup(i).value;}

  const T& access(int i) const {assert(is_used(i) or is_marked(i));return lookup(i).value;}
        T& access(int i)       {assert(is_used(i) or is_marked(i));return lookup(i).value;}

  const T& operator[](int i) const {return access(i);}
        T& operator[](int i)       {return access(i);}

  pool()
  { }

  pool(int n)
    :n_null_(n)
  {
    // Add this number of null records.
    memory.resize(n);
  }
};

template <typename T>
class checked_pool: public pool<T>
{
public:

  int add_to_free_list(int r)
  {
    this->lookup(r).value.clear();
    
    return pool<T>::add_to_free_list(r);
  }

  int get_free_element()
  {
    int r = pool<T>::get_free_element();

#ifndef NDEBUG
    if (r != -1)
      this->lookup(r).value.check_cleared();
#endif

    return r;
  }

  checked_pool(): pool<T>() {}

  checked_pool(int n): pool<T>(n) {}
};


template <typename T>
class checked_pool_callback: public checked_pool<T>
{
public:

  void reclaim_used(int r)
  {
    // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
    this->remove_from_used_list(r);
    
    clear_references(r);
    
    this->add_to_free_list(r);
  }

  void get_more_memory()
  {
    collect_garbage();
    checked_pool<T>::get_more_memory();
  }

  std::function<void()> collect_garbage;

  std::function<void(int)> clear_references;

  checked_pool_callback(): checked_pool<T>() {}

  checked_pool_callback(int n): checked_pool<T>(n) {}
};


#endif
