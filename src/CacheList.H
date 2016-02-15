#ifndef CACHE_LIST
#define CACHE_LIST

#include <list>
#include <cassert>
#include <initializer_list>
// Currently, since I'm not using T*, this only works for POD type.
// Perhaps I should clear list entries by setting their x equal to T()?

template <typename T>
class CacheList
{
  /// This points to the first free node.
  static std::list<T> free_pool;

  /// The actual storage.
  std::list<T> L;

public:
  typedef typename std::list<T>::iterator iterator;
  typedef typename std::list<T>::const_iterator const_iterator;

  int size() const {return L.size();}

  const_iterator begin() const 
  {
    return L.begin();
  }

  iterator begin()
  {
    return L.begin();
  }

  const_iterator end() const 
  {
    return L.end();
  }

  iterator end() {
    return L.end();
  }

  /// Remove an element from the list. (Place it in the free area.)
  void erase(iterator loc) 
  {
    // Move (used) element from loc to the free pool.
    free_pool.splice(free_pool.begin(), L, loc);
  }

  /// Insert the value t *before* loc.
  iterator insert(const_iterator loc, const T& t)
  {
    if (free_pool.empty())
      return L.insert(loc, t);
    else
    {
      iterator free_loc = free_pool.begin();
      
      // Set the value
      *free_loc = t;

      // Move (used) element from loc to *before* begin_free.
      L.splice(loc, free_pool, free_loc);

      return free_loc;
    }
  }

  void splice(const_iterator loc, CacheList<T>& CL2)
  {
    L.splice(loc, CL2.L);
  }

  const T& back() const
  {
    return L.back();
  }

  T& back()
  {
    return L.back();
  }

  void pop_back()
  {
    assert(not empty());

    iterator last = end();
    last--;
    erase(last);
  }

  iterator push_back(const T& t)
  {
    return insert(end(), t);
  }

  void pop_front()
  {
    assert(not empty());

    erase(begin());
  }

  iterator push_front(const T& t)
  {
    return insert(begin(), t);
  }

  bool empty() const
  {
    return L.empty();
  }

  int count(const T& t) const
  {
    int total = 0;
    for(const auto& i: L)
      if (i == t)
	total++;
    return total;
  }

  void clear() 
  {
    free_pool.splice(free_pool.begin(), L);
  }

  void swap(CacheList<T>& CL2) noexcept
  {
    L.swap(CL2.L);
  }

  CacheList<T>& operator=(const std::list<T>& L2)
  {
    clear();
    for(auto i: L2)
      push_back(i);
    return *this;
  }

  CacheList<T>& operator=(const CacheList<T>& CL2)
  {
    return operator=(CL2.L);
  }

  CacheList<T>& operator=(std::list<T>&& L2) noexcept
  {
    clear();
    L.swap(L2);
    return *this;
  }

  CacheList<T>& operator=(CacheList<T>&& CL2) noexcept
  {
    return operator=(std::move(CL2.L));
  }

  CacheList() {}

  CacheList(const std::list<T>& L2)
  {
    operator=(L2);
  }

  CacheList(const std::list<T>&& L2)
  { 
    operator=(std::move(L2));
  }

  CacheList(std::initializer_list<T> IL2)
  { 
    for(auto i: IL2)
      push_back(i);
  }

  CacheList(const CacheList<T>& L)
    :CacheList()
  {
    operator=(L);
  }

  CacheList(CacheList<T>&& L) noexcept
    :CacheList()
  { 
    swap(L);
  }

  ~CacheList()
  {
    clear();
  }
};

template<typename T> std::list<T> CacheList<T>::free_pool = std::list<T>{};

#endif
