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
  /// The actual storage.
  std::list<T> L;

  /// How many entries on the used list?
  int n_used = 0;

  /// How many entries on the free list?
  int n_free = 0;

  /// This points to the first free node.
  typename std::list<T>::iterator begin_free;

public:
  typedef typename std::list<T>::iterator iterator;
  typedef typename std::list<T>::const_iterator const_iterator;

  int size() const {return n_used;}

  const_iterator begin() const 
  {
    return L.begin();
  }

  iterator begin()
  {
    return L.begin();
  }

  const_iterator end() const {return begin_free;}
  iterator end() {return begin_free;}

  /// Remove an element from the list. (Place it in the free area.)
  void erase(iterator loc) 
  {
    assert(loc != begin_free);

    // Move (used) element from loc to *before* begin_free.
    L.splice(begin_free, L, loc);
    // Adjust begin_free to point to the new element.
    begin_free = loc;

    n_used--;
    n_free++;
  }

  /// Insert the value t *before* loc.
  iterator insert(iterator loc, const T& t)
  {
    if (begin_free == L.end())
    {
      assert(n_free == 0);
      n_used++;
      return L.insert(begin_free, t);
    }
    assert(n_free > 0);

    // Set the value
    iterator node = begin_free;
    *node = t;

    // Move begin_free to the next free value
    begin_free++;

    n_used++;
    n_free--;

    // Move (used) element from loc to *before* begin_free.
    L.splice(loc, L, node);

    return node;
  }

  const T& back() const
  {
    iterator last = begin_free;
    last--;
    return *last;
  }

  T& back()
  {
    iterator last = begin_free;
    last--;
    return *last;
  }

  void pop_back()
  {
    assert(size());

    iterator last = begin_free;
    last--;
    erase(last);
  }

  void push_back(const T& t)
  {
    insert(end(), t);
  }

  void pop_front()
  {
    assert(size());

    erase(begin());
  }

  void push_front(const T& t)
  {
    insert(begin(), t);
  }

  bool empty() const
  {
    return not n_used;
  }

  int count(const T& t) const
  {
    int total = 0;
    for(const auto& i: *this)
      if (i == t)
	total++;
    return total;
  }

  void clear() 
  {
    n_free += n_used;
    n_used = 0;

    assert(n_free == L.size());

    begin_free = L.begin();
  }

  int capacity() const
  {
    assert(n_used + n_free = L.size());
    return L.size();
  }

  void swap(CacheList<T>& CL2) noexcept
  {
    bool bf_is_end = (begin_free == L.end());
    bool CL2_bf_is_end = (CL2.begin_free == CL2.L.end());
      
    std::swap(L, CL2.L);
    std::swap(n_used, CL2.n_used);
    std::swap(n_free, CL2.n_free);

    std::swap(begin_free, CL2.begin_free);
    if (CL2_bf_is_end)
      begin_free = L.end();
    if (bf_is_end)
      CL2.begin_free = CL2.L.end();
  }

  CacheList<T>& operator=(std::initializer_list<T> L2)
  {
    L = L2;
    begin_free = L.end();
  }

  CacheList<T>& operator=(const CacheList<T>& CL2)
  {
    clear();
    for(auto i: CL2)
      push_back(i);
    return *this;
  }

  CacheList<T>& operator=(CacheList<T>&& CL2) noexcept
  {
    swap(CL2);

    return *this;
  }

  CacheList()
    :begin_free(L.end())
  { }

  CacheList(const std::list<T>& L2)
    :L(L2),
     n_used(L.size()),
     n_free(0),
     begin_free(L.end())
  { }

  CacheList(const std::list<T>&& L2)
    :L(L2),
     n_used(L.size()),
     n_free(0),
     begin_free(L.end())
  { }

  CacheList(std::initializer_list<T> L2)
    :L(L2),
     n_used(L.size()),
     n_free(0),
     begin_free(L.end())
  { }

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
};

#endif
