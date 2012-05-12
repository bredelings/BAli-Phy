#ifndef ARRAY_LIST
#define ARRAY_LIST

#include <cassert>
#include <initializer_list>
// Currently, since I'm not using T*, this only works for POD type.
// Perhaps I should clear list entries by setting their x equal to T()?

template <typename T>
class ArrayList
{
  struct list_node
  {
    T element;
    list_node* prev = nullptr;
    list_node* next = nullptr;
  };

  // NOTE: The list will be circular.

  /// This points to the array.  Total size is used_size + free_size.
  list_node* nodes = nullptr;

  /// This points to a node before the used nodes, and after the free nodes.
  list_node* before_used = nullptr;
  /// How many entries on the used list?
  int n_used = 0;

  /// This points to a node before the free nodes, and after the used nodes.
  list_node* before_free = nullptr;
  /// How many entries on the free list?
  int n_free = 0;

  // Unlink this node from its current location.
  static void remove_node(list_node* node)
  {
    list_node* prev = node->prev;
    list_node* next = node->next;

    // Remove the node from its place in the chain
    prev->next = next;
    next->prev = prev;
  }

  // Link a node into a new location.
  static void insert_node_before(list_node* node, list_node* here)
  {
    list_node* prev = here->prev;

    prev->next = node;
    here->prev = node;

    node->next = here;
    node->prev = prev;
  }

  // Link a node into a new location.
  static void insert_node_after(list_node* node, list_node* here)
  {
    list_node* next = here->next;

    here->next = node;
    next->prev = node;

    node->next = next;
    node->prev = here;
  }

  void expand_memory(int s)
  {
    int new_size = n_used + n_free + 2 + s;
    list_node* new_nodes = new list_node[new_size];

    // Chain the pointers along the entire chain...
    for(int i=0;i<new_size;i++)
    {
      new_nodes[i].prev = new_nodes+(i-1);
      new_nodes[i].next = new_nodes+(i+1);
    }
    // ... remembering to fix up the ends.
    new_nodes[0].prev = new_nodes+(new_size-1);
    new_nodes[new_size-1].next = new_nodes;

    // Copy the elements from the old array.
    if (n_used)
    {
      int i=0;
      for(list_node* ptr = before_used->next;ptr != before_free; ptr = ptr->next,i++)
	new_nodes[i].element = ptr->element;
    }

    // set the boundary markers.
    before_used = new_nodes;
    before_free = new_nodes + 1 + n_used;
    n_free += s;

    // Delete the old storage and switch to the new storage
    delete nodes;
    nodes = new_nodes;
  }

public:
  struct iterator;
  struct const_iterator;
  friend struct iterator;
  friend struct const_iterator;

  struct iterator
  {
    // Fix this later to just have a single 
    list_node* ptr;

    iterator operator++(int)
    {
      list_node* tmp = ptr;
      ptr = ptr->next;
      return tmp;
    }

    iterator& operator++() 
    {
      ptr = ptr->next;
      return *this;
    }

    iterator operator--(int)
    {
      list_node* tmp = ptr;
      ptr = ptr->prev;
      return tmp;
    }

    iterator& operator--()
    {
      ptr = ptr->prev;
      return *this;
    }

    bool operator==(const iterator& iter)
    {
      return ptr == iter.ptr;
    }

    bool operator!=(const iterator& iter)
    {
      return ptr != iter.ptr;
    }

    bool operator==(const const_iterator& iter)
    {
      return ptr == iter.ptr;
    }

    bool operator!=(const const_iterator& iter)
    {
      return ptr != iter.ptr;
    }

    T& operator*() const {return ptr->element;}
    T* operator->() const {return &(ptr->element);}

    iterator(list_node* p):ptr(p) { }
  };

  struct const_iterator
  {
    // Fix this later to just have a single 
    const list_node* ptr;

    const_iterator operator++(int)
    {
      const list_node* tmp = ptr;
      ptr = ptr->next;
      return tmp;
    }

    const_iterator& operator++() 
    {
      ptr = ptr->next;
      return *this;
    }

    const_iterator operator--(int)
    {
      const list_node* tmp = ptr;
      ptr = ptr->prev;
      return tmp;
    }

    const_iterator& operator--()
    {
      ptr = ptr->prev;
      return *this;
    }

    bool operator==(const iterator& iter)
    {
      return ptr == iter.ptr;
    }

    bool operator!=(const iterator& iter)
    {
      return ptr != iter.ptr;
    }

    bool operator==(const const_iterator& iter)
    {
      return ptr == iter.ptr;
    }

    bool operator!=(const const_iterator& iter)
    {
      return ptr != iter.ptr;
    }

    const T& operator*() const {return ptr->element;}
    const T* operator->() const {return &(ptr->element);}

    const_iterator(const list_node* p):ptr(p) { }
  };

  int size() const {return n_used;}

  const_iterator begin() const {return before_used->next;}
  iterator begin() {return before_used->next;}

  const_iterator end() const {return before_free;}
  iterator end() {return before_free;}

  /// Remove an element from the list. (Place it in the free area.)
  void erase(iterator loc) 
  {
    list_node* node = loc.ptr;

    assert(node != before_used);
    assert(node != before_free);

    // Remove node from the used list
    remove_node(node);
    n_used--;

    // Add it to the end of the free list
    insert_node_before(node, before_free);
    n_free++;
  }

private:
  void insert_no_check(iterator loc, const T& t)
  {
    assert(n_free > 0);

    list_node* node = before_free->next;
    list_node* here = loc.ptr;

    // Remove node from the free list
    remove_node(before_free->next);
    n_free--;

    // Add it to the correct place in the used list
    insert_node_before(node, here);
    n_used++;

    // Set the value to the right value
    node->element = t;
  }

public:
  /// Insert the value t before loc.
  void insert(iterator loc, const T& t)
  {
    if (not n_free)
      expand_memory(n_used + 10);

    insert_no_check(loc,t);
  }

  void pop_back()
  {
    iterator loc = end;
    end--;
    erase(loc);
  }

  void push_back(const T& t)
  {
    if (not n_free)
      expand_memory(n_used + 10);

    insert_no_check(end(), t);
  }

  void pop_front()
  {
    erase(begin());
  }

  void push_front(const T& t)
  {
    if (not n_free)
      expand_memory(n_used + 10);

    insert_no_check(begin(), t);
  }

  bool empty() const
  {
    return not n_used;
  }

  void clear() 
  {
    // In theory, we should destroy or nullify all the nodes on the used list.
    n_free += n_used;
    n_used = 0;

    // reset end t to right after begin
    before_free = before_used->next;
  }

  void reserve(int s)
  {
    if (s <= size()) return;

    expand_memory(s - size());
  }

  ArrayList<T>& operator=(std::initializer_list<T> L)
  {
    for(auto t: L)
      push_back(t);
  }

  ArrayList<T>& operator=(const ArrayList<T>& L)
  {
    // We could speed this up.
    clear();
    reserve(L.size() * 2);
    for(auto i: L)
      push_back(i);
    return *this;
  }

  ArrayList<T>& operator=(ArrayList<T>&& L)
  {
    // Share the other guy's resources
    nodes = L.nodes;
    before_used = L.before_used;
    before_free = L.before_free;
    n_used = L.n_used;
    n_free = L.n_free;

    // Then take sole ownership of them
    L.nodes = nullptr;
    L.before_used = nullptr;
    L.before_free = nullptr;
    L.n_used = 0;
    L.n_free = 0;

    return *this;
  }

  ArrayList()
  { }

  ArrayList(std::initializer_list<T> L)
  {
    operator=(L);
  }

  ArrayList(const ArrayList<T>& L)
  {
    operator=(L);
  }

  ArrayList(const ArrayList<T>&& L)
    :nodes(L.nodes),
     before_used(L.before_used),
     before_free(L.before_free),
     n_used(L.n_used),
     n_free(L.n_free)
  { 
    L.nodes = nullptr;
    L.before_used = nullptr;
    L.before_free = nullptr;
    L.n_used = 0;
    L.n_free = 0;
  }


  ~ArrayList()
  {
    delete nodes;
  }
};

#endif
