//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2005-2008. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////
//
// This file comes from SGI's stl_set/stl_multiset files. Modified by Ion Gaztanaga 2004.
// Renaming, isolating and porting to generic algorithms. Pointer typedef 
// set to allocator::pointer to allow placing it in shared memory.
//
///////////////////////////////////////////////////////////////////////////////
/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#ifndef BOOST_INTERPROCESS_SET_HPP
#define BOOST_INTERPROCESS_SET_HPP

#if (defined _MSC_VER) && (_MSC_VER >= 1200)
#  pragma once
#endif

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/interprocess/detail/workaround.hpp>
#include <boost/interprocess/interprocess_fwd.hpp>

#include <utility>
#include <functional>
#include <memory>

#include <boost/interprocess/detail/move.hpp>
#include <boost/interprocess/detail/mpl.hpp>
#include <boost/interprocess/containers/detail/tree.hpp>
#include <boost/interprocess/detail/move.hpp>

namespace boost {   namespace interprocess {

/// @cond
// Forward declarations of operators < and ==, needed for friend declaration.
template <class T, class Pred, class Alloc>
inline bool operator==(const set<T,Pred,Alloc>& x, 
                       const set<T,Pred,Alloc>& y);

template <class T, class Pred, class Alloc>
inline bool operator<(const set<T,Pred,Alloc>& x, 
                      const set<T,Pred,Alloc>& y);
/// @endcond

//! A set is a kind of associative container that supports unique keys (contains at 
//! most one of each key value) and provides for fast retrieval of the keys themselves. 
//! Class set supports bidirectional iterators. 
//! 
//! A set satisfies all of the requirements of a container and of a reversible container 
//! , and of an associative container. A set also provides most operations described in 
//! for unique keys.
template <class T, class Pred, class Alloc>
class set 
{
   /// @cond
   private:
   typedef detail::rbtree<T, T, 
                     detail::identity<T>, Pred, Alloc> tree_t;
   tree_t m_tree;  // red-black tree representing set
   /// @endcond

   public:
   // typedefs:
   typedef typename tree_t::key_type               key_type;
   typedef typename tree_t::value_type             value_type;
   typedef typename tree_t::pointer                pointer;
   typedef typename tree_t::const_pointer          const_pointer;
   typedef typename tree_t::reference              reference;
   typedef typename tree_t::const_reference        const_reference;
   typedef Pred                                    key_compare;
   typedef Pred                                    value_compare;
   typedef typename tree_t::iterator               iterator;
   typedef typename tree_t::const_iterator         const_iterator;
   typedef typename tree_t::reverse_iterator       reverse_iterator;
   typedef typename tree_t::const_reverse_iterator const_reverse_iterator;
   typedef typename tree_t::size_type              size_type;
   typedef typename tree_t::difference_type        difference_type;
   typedef typename tree_t::allocator_type         allocator_type;
   typedef typename tree_t::stored_allocator_type  stored_allocator_type;

   //! <b>Effects</b>: Constructs an empty set using the specified comparison object 
   //! and allocator.
   //! 
   //! <b>Complexity</b>: Constant.
   explicit set(const Pred& comp = Pred(),
                const allocator_type& a = allocator_type())
      : m_tree(comp, a)
   {}

   //! <b>Effects</b>: Constructs an empty set using the specified comparison object and 
   //! allocator, and inserts elements from the range [first ,last ).
   //! 
   //! <b>Complexity</b>: Linear in N if the range [first ,last ) is already sorted using 
   //! comp and otherwise N logN, where N is last - first.
   template <class InputIterator>
   set(InputIterator first, InputIterator last, const Pred& comp = Pred(),
         const allocator_type& a = allocator_type())
      : m_tree(first, last, comp, a, true) 
   {}

   //! <b>Effects</b>: Copy constructs a set.
   //! 
   //! <b>Complexity</b>: Linear in x.size().
   set(const set<T,Pred,Alloc>& x) 
      : m_tree(x.m_tree)
   {}

   //! <b>Effects</b>: Move constructs a set. Constructs *this using x's resources.
   //! 
   //! <b>Complexity</b>: Construct.
   //! 
   //! <b>Postcondition</b>: x is emptied.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   set(const detail::moved_object<set<T,Pred,Alloc> >& x) 
      : m_tree(detail::move_impl(x.get().m_tree))
   {}
   #else
   set(set<T,Pred,Alloc> &&x) 
      : m_tree(detail::move_impl(x.m_tree))
   {}
   #endif

   //! <b>Effects</b>: Makes *this a copy of x.
   //! 
   //! <b>Complexity</b>: Linear in x.size().
   set<T,Pred,Alloc>& operator=(const set<T, Pred, Alloc>& x)
   {  m_tree = x.m_tree;   return *this;  }

   //! <b>Effects</b>: this->swap(x.get()).
   //! 
   //! <b>Complexity</b>: Constant.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   set<T,Pred,Alloc>& operator=(const detail::moved_object<set<T, Pred, Alloc> >& x)
   {  m_tree = detail::move_impl(x.get().m_tree);   return *this;  }
   #else
   set<T,Pred,Alloc>& operator=(set<T, Pred, Alloc> &&x)
   {  m_tree = detail::move_impl(x.m_tree);   return *this;  }
   #endif

   //! <b>Effects</b>: Returns the comparison object out
   //!   of which a was constructed.
   //! 
   //! <b>Complexity</b>: Constant.
   key_compare key_comp() const 
   { return m_tree.key_comp(); }

   //! <b>Effects</b>: Returns an object of value_compare constructed out
   //!   of the comparison object.
   //! 
   //! <b>Complexity</b>: Constant.
   value_compare value_comp() const 
   { return m_tree.key_comp(); }

   //! <b>Effects</b>: Returns a copy of the Allocator that
   //!   was passed to the object's constructor.
   //! 
   //! <b>Complexity</b>: Constant.
   allocator_type get_allocator() const 
   { return m_tree.get_allocator(); }

   const stored_allocator_type &get_stored_allocator() const 
   { return m_tree.get_stored_allocator(); }

   stored_allocator_type &get_stored_allocator()
   { return m_tree.get_stored_allocator(); }

   //! <b>Effects</b>: Returns an iterator to the first element contained in the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant
   iterator begin() 
   { return m_tree.begin(); }

   //! <b>Effects</b>: Returns a const_iterator to the first element contained in the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_iterator begin() const 
   { return m_tree.begin(); }

   //! <b>Effects</b>: Returns an iterator to the end of the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   iterator end() 
   { return m_tree.end(); }

   //! <b>Effects</b>: Returns a const_iterator to the end of the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_iterator end() const 
   { return m_tree.end(); }

   //! <b>Effects</b>: Returns a reverse_iterator pointing to the beginning 
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   reverse_iterator rbegin() 
   { return m_tree.rbegin(); } 

   //! <b>Effects</b>: Returns a const_reverse_iterator pointing to the beginning 
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_reverse_iterator rbegin() const 
   { return m_tree.rbegin(); } 

   //! <b>Effects</b>: Returns a reverse_iterator pointing to the end
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   reverse_iterator rend() 
   { return m_tree.rend(); }

   //! <b>Effects</b>: Returns a const_reverse_iterator pointing to the end
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_reverse_iterator rend() const 
   { return m_tree.rend(); }

   //! <b>Effects</b>: Returns true if the container contains no elements.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   bool empty() const 
   { return m_tree.empty(); }

   //! <b>Effects</b>: Returns the number of the elements contained in the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   size_type size() const 
   { return m_tree.size(); }

   //! <b>Effects</b>: Returns the largest possible size of the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   size_type max_size() const 
   { return m_tree.max_size(); }

   //! <b>Effects</b>: Swaps the contents of *this and x.
   //!   If this->allocator_type() != x.allocator_type() allocators are also swapped.
   //!
   //! <b>Throws</b>: Nothing.
   //!
   //! <b>Complexity</b>: Constant.
   void swap(set<T,Pred,Alloc>& x) 
   { m_tree.swap(x.m_tree); }

   //! <b>Effects</b>: Swaps the contents of *this and x.
   //!   If this->allocator_type() != x.allocator_type() allocators are also swapped.
   //!
   //! <b>Throws</b>: Nothing.
   //!
   //! <b>Complexity</b>: Constant.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   void swap(const detail::moved_object<set<T,Pred,Alloc> >& x) 
   { m_tree.swap(x.get().m_tree); }
   #else
   void swap(set<T,Pred,Alloc> &&x) 
   { m_tree.swap(x.m_tree); }
   #endif

   //! <b>Effects</b>: Inserts x if and only if there is no element in the container 
   //!   with key equivalent to the key of x.
   //!
   //! <b>Returns</b>: The bool component of the returned pair is true if and only 
   //!   if the insertion takes place, and the iterator component of the pair
   //!   points to the element with key equivalent to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic.
   std::pair<iterator,bool> insert(const value_type& x) 
   {  return m_tree.insert_unique(x);  }

   //! <b>Effects</b>: Move constructs a new value from x if and only if there is 
   //!   no element in the container with key equivalent to the key of x.
   //!
   //! <b>Returns</b>: The bool component of the returned pair is true if and only 
   //!   if the insertion takes place, and the iterator component of the pair
   //!   points to the element with key equivalent to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   std::pair<iterator,bool> insert(const detail::moved_object<value_type>& x) 
   {  return m_tree.insert_unique(x);  }
   #else
   std::pair<iterator,bool> insert(value_type &&x) 
   {  return m_tree.insert_unique(detail::move_impl(x));  }
   #endif

   //! <b>Effects</b>: Inserts a copy of x in the container if and only if there is 
   //!   no element in the container with key equivalent to the key of x.
   //!   p is a hint pointing to where the insert should start to search.
   //!
   //! <b>Returns</b>: An iterator pointing to the element with key equivalent
   //!   to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic in general, but amortized constant if t
   //!   is inserted right before p.
   iterator insert(const_iterator p, const value_type& x) 
   {  return m_tree.insert_unique(p, x); }

   //! <b>Effects</b>: Inserts an element move constructed from x in the container.
   //!   p is a hint pointing to where the insert should start to search.
   //!
   //! <b>Returns</b>: An iterator pointing to the element with key equivalent to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   iterator insert(const_iterator p, const detail::moved_object<value_type>& x) 
   {  return m_tree.insert_unique(p, x); }
   #else
   iterator insert(const_iterator p, value_type &&x) 
   {  return m_tree.insert_unique(p, detail::move_impl(x)); }
   #endif

   //! <b>Requires</b>: i, j are not iterators into *this.
   //!
   //! <b>Effects</b>: inserts each element from the range [i,j) if and only 
   //!   if there is no element with key equivalent to the key of that element.
   //!
   //! <b>Complexity</b>: N log(size()+N) (N is the distance from i to j)
   template <class InputIterator>
   void insert(InputIterator first, InputIterator last) 
   {  m_tree.insert_unique(first, last);  }

   //! <b>Effects</b>: Erases the element pointed to by p.
   //!
   //! <b>Returns</b>: Returns an iterator pointing to the element immediately
   //!   following q prior to the element being erased. If no such element exists, 
   //!   returns end().
   //!
   //! <b>Complexity</b>: Amortized constant time
   iterator erase(const_iterator p) 
   {  return m_tree.erase(p); }

   //! <b>Effects</b>: Erases all elements in the container with key equivalent to x.
   //!
   //! <b>Returns</b>: Returns the number of erased elements.
   //!
   //! <b>Complexity</b>: log(size()) + count(k)
   size_type erase(const key_type& x) 
   {  return m_tree.erase(x); }

   //! <b>Effects</b>: Erases all the elements in the range [first, last).
   //!
   //! <b>Returns</b>: Returns last.
   //!
   //! <b>Complexity</b>: log(size())+N where N is the distance from first to last.
   iterator erase(const_iterator first, const_iterator last) 
   {  return m_tree.erase(first, last);  }

   //! <b>Effects</b>: erase(a.begin(),a.end()).
   //!
   //! <b>Postcondition</b>: size() == 0.
   //!
   //! <b>Complexity</b>: linear in size().
   void clear() 
   { m_tree.clear(); }

   //! <b>Returns</b>: An iterator pointing to an element with the key
   //!   equivalent to x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic.
   iterator find(const key_type& x) 
   { return m_tree.find(x); }

   //! <b>Returns</b>: A const_iterator pointing to an element with the key
   //!   equivalent to x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic.
   const_iterator find(const key_type& x) const 
   { return m_tree.find(x); }

   //! <b>Returns</b>: The number of elements with key equivalent to x.
   //!
   //! <b>Complexity</b>: log(size())+count(k)
   size_type count(const key_type& x) const 
   {  return m_tree.find(x) == m_tree.end() ? 0 : 1;  }

   //! <b>Returns</b>: An iterator pointing to the first element with key not less
   //!   than k, or a.end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   iterator lower_bound(const key_type& x) 
   {  return m_tree.lower_bound(x); }

   //! <b>Returns</b>: A const iterator pointing to the first element with key not
   //!   less than k, or a.end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   const_iterator lower_bound(const key_type& x) const 
   {  return m_tree.lower_bound(x); }

   //! <b>Returns</b>: An iterator pointing to the first element with key not less
   //!   than x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   iterator upper_bound(const key_type& x)
   {  return m_tree.upper_bound(x);    }

   //! <b>Returns</b>: A const iterator pointing to the first element with key not
   //!   less than x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   const_iterator upper_bound(const key_type& x) const 
   {  return m_tree.upper_bound(x);    }

   //! <b>Effects</b>: Equivalent to std::make_pair(this->lower_bound(k), this->upper_bound(k)).
   //!
   //! <b>Complexity</b>: Logarithmic
   std::pair<iterator,iterator> 
      equal_range(const key_type& x) 
   {  return m_tree.equal_range(x); }

   //! <b>Effects</b>: Equivalent to std::make_pair(this->lower_bound(k), this->upper_bound(k)).
   //!
   //! <b>Complexity</b>: Logarithmic
   std::pair<const_iterator, const_iterator> 
      equal_range(const key_type& x) const 
   {  return m_tree.equal_range(x); }

   /// @cond
   template <class K1, class C1, class A1>
   friend bool operator== (const set<K1,C1,A1>&, const set<K1,C1,A1>&);

   template <class K1, class C1, class A1>
   friend bool operator< (const set<K1,C1,A1>&, const set<K1,C1,A1>&);
   /// @endcond
};

template <class T, class Pred, class Alloc>
inline bool operator==(const set<T,Pred,Alloc>& x, 
                       const set<T,Pred,Alloc>& y) 
{  return x.m_tree == y.m_tree;  }

template <class T, class Pred, class Alloc>
inline bool operator<(const set<T,Pred,Alloc>& x, 
                      const set<T,Pred,Alloc>& y) 
{  return x.m_tree < y.m_tree;   }

template <class T, class Pred, class Alloc>
inline bool operator!=(const set<T,Pred,Alloc>& x, 
                       const set<T,Pred,Alloc>& y) 
{  return !(x == y);   }

template <class T, class Pred, class Alloc>
inline bool operator>(const set<T,Pred,Alloc>& x, 
                      const set<T,Pred,Alloc>& y) 
{  return y < x; }

template <class T, class Pred, class Alloc>
inline bool operator<=(const set<T,Pred,Alloc>& x, 
                       const set<T,Pred,Alloc>& y) 
{  return !(y < x); }

template <class T, class Pred, class Alloc>
inline bool operator>=(const set<T,Pred,Alloc>& x, 
                       const set<T,Pred,Alloc>& y) 
{  return !(x < y);  }

#ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
template <class T, class Pred, class Alloc>
inline void swap(set<T,Pred,Alloc>& x, 
                 set<T,Pred,Alloc>& y) 
{  x.swap(y);  }

template <class T, class Pred, class Alloc>
inline void swap(set<T,Pred,Alloc>& x, 
                 detail::moved_object<set<T,Pred,Alloc> >& y) 
{  x.swap(y.get());  }

template <class T, class Pred, class Alloc>
inline void swap(detail::moved_object<set<T,Pred,Alloc> >& y, 
                 set<T,Pred,Alloc>& x) 
{  y.swap(x.get());  }

#else
template <class T, class Pred, class Alloc>
inline void swap(set<T,Pred,Alloc>&&x, 
                 set<T,Pred,Alloc>&&y) 
{  x.swap(y);  }
#endif

/// @cond

//!This class is movable
template <class T, class P, class A>
struct is_movable<set<T, P, A> >
{
   enum {   value = true };
};

//!has_trivial_destructor_after_move<> == true_type
//!specialization for optimizations
template <class T, class C, class A>
struct has_trivial_destructor_after_move<set<T, C, A> >
{
   enum {   value = 
               has_trivial_destructor<A>::value &&
               has_trivial_destructor<C>::value  };
};

// Forward declaration of operators < and ==, needed for friend declaration.

template <class T, class Pred, class Alloc>
inline bool operator==(const multiset<T,Pred,Alloc>& x, 
                       const multiset<T,Pred,Alloc>& y);

template <class T, class Pred, class Alloc>
inline bool operator<(const multiset<T,Pred,Alloc>& x, 
                      const multiset<T,Pred,Alloc>& y);
/// @endcond

//! A multiset is a kind of associative container that supports equivalent keys 
//! (possibly contains multiple copies of the same key value) and provides for 
//! fast retrieval of the keys themselves. Class multiset supports bidirectional iterators.
//! 
//! A multiset satisfies all of the requirements of a container and of a reversible 
//! container, and of an associative container). multiset also provides most operations 
//! described for duplicate keys.
template <class T, class Pred, class Alloc>
class multiset 
{
   /// @cond
   private:
   typedef detail::rbtree<T, T, 
                     detail::identity<T>, Pred, Alloc> tree_t;
   tree_t m_tree;  // red-black tree representing multiset
   /// @endcond

   public:
   // typedefs:
   typedef typename tree_t::key_type               key_type;
   typedef typename tree_t::value_type             value_type;
   typedef typename tree_t::pointer                pointer;
   typedef typename tree_t::const_pointer          const_pointer;
   typedef typename tree_t::reference              reference;
   typedef typename tree_t::const_reference        const_reference;
   typedef Pred                                    key_compare;
   typedef Pred                                    value_compare;
   typedef typename tree_t::iterator               iterator;
   typedef typename tree_t::const_iterator         const_iterator;
   typedef typename tree_t::reverse_iterator       reverse_iterator;
   typedef typename tree_t::const_reverse_iterator const_reverse_iterator;
   typedef typename tree_t::size_type              size_type;
   typedef typename tree_t::difference_type        difference_type;
   typedef typename tree_t::allocator_type         allocator_type;
   typedef typename tree_t::stored_allocator_type  stored_allocator_type;

   //! <b>Effects</b>: Constructs an empty multiset using the specified comparison
   //!   object and allocator.
   //! 
   //! <b>Complexity</b>: Constant.
   explicit multiset(const Pred& comp = Pred(),
                     const allocator_type& a = allocator_type())
      : m_tree(comp, a)
   {}

   //! <b>Effects</b>: Constructs an empty multiset using the specified comparison object
   //!   and allocator, and inserts elements from the range [first ,last ).
   //! 
   //! <b>Complexity</b>: Linear in N if the range [first ,last ) is already sorted using 
   //! comp and otherwise N logN, where N is last - first.
   template <class InputIterator>
   multiset(InputIterator first, InputIterator last,
            const Pred& comp = Pred(),
            const allocator_type& a = allocator_type())
      : m_tree(first, last, comp, a, false) 
   {}

   //! <b>Effects</b>: Copy constructs a multiset.
   //! 
   //! <b>Complexity</b>: Linear in x.size().
   multiset(const multiset<T,Pred,Alloc>& x) 
      : m_tree(x.m_tree)
   {}

   //! <b>Effects</b>: Move constructs a multiset. Constructs *this using x's resources.
   //! 
   //! <b>Complexity</b>: Construct.
   //! 
   //! <b>Postcondition</b>: x is emptied.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   multiset(const detail::moved_object<multiset<T,Pred,Alloc> >& x) 
      : m_tree(detail::move_impl(x.get().m_tree))
   {}
   #else
   multiset(multiset<T,Pred,Alloc> &&x) 
      : m_tree(detail::move_impl(x.m_tree))
   {}
   #endif

   //! <b>Effects</b>: Makes *this a copy of x.
   //! 
   //! <b>Complexity</b>: Linear in x.size().
   multiset<T,Pred,Alloc>& operator=(const multiset<T,Pred,Alloc>& x) 
   {  m_tree = x.m_tree;   return *this;  }

   //! <b>Effects</b>: this->swap(x.get()).
   //! 
   //! <b>Complexity</b>: Constant.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   multiset<T,Pred,Alloc>& operator=(const detail::moved_object<multiset<T,Pred,Alloc> >& x) 
   {  m_tree = detail::move_impl(x.get().m_tree);   return *this;  }
   #else
   multiset<T,Pred,Alloc>& operator=(multiset<T,Pred,Alloc> &&x) 
   {  m_tree = detail::move_impl(x.m_tree);   return *this;  }
   #endif

   //! <b>Effects</b>: Returns the comparison object out
   //!   of which a was constructed.
   //! 
   //! <b>Complexity</b>: Constant.
   key_compare key_comp() const 
   { return m_tree.key_comp(); }

   //! <b>Effects</b>: Returns an object of value_compare constructed out
   //!   of the comparison object.
   //! 
   //! <b>Complexity</b>: Constant.
   value_compare value_comp() const 
   { return m_tree.key_comp(); }

   //! <b>Effects</b>: Returns a copy of the Allocator that
   //!   was passed to the object's constructor.
   //! 
   //! <b>Complexity</b>: Constant.
   allocator_type get_allocator() const 
   { return m_tree.get_allocator(); }

   const stored_allocator_type &get_stored_allocator() const 
   { return m_tree.get_stored_allocator(); }

   stored_allocator_type &get_stored_allocator()
   { return m_tree.get_stored_allocator(); }

   //! <b>Effects</b>: Returns an iterator to the first element contained in the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   iterator begin() 
   { return m_tree.begin(); }

   //! <b>Effects</b>: Returns a const_iterator to the first element contained in the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_iterator begin() const 
   { return m_tree.begin(); }

   //! <b>Effects</b>: Returns an iterator to the end of the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   iterator end() 
   { return m_tree.end(); }

   //! <b>Effects</b>: Returns a const_iterator to the end of the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_iterator end() const 
   { return m_tree.end(); }

   //! <b>Effects</b>: Returns a reverse_iterator pointing to the beginning 
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   reverse_iterator rbegin() 
   { return m_tree.rbegin(); } 

   //! <b>Effects</b>: Returns a const_reverse_iterator pointing to the beginning 
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_reverse_iterator rbegin() const 
   { return m_tree.rbegin(); } 

   //! <b>Effects</b>: Returns a reverse_iterator pointing to the end
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   reverse_iterator rend() 
   { return m_tree.rend(); }

   //! <b>Effects</b>: Returns a const_reverse_iterator pointing to the end
   //! of the reversed container. 
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   const_reverse_iterator rend() const 
   { return m_tree.rend(); }

   //! <b>Effects</b>: Returns true if the container contains no elements.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   bool empty() const 
   { return m_tree.empty(); }

   //! <b>Effects</b>: Returns the number of the elements contained in the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   size_type size() const 
   { return m_tree.size(); }

   //! <b>Effects</b>: Returns the largest possible size of the container.
   //! 
   //! <b>Throws</b>: Nothing.
   //! 
   //! <b>Complexity</b>: Constant.
   size_type max_size() const 
   { return m_tree.max_size(); }

   //! <b>Effects</b>: Swaps the contents of *this and x.
   //!   If this->allocator_type() != x.allocator_type() allocators are also swapped.
   //!
   //! <b>Throws</b>: Nothing.
   //!
   //! <b>Complexity</b>: Constant.
   void swap(multiset<T,Pred,Alloc>& x) 
   { m_tree.swap(x.m_tree); }

   //! <b>Effects</b>: Swaps the contents of *this and x.
   //!   If this->allocator_type() != x.allocator_type() allocators are also swapped.
   //!
   //! <b>Throws</b>: Nothing.
   //!
   //! <b>Complexity</b>: Constant.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   void swap(const detail::moved_object<multiset<T,Pred,Alloc> >& x) 
   { m_tree.swap(x.get().m_tree); }
   #else
   void swap(multiset<T,Pred,Alloc> && x) 
   { m_tree.swap(x.m_tree); }
   #endif

   //! <b>Effects</b>: Inserts x and returns the iterator pointing to the
   //!   newly inserted element. 
   //!
   //! <b>Complexity</b>: Logarithmic.
   iterator insert(const value_type& x) 
   {  return m_tree.insert_equal(x);   }

   //! <b>Effects</b>: Inserts a copy of x in the container.
   //!
   //! <b>Returns</b>: An iterator pointing to the element with key equivalent
   //!   to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic in general, but amortized constant if t
   //!   is inserted right before p.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   iterator insert(const detail::moved_object<value_type>& x) 
   {  return m_tree.insert_equal(x);  }
   #else
   iterator insert(value_type && x) 
   {  return m_tree.insert_equal(detail::move_impl(x));  }
   #endif

   //! <b>Effects</b>: Inserts a copy of x in the container.
   //!   p is a hint pointing to where the insert should start to search.
   //!
   //! <b>Returns</b>: An iterator pointing to the element with key equivalent
   //!   to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic in general, but amortized constant if t
   //!   is inserted right before p.
   iterator insert(const_iterator p, const value_type& x) 
   {  return m_tree.insert_equal(p, x);  }

   //! <b>Effects</b>: Inserts a value move constructed from x in the container.
   //!   p is a hint pointing to where the insert should start to search.
   //!
   //! <b>Returns</b>: An iterator pointing to the element with key equivalent
   //!   to the key of x.
   //!
   //! <b>Complexity</b>: Logarithmic in general, but amortized constant if t
   //!   is inserted right before p.
   #ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
   iterator insert(const_iterator p, const detail::moved_object<value_type>& x) 
   {  return m_tree.insert_equal(p, x);  }
   #else
   iterator insert(const_iterator p, value_type && x) 
   {  return m_tree.insert_equal(p, detail::move_impl(x));  }
   #endif

   //! <b>Requires</b>: i, j are not iterators into *this.
   //!
   //! <b>Effects</b>: inserts each element from the range [i,j) .
   //!
   //! <b>Complexity</b>: N log(size()+N) (N is the distance from i to j)
   template <class InputIterator>
   void insert(InputIterator first, InputIterator last) 
   {  m_tree.insert_equal(first, last);  }

   //! <b>Effects</b>: Erases the element pointed to by p.
   //!
   //! <b>Returns</b>: Returns an iterator pointing to the element immediately
   //!   following q prior to the element being erased. If no such element exists, 
   //!   returns end().
   //!
   //! <b>Complexity</b>: Amortized constant time
   iterator erase(const_iterator p) 
   {  return m_tree.erase(p); }

   //! <b>Effects</b>: Erases all elements in the container with key equivalent to x.
   //!
   //! <b>Returns</b>: Returns the number of erased elements.
   //!
   //! <b>Complexity</b>: log(size()) + count(k)
   size_type erase(const key_type& x) 
   {  return m_tree.erase(x); }

   //! <b>Effects</b>: Erases all the elements in the range [first, last).
   //!
   //! <b>Returns</b>: Returns last.
   //!
   //! <b>Complexity</b>: log(size())+N where N is the distance from first to last.
   iterator erase(const_iterator first, const_iterator last)
   {  return m_tree.erase(first, last); }

   //! <b>Effects</b>: erase(a.begin(),a.end()).
   //!
   //! <b>Postcondition</b>: size() == 0.
   //!
   //! <b>Complexity</b>: linear in size().
   void clear() 
   { m_tree.clear(); }

   //! <b>Returns</b>: An iterator pointing to an element with the key
   //!   equivalent to x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic.
   iterator find(const key_type& x) 
   { return m_tree.find(x); }

   //! <b>Returns</b>: A const iterator pointing to an element with the key
   //!   equivalent to x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic.
   const_iterator find(const key_type& x) const 
   { return m_tree.find(x); }

   //! <b>Returns</b>: The number of elements with key equivalent to x.
   //!
   //! <b>Complexity</b>: log(size())+count(k)
   size_type count(const key_type& x) const 
   {  return m_tree.count(x);  }

   //! <b>Returns</b>: An iterator pointing to the first element with key not less
   //!   than k, or a.end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   iterator lower_bound(const key_type& x) 
   {  return m_tree.lower_bound(x); }

   //! <b>Returns</b>: A const iterator pointing to the first element with key not
   //!   less than k, or a.end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   const_iterator lower_bound(const key_type& x) const 
   {  return m_tree.lower_bound(x); }

   //! <b>Returns</b>: An iterator pointing to the first element with key not less
   //!   than x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   iterator upper_bound(const key_type& x)
   {  return m_tree.upper_bound(x);    }

   //! <b>Returns</b>: A const iterator pointing to the first element with key not
   //!   less than x, or end() if such an element is not found.
   //!
   //! <b>Complexity</b>: Logarithmic
   const_iterator upper_bound(const key_type& x) const 
   {  return m_tree.upper_bound(x);    }

   //! <b>Effects</b>: Equivalent to std::make_pair(this->lower_bound(k), this->upper_bound(k)).
   //!
   //! <b>Complexity</b>: Logarithmic
   std::pair<iterator,iterator> 
      equal_range(const key_type& x) 
   {  return m_tree.equal_range(x); }

   //! <b>Effects</b>: Equivalent to std::make_pair(this->lower_bound(k), this->upper_bound(k)).
   //!
   //! <b>Complexity</b>: Logarithmic
   std::pair<const_iterator, const_iterator> 
      equal_range(const key_type& x) const 
   {  return m_tree.equal_range(x); }

   /// @cond
   template <class K1, class C1, class A1>
   friend bool operator== (const multiset<K1,C1,A1>&,
                           const multiset<K1,C1,A1>&);
   template <class K1, class C1, class A1>
   friend bool operator< (const multiset<K1,C1,A1>&,
                          const multiset<K1,C1,A1>&);
   /// @endcond
};

template <class T, class Pred, class Alloc>
inline bool operator==(const multiset<T,Pred,Alloc>& x, 
                       const multiset<T,Pred,Alloc>& y) 
{  return x.m_tree == y.m_tree;  }

template <class T, class Pred, class Alloc>
inline bool operator<(const multiset<T,Pred,Alloc>& x, 
                      const multiset<T,Pred,Alloc>& y) 
{  return x.m_tree < y.m_tree;   }

template <class T, class Pred, class Alloc>
inline bool operator!=(const multiset<T,Pred,Alloc>& x, 
                       const multiset<T,Pred,Alloc>& y) 
{  return !(x == y);  }

template <class T, class Pred, class Alloc>
inline bool operator>(const multiset<T,Pred,Alloc>& x, 
                      const multiset<T,Pred,Alloc>& y) 
{  return y < x;  }

template <class T, class Pred, class Alloc>
inline bool operator<=(const multiset<T,Pred,Alloc>& x, 
                       const multiset<T,Pred,Alloc>& y) 
{  return !(y < x);  }

template <class T, class Pred, class Alloc>
inline bool operator>=(const multiset<T,Pred,Alloc>& x, 
                       const multiset<T,Pred,Alloc>& y) 
{  return !(x < y);  }

#ifndef BOOST_INTERPROCESS_RVALUE_REFERENCE
template <class T, class Pred, class Alloc>
inline void swap(multiset<T,Pred,Alloc>& x, 
                 multiset<T,Pred,Alloc>& y) 
{  x.swap(y);  }

template <class T, class Pred, class Alloc>
inline void swap(multiset<T,Pred,Alloc>& x, 
                 detail::moved_object<multiset<T,Pred,Alloc> >& y) 
{  x.swap(y.get());  }

template <class T, class Pred, class Alloc>
inline void swap(detail::moved_object<multiset<T,Pred,Alloc> >& y, 
                 multiset<T,Pred,Alloc>& x) 
{  y.swap(x.get());  }
#else
template <class T, class Pred, class Alloc>
inline void swap(multiset<T,Pred,Alloc>&&x, 
                 multiset<T,Pred,Alloc>&&y) 
{  x.swap(y);  }
#endif

/// @cond

//!This class is movable
template <class T, class P, class A>
struct is_movable<multiset<T, P, A> >
{
   enum {   value = true };
};

//!has_trivial_destructor_after_move<> == true_type
//!specialization for optimizations
template <class T, class C, class A>
struct has_trivial_destructor_after_move<multiset<T, C, A> >
{
   enum {   value = 
               has_trivial_destructor<A>::value &&
               has_trivial_destructor<C>::value  };
};
/// @endcond

}} //namespace boost {   namespace interprocess {

#include <boost/interprocess/detail/config_end.hpp>

#endif /* BOOST_INTERPROCESS_SET_HPP */

