/////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Olaf Krzikalla 2004-2006.
// (C) Copyright Ion Gaztanaga  2006-2007
//
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/intrusive for documentation.
//
/////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTRUSIVE_SLIST_NODE_HPP
#define BOOST_INTRUSIVE_SLIST_NODE_HPP

#include <boost/intrusive/detail/config_begin.hpp>
#include <iterator>
#include <boost/intrusive/detail/assert.hpp>
#include <boost/intrusive/detail/pointer_to_other.hpp>

namespace boost {
namespace intrusive {

template<class VoidPointer>
struct slist_node
{
   typedef typename boost::pointer_to_other
      <VoidPointer, slist_node>::type   node_ptr;
   node_ptr next_;
};

// slist_node_traits can be used with circular_slist_algorithms and supplies
// a slist_node holding the pointers needed for a singly-linked list
// it is used by slist_base_hook and slist_member_hook
template<class VoidPointer>
struct slist_node_traits
{
   typedef slist_node<VoidPointer> node;
   typedef typename boost::pointer_to_other
      <VoidPointer, node>::type          node_ptr;
   typedef typename boost::pointer_to_other
      <VoidPointer, const node>::type    const_node_ptr;

   static node_ptr get_next(const_node_ptr n)
   {  return n->next_;  }  

   static void set_next(node_ptr n, node_ptr next)
   {  n->next_ = next;  }  
};

// slist_iterator provides some basic functions for a 
// node oriented bidirectional iterator:
template<class Container, bool IsConst>
class slist_iterator
   :  public std::iterator
         < std::forward_iterator_tag
         , typename detail::add_const_if_c
            <typename Container::value_type, IsConst>::type
         >
{
   protected:
   typedef typename Container::real_value_traits   real_value_traits;
   typedef typename real_value_traits::node_traits node_traits;
   typedef typename node_traits::node              node;
   typedef typename node_traits::node_ptr          node_ptr;
   typedef typename boost::pointer_to_other
      <node_ptr, void>::type                       void_pointer;
   static const bool store_container_ptr = 
      detail::store_cont_ptr_on_it<Container>::value;

   public:
   typedef typename detail::add_const_if_c
      <typename Container::value_type, IsConst>
      ::type                                       value_type;
   typedef value_type & reference;
   typedef value_type * pointer;

   slist_iterator()
      : members_ (node_ptr(0), 0)
   {}

   explicit slist_iterator(node_ptr node, const Container *cont_ptr)
      : members_ (node, cont_ptr)
   {}

   slist_iterator(slist_iterator<Container, false> const& other)
      :  members_(other.pointed_node(), other.get_container())
   {}

   const node_ptr &pointed_node() const
   { return members_.nodeptr_; }

   slist_iterator &operator=(const node_ptr &node)
   {  members_.nodeptr_ = node;  return static_cast<slist_iterator&>(*this);  }

   public:
   slist_iterator& operator++() 
   { 
      members_.nodeptr_ = node_traits::get_next(members_.nodeptr_); 
      return static_cast<slist_iterator&> (*this); 
   }
   
   slist_iterator operator++(int)
   {
      slist_iterator result (*this);
      members_.nodeptr_ = node_traits::get_next(members_.nodeptr_);
      return result;
   }

   bool operator== (const slist_iterator& i) const
   {  return members_.nodeptr_ == i.pointed_node();   }

   bool operator!= (const slist_iterator& i) const
   {  return !operator== (i); }

   value_type& operator*() const
   {  return *operator->();   }

   pointer operator->() const
   { return detail::get_pointer(this->get_real_value_traits()->to_value_ptr(members_.nodeptr_)); }

   const Container *get_container() const
   {
      if(store_container_ptr)
         return static_cast<const Container*>(members_.get_ptr());
      else
         return 0;
   }

   const real_value_traits *get_real_value_traits() const
   {
      if(store_container_ptr)
         return &this->get_container()->get_real_value_traits();
      else
         return 0;
   }

   private:
   struct members
      :  public detail::select_constptr
         <void_pointer, store_container_ptr>::type
   {
      typedef typename detail::select_constptr
         <void_pointer, store_container_ptr>::type Base;

      members(const node_ptr &n_ptr, const void *cont)
         :  Base(cont), nodeptr_(n_ptr)
      {}

      node_ptr nodeptr_;
   } members_;
};

} //namespace intrusive 
} //namespace boost 

#include <boost/intrusive/detail/config_end.hpp>

#endif //BOOST_INTRUSIVE_SLIST_NODE_HPP
