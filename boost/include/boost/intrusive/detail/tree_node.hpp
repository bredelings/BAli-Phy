/////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga  2007.
//
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/intrusive for documentation.
//
/////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTRUSIVE_TREE_NODE_HPP
#define BOOST_INTRUSIVE_TREE_NODE_HPP

#include <boost/intrusive/detail/config_begin.hpp>
#include <iterator>
#include <boost/intrusive/detail/pointer_to_other.hpp>
#include <boost/intrusive/detail/mpl.hpp>

namespace boost {
namespace intrusive {

template<class VoidPointer>
struct tree_node
{
   typedef typename pointer_to_other
      <VoidPointer
      ,tree_node<VoidPointer> >::type   node_ptr;

   node_ptr parent_, left_, right_;
};

template<class VoidPointer>
struct tree_node_traits
{
   typedef tree_node<VoidPointer> node;

   typedef typename boost::pointer_to_other
      <VoidPointer, node>::type              node_ptr;
   typedef typename boost::pointer_to_other
      <VoidPointer, const node>::type        const_node_ptr;

   static node_ptr get_parent(const_node_ptr n)
   {  return n->parent_;  }

   static void set_parent(node_ptr n, node_ptr p)
   {  n->parent_ = p;  }

   static node_ptr get_left(const_node_ptr n)
   {  return n->left_;  }

   static void set_left(node_ptr n, node_ptr l)
   {  n->left_ = l;  }

   static node_ptr get_right(const_node_ptr n)
   {  return n->right_;  }

   static void set_right(node_ptr n, node_ptr r)
   {  n->right_ = r;  }
};

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
//                   Implementation of the tree iterator                   //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

// tree_iterator provides some basic functions for a 
// node oriented bidirectional iterator:
template<class Container, bool IsConst>
class tree_iterator
   :  public std::iterator
         < std::bidirectional_iterator_tag
         , typename detail::add_const_if_c
            <typename Container::value_type, IsConst>::type
         >
{
   protected:
   typedef typename Container::real_value_traits   real_value_traits;
   typedef typename Container::node_algorithms     node_algorithms;
   typedef typename real_value_traits::node_traits node_traits;
   typedef typename node_traits::node              node;
   typedef typename node_traits::node_ptr          node_ptr;
   typedef typename boost::pointer_to_other
      <node_ptr, void>::type                       void_pointer;
   static const bool store_container_ptr = 
      detail::store_cont_ptr_on_it<Container>::value;

   public:
   public:
   typedef typename detail::add_const_if_c
      <typename Container::value_type, IsConst>
      ::type                                       value_type;
   typedef value_type & reference;
   typedef value_type * pointer;

   tree_iterator()
      : members_ (0, 0)
   {}

   explicit tree_iterator(node_ptr node, const Container *cont_ptr)
      : members_ (node, cont_ptr)
   {}

   tree_iterator(tree_iterator<Container, false> const& other)
      :  members_(other.pointed_node(), other.get_container())
   {}

   const node_ptr &pointed_node() const
   { return members_.nodeptr_; }

   tree_iterator &operator=(const node_ptr &node)
   {  members_.nodeptr_ = node;  return static_cast<tree_iterator&>(*this);  }

   public:
   tree_iterator& operator++() 
   { 
      members_.nodeptr_ = node_algorithms::next_node(members_.nodeptr_); 
      return static_cast<tree_iterator&> (*this); 
   }
   
   tree_iterator operator++(int)
   {
      tree_iterator result (*this);
      members_.nodeptr_ = node_algorithms::next_node(members_.nodeptr_);
      return result;
   }

   tree_iterator& operator--() 
   { 
      members_.nodeptr_ = node_algorithms::prev_node(members_.nodeptr_); 
      return static_cast<tree_iterator&> (*this); 
   }
   
   tree_iterator operator--(int)
   {
      tree_iterator result (*this);
      members_.nodeptr_ = node_algorithms::prev_node(members_.nodeptr_);
      return result;
   }

   bool operator== (const tree_iterator& i) const
   { return members_.nodeptr_ == i.pointed_node(); }

   bool operator!= (const tree_iterator& i) const
   { return !operator== (i); }

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

   tree_iterator end_iterator_from_it() const
   {
      return tree_iterator(node_algorithms::get_header(this->pointed_node()), this->get_container());
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

#endif //BOOST_INTRUSIVE_TREE_NODE_HPP
