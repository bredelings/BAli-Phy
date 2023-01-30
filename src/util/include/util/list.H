#ifndef UTIL_LIST_H
#define UTIL_LIST_H

#include <variant>
#include "util/variant.H"
#include <iterator> // For std::forward_iterator_

template <typename T>
struct IList
{
    struct Nil{};

    struct Cons
    {
        const T head;
        const IList<T> tail;
    };

    using NilOrCons = std::variant<Nil,Cons>;

    typedef std::shared_ptr<const NilOrCons> node_pointer;

private:
    node_pointer list = std::make_shared<NilOrCons>(Nil());

public:
    bool empty() const
    {
        return to<Nil>(*list);
    };

    node_pointer ptr() const
    {
        return list;
    }

    const T& front() const
    {
        assert(not empty());
        return to<Cons>(*list)->head;
    };

    [[nodiscard]] IList<T> pop_front() const
    {
        assert(not empty());
        return to<Cons>(*list)->tail;
    }

    [[nodiscard]] IList<T> push_front(const T& t) const
    {
        return {std::make_shared<const NilOrCons>(Cons{t,*this})};
    }

    IList() = default;

    IList(const node_pointer& p):list(p) {}

    IList(const std::initializer_list<T>& xs)
    {
        for(auto it = std::rbegin(xs); it != std::rend(xs); it++)
            (*this) = push_front(*it);
    }

    struct Iterator
    {
        using iterator_category = std::forward_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = T;
        using pointer           = const T*;      // or also value_type*
        using reference         = const T&;      // or also value_type&

        Iterator(node_pointer p) : node(p) {}

        operator bool () const
        {
            return (bool)to<Cons>(*node);
        }

        reference operator*() const
        {
            auto cons = to<Cons>(*node);
            assert(cons);
            return cons->head;
        }

        pointer operator->() const
        {
            auto cons = to<Cons>(*node);
            assert(cons);
            return &(cons->head);
        }

        // Prefix increment
        Iterator& operator++()
        {
            auto cons = to<Cons>(*node);
            assert(cons);
            node = cons->tail.ptr();
            return *this;
        }

        // Postfix increment
        Iterator operator++(int) { Iterator tmp = *this; ++(*this); return tmp; }

        friend bool operator== (const Iterator& a, const Iterator& b) { return a.node == b.node; };

    private:

        node_pointer node;

    };

    Iterator begin() const {return Iterator(list);}

    struct Sentinel {
        friend bool operator==(const Iterator& a, const Sentinel&) {return (bool)a;}
    };

    Sentinel end() const
    {
        return Sentinel();
    }
};


#endif
