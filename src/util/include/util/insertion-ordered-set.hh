#ifndef UTIL_INSERTION_ORDERED_SET_H
#define UTIL_INSERTION_ORDERED_SET_H

#include <cstddef>
#include <list>
#include <unordered_map>

/// A set that is walked in the order in which its members were inserted.
///
/// std::unordered_set<int> is walked in an order that depends on the values of its members and on
/// its bucket count.  When the members are indices into the machine's heap, that order changes with
/// anything that changes how the heap is laid out, and so does any result that depends on the order.
/// Insertion order depends only on the order in which things happen.
template <typename T>
class insertion_ordered_set
{
    std::list<T> members;
    std::unordered_map<T, typename std::list<T>::iterator> position;

public:
    typedef T value_type;
    typedef typename std::list<T>::const_iterator const_iterator;
    typedef const_iterator iterator;

    const_iterator begin() const {return members.begin();}
    const_iterator end()   const {return members.end();}

    bool empty() const {return members.empty();}
    std::size_t size() const {return members.size();}
    std::size_t count(const T& t) const {return position.count(t);}

    /// Add t at the end, unless it is already a member.
    bool insert(const T& t)
    {
        if (position.count(t)) return false;
        position.insert({t, members.insert(members.end(), t)});
        return true;
    }

    std::size_t erase(const T& t)
    {
        auto it = position.find(t);
        if (it == position.end()) return 0;
        members.erase(it->second);
        position.erase(it);
        return 1;
    }

    void clear()
    {
        members.clear();
        position.clear();
    }

    insertion_ordered_set() = default;

    // The positions point into this object's own list, so they cannot be copied as they are.
    insertion_ordered_set(const insertion_ordered_set& other)
    {
        for(auto& t: other.members)
            insert(t);
    }

    insertion_ordered_set& operator=(const insertion_ordered_set& other)
    {
        if (this != &other)
        {
            clear();
            for(auto& t: other.members)
                insert(t);
        }
        return *this;
    }

    // Moving a std::list keeps iterators to its elements valid.
    insertion_ordered_set(insertion_ordered_set&&) = default;
    insertion_ordered_set& operator=(insertion_ordered_set&&) = default;
};

#endif
