#ifndef BITMASK_H
#define BITMASK_H

#include "util/assert.hh"

template <typename T>
class bitmask
{
    T data=0;
public:
    bitmask() = default;
    bitmask(T d):data(d) {}
    T raw() const {return data;}

    bool test(int i) const { return data & (1<<i); }
    bool any() const {return data;}
    bool all() const {return ~data;}
    bool none() const {return !data;}

    std::size_t size() const {return 8*sizeof(T);}

    bitmask<T>& reset()
    {
        data = 0;
        return *this;
    }

    bitmask<T>& reset(int i)
    {
        assert(i >=0 and i < size());
        data &= ~(1<<i);
        return *this;
    }

    bitmask<T>& set()
    {
        data = ~(T(0));
    }

    bitmask<T>& set(int i)
    {
        assert(i >=0 and i < size());
        data |= (1<<i);
        return *this;
    }

    bitmask<T>& set(int i, bool b)
    {
        assert(i >=0 and i < size());
        if (b)
            set(i);
        else
            reset(i);

        return *this;
    }
    
    bitmask<T>& flip()
    {
        data = ~data;
        return *this;
    }

    bitmask<T>& flip(int i)
    {
        assert(i >=0 and i < size());
        data ^= (1<<i);
        return *this;
    }

    std::size_t count() const { return __builtin_popcount(data);};

    bitmask<T>& operator|=(const bitmask<T>& b2)
    {
        data |= b2.raw();
        return *this;
    }

    bitmask<T>& operator&=(const bitmask<T>& b2)
    {
        data &= b2.raw();
        return *this;
    }
};


template <typename T>
bool operator==(const bitmask<T> b1, const bitmask<T> b2)
{
    return b1.raw() == b2.raw();
}

template <typename T>
bitmask<T> operator&(bitmask<T> b1, const bitmask<T> b2)
{
    b1 &= b2;
    return b1;
}

template <typename T>
bitmask<T> operator|(bitmask<T> b1, const bitmask<T> b2)
{
    b1 |= b2;
    return b1;
}

template <typename T>
bitmask<T> operator~(bitmask<T> b1)
{
    b1.flip();
    return b1;
}

typedef bitmask<unsigned char> bitmask_8;

#endif
