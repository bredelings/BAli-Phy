#ifndef LOCATED_H
#define LOCATED_H
#include <iostream>
#include <optional>
#include "location.hh"

#include "computation/expression/expression_ref.H"

template <typename T>
struct Unlocated
{
    const T& obj;
    Unlocated(const T& t):obj(t) {}
};

template <typename T>
struct Located
{
public:
    std::optional<yy::location> loc;
    T obj;

    Located<T>* clone() const {return new Located<T>(*this);}

    Located<T>& operator=(const Located<T>& t) = default;
    Located<T>& operator=(Located<T>&& t) = default;

    template <typename U>
    Located<T>& operator=(const Unlocated<U>& u)
    {
        loc = {};
        obj = u;
    }

    template <typename U>
    Located<T>& operator=(Unlocated<U>&& u)
    {
        loc = {};
        obj = std::move(u);
    }

    std::string print() const;

    expression_ref copy() const {
	return object_ptr<Located<T>>(clone());
    }

    const T& value() const {return obj;}
          T& value()       {return obj;}

    Located() = default;
    Located(const Located<T>&) = default;
    Located(Located<T>&&) = default;

    template <typename U>
    Located(const Unlocated<U>& u):obj(u.obj) {}

    template <typename U>
    Located(Unlocated<U>&& u):obj(std::move(u.obj)) {}

    Located(const std::optional<yy::location>& l, const T& t):loc(l),obj(t) {}
};

template <typename T>
inline std::string Located<T>::print() const {
    return obj.print();
}

template <>
inline std::string Located<std::string>::print() const {
    return obj;
}

template <typename T>
std::ostream& operator<<(std::ostream& o,const Located<T>& lt)
{
    return o<<lt.print();
}

inline yy::location merge_loc(const yy::location& l1, const yy::location& l2)
{
    return {l1.begin, l2.end};
}

inline std::nullopt_t noloc = std::nullopt;

template <typename T>
const T& unloc(const Located<T>& l) {return l.obj;}

template <typename T>
      T& unloc(      Located<T>& l) {return l.obj;}
#endif
