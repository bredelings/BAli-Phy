#ifndef LOCATED_H
#include <iostream>
#include "location.hh"

template <typename T>
struct Located: public Object, public yy::location, public T
{
public:
    Located<T>* clone() const {return new Located<T>(*this);}

    Located<T>& operator=(const Located<T>& t) = default;
    Located<T>& operator=(Located<T>&& t) = default;

    std::string print() const {return Object::print();}

    Located() = default;
    Located(const Located<T>&) = default;
    Located(Located<T>&&) = default;
    Located(const location& loc, const T& t):location(loc),T(t) {}
};

template <typename T>
std::ostream& operator<<(std::ostream& o,const Located<T>& lt)
{
    const T& t = lt;
    return (o<<t);
}
#endif
