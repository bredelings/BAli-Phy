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

    std::string print() const;

    expression_ref copy() const {
	return object_ptr<Located<T>>(clone());
    }

    Located() = default;
    Located(const Located<T>&) = default;
    Located(Located<T>&&) = default;
    Located(const location& loc, const T& t):location(loc),T(t) {}
};

template <typename T>
inline std::string Located<T>::print() const {
    const T& t = *this;
    const yy::location& l = *this;
    return "[" + convertToString(t) + "@" + convertToString(l) + "]";
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
#endif
