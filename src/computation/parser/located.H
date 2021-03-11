#ifndef LOCATED_H
#include <iostream>
#include "location.hh"

template <typename T>
struct Located: public Object
{
public:
    yy::location loc;
    T obj;

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
    Located(const yy::location& l, const T& t):loc(l),obj(t) {}
};

template <typename T>
inline std::string Located<T>::print() const {
    return "[" + convertToString(loc) + "@" + convertToString(obj) + "]";
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
