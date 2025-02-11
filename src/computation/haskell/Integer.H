#ifndef HASKELL_INTEGER_H
#define HASKELL_INTEGER_H

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/rational.hpp>
#include "computation/object.H"

typedef boost::multiprecision::cpp_int integer;
typedef Box<integer> Integer;

typedef boost::rational<integer> rational;


template<>
inline std::string Box<integer>::print() const
{
    return str();
}


struct integer_container
{
    integer i;
    operator integer() {return i;}

    integer_container() = default;
    integer_container(const integer& ii):i(ii) {}

    bool operator==(const integer_container& ic) const { return i == ic.i;}
    bool operator<(const integer_container& ic) const { return i < ic.i;}

    template <class Archive>
    void load(Archive& ar)
    {
	std::string s;
	ar(s);
	integer ii(s);
	i = ii;
    }

    template <class Archive>
    void save(Archive& ar) const
    {
	std::string s = i.str();
	ar(s);
    }
};

#endif
