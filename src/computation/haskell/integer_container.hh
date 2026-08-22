#ifndef HASKELL_INTEGER_CONTAINER_H
#define HASKELL_INTEGER_CONTAINER_H

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/rational.hpp>

typedef boost::multiprecision::cpp_int integer;
typedef boost::rational<integer> rational;

struct integer_container
{
    integer i;
    operator integer() {return i;}

    integer_container() = default;
    integer_container(const integer& ii):i(ii) {}

    std::strong_ordering operator<=>(const integer_container&) const = default;

    template <typename T>
    std::optional<T> try_convert() const {
        if (i >= std::numeric_limits<T>::min() && i <= std::numeric_limits<T>::max()) {
            return i.template convert_to<T>();
        }
        return std::nullopt;
    }

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
