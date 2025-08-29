#ifndef OBJECT_H
#define OBJECT_H

#include <string>
#include <memory>
#include <iostream>
#include <concepts>
#include <utility>
#include <vector>

#include "util/ref_counted.H"
#include "util/myexception.H"
#include "util/math/log-double.H"
#include "computation/type_constant.H"

class Object: public ref_counted
{
public:

    virtual Object* clone() const =0;

    virtual bool operator==(const Object& O) const
	{
	    if (this == &O)
		return true;

	    if (typeid(*this) != typeid(O)) return false;

	    std::abort();
	}

    virtual type_constant type() const {return type_constant::unknown_type;}

    virtual std::string print() const;

    virtual ~Object() = default;
};

// What type of smart pointer are we using to point to Objects?
template <typename T>
using object_ptr = ref_ptr<T>;

template<typename T>
class Box: public Object, public T
{
public:
    Box<T>* clone() const override {return new Box<T>(*this);}

    Box<T>& operator=(const Box<T>& t) = default;
    Box<T>& operator=(Box<T>&& t) = default;

    Box<T>& operator=(const T& t)
	{
	    T::operator=(t);
	    return *this;
	}

    Box<T>& operator=(T&& t)
	{
	    T::operator=(std::move(t));
	    return *this;
	}

    bool operator==(const Object& o2) const override
    {
        if constexpr (std::equality_comparable<T>)
        {
            if (const T* t2 = dynamic_cast<const T*>(&o2))
                return (value() == *t2);
            else
                return false;
        }
        else
            return false;
    }

    bool operator==(const Box<T>& b2) const requires std::equality_comparable<T>
    {
        return (value() == b2.value());
    }

    std::string print() const override {return Object::print();}

    const T& value() const {return *(const T*)this;}

    using T::T;

    Box() = default;
    Box(const Box<T>&) = default;
    Box(Box<T>&&) = default;
    Box(const T& t):T(t) {}
    Box(T&& t):T(std::move(t)) {}
};

template<typename T>
using PtrBox = Box<std::shared_ptr<const T>>;

/***************** Make log_double_t count as an arithmetic type **************/

typedef Box<std::string> String;

template<> std::string Box<std::string>::print() const;

#include "util/matrix.H"

template<> std::string Box<Matrix>::print() const;

template<> std::string Box<std::vector<std::pair<int,int>>>::print() const;

template <typename T>
using Vector = Box<std::vector<T>>;

template <typename T1,typename T2>
using Pair = Box<std::pair<T1,T2>>;

template <typename T>
object_ptr<T> ptr(const T& t) {return object_ptr<T>(t.clone());}
template <typename T>
object_ptr<const T> const_ptr(const T& t) {return object_ptr<const T>(t.clone());}

template<typename T>
inline std::ostream& operator<<(std::ostream& o,const object_ptr<T>& R)
{
    if (R)
	return o<<R->print();
    else
	return o<<"[NULL]";
}

inline std::ostream& operator<<(std::ostream& o,const Object& R)
{
    return o<<R.print();
}

// In util.cc
std::string demangle(const std::string&);

template <typename T>
const T* convert_and_check(const Object* o)
{
    assert(o);
    const T* converted = dynamic_cast<const T*>(o);
    if (not converted)
	throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(*o).name())<<" to type "<<demangle(typeid(T).name());
    return converted;
}

template <typename T1, typename T2>
std::shared_ptr<const T2> poly_convert_and_check(const Object* o)
{
    auto* converted1 = convert_and_check<PtrBox<T1>>(o);
    std::shared_ptr<const T2> converted2 = std::dynamic_pointer_cast<const T2>(*converted1);
    if (not converted2)
	throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(T1).name())<<" to type "<<demangle(typeid(T2).name());
    return converted2;
}

template <typename T>
object_ptr<const T> convert_and_check(const object_ptr<const Object>& o)
{
    assert(o);
    object_ptr<const T> converted =  boost::dynamic_pointer_cast<const T>(o);
    if (not converted)
    {
	const Object* oo = o.get();
	throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(*oo).name())<<" to type "<<demangle(typeid(T).name());
    }
    return converted;
}

template <typename T1, typename T2>
std::shared_ptr<const T2> poly_convert_and_check(const object_ptr<const Object>& o)
{
    assert(o);
    auto* converted1 = convert_and_check<PtrBox<T1>>(o);
    std::shared_ptr<const T2> converted2 = std::dynamic_pointer_cast<const T2>(*converted1);
    if (not converted2)
    {
	throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(T1).name())<<" to type "<<demangle(typeid(T2).name());
    }
    return converted2;
}

template <typename T>
const T* convert(const Object* o)
{
#ifdef NDEBUG
    return static_cast<const T*>(o);
#else
    return convert_and_check<T>(o);
#endif
}

template <typename T1, typename T2>
std::shared_ptr<const T2> poly_convert(const Object* o)
{
#ifdef NDEBUG
    auto* converted1 = convert<PtrBox<T1>>(o);
    return std::static_pointer_cast<const T2>(*converted1);
#else
    return poly_convert_and_check<T1,T2>(o);
#endif
}

template <typename T>
object_ptr<const T> convert(const object_ptr<const Object>& o)
{
#ifdef NDEBUG
    return boost::static_pointer_cast<const T>(o);
#else
    return convert_and_check<T>(o);
#endif
}

template <typename T1, typename T2>
std::shared_ptr<T2> poly_convert(const object_ptr<const Object>& o)
{
#ifdef NDEBUG
    std::shared_ptr<const T1> converted1 = convert_and_check<PtrBox<T1>>(o);
    std::shared_ptr<const T2> converted2 = std::static_pointer_cast<const T2>(converted1);
    return converted2;
#else
    return poly_convert_and_check<T1,T2>(o);
#endif
}

#endif
