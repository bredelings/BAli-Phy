// mcs::units - A C++ library for zero-overhead dimensional analysis and 
// unit/quantity manipulation and conversion
//
// Copyright (C) 2003-2008 Matthias Christian Schabel
// Copyright (C) 2008 Steven Watanabe
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_UNITS_CMATH_MWCW_IMPL_BOOST_1_35_HPP 
#define BOOST_UNITS_CMATH_MWCW_IMPL_BOOST_1_35_HPP

#if __MWERKS__

#include <cmath>

// BOOST_PREVENT_MACRO_SUBSTITUTION is used for all functions even though it
// isn't necessary -- I didn't want to think :)

// the form using namespace detail; return(f(x)); is used
// to enable ADL for UDTs

namespace boost {

namespace units { 

namespace detail {

template<class Y> 
inline bool isfinite BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return isfinite(val);
}

template<class Y> 
inline bool isinf BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return isinf(val);
}

template<class Y> 
inline bool isnan BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return isnan(val);
}

template<class Y> 
inline bool isnormal BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return isnormal(val);
}

template<class Y> 
inline bool isgreater BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return ::isgreater(v1,v2);
}

template<class Y> 
inline bool isgreaterequal BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return ::isgreaterequal(v1,v2);
}

template<class Y> 
inline bool isless BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return ::isless(v1,v2);
}

template<class Y> 
inline bool islessequal BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return ::islessequal(v1,v2);
}

template<class Y> 
inline bool islessgreater BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return ::islessgreater(v1,v2);
}

template<class Y> 
inline bool isunordered BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return ::isunordered(v1,v2);
}

template<class Y>
inline Y abs BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::abs(val);
}

template<class Y>
inline Y ceil BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::ceil(val);
}

template<class Y>
inline Y copysign BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::copysign(v1,v2);
}

template<class Y>
inline Y fabs BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::fabs(val);
}

template<class Y>
inline Y floor BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::floor(val);
}

template<class Y>
inline Y fdim BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::fdim(v1,v2);
}

template<class Y>
inline Y fma BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2,const Y& v3)
{
    return std::fma(v1,v2,v3);
}

template<class Y>
inline Y fmax BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::fmax(v1,v2);
}

template<class Y>
inline Y fmin BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::fmin(v1,v2);
}

template<class Y>
inline int fpclassify BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return fpclassify(val);
}

template<class Y>
inline Y hypot BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::hypot(v1,v2);
}

//template<class Y>
//inline long long llrint BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
//{
//    return std::llrint(val);
//}
//
//template<class Y>
//inline long long llround BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
//{
//    return std::llround(val);
//}

template<class Y>
inline Y nearbyint BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::nearbyint(val);
}

template<class Y>
inline Y nextafter BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::nextafter(v1,v2);
}

template<class Y>
inline Y nexttoward BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& v1,const Y& v2)
{
    return std::nexttoward(v1,v2);
}

template<class Y>
inline Y rint BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::rint(val);
}

template<class Y>
inline Y round BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::round(val);
}

template<class Y>
inline bool signbit BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return signbit(val);
}

template<class Y>
inline Y trunc BOOST_PREVENT_MACRO_SUBSTITUTION (const Y& val)
{
    return std::trunc(val);
}

} // namespace detail

} // namespace units

} // namespace boost

#endif // __MWERKS__

#endif // BOOST_UNITS_CMATH_MWCW_IMPL_BOOST_1_35_HPP
