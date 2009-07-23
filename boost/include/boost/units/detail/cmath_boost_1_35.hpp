// Boost.Units - A C++ library for zero-overhead dimensional analysis and 
// unit/quantity manipulation and conversion
//
// Copyright (C) 2003-2008 Matthias Christian Schabel
// Copyright (C) 2007-2008 Steven Watanabe
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_UNITS_CMATH_BOOST_1_35_HPP 
#define BOOST_UNITS_CMATH_BOOST_1_35_HPP

#include <cmath>
#include <cstdlib>

#include <boost/units/dimensionless_quantity.hpp>
#include <boost/units/pow.hpp>
#include <boost/units/quantity.hpp>
#include <boost/units/detail/cmath_impl.hpp>

#include <boost/units/systems/si/plane_angle.hpp>

/// \file 
/// \brief Overloads of functions in \<cmath\> for quantities
///
/// \detailed Only functions for which a dimensionally-correct result type  
/// can be determined are overloaded. All functions work with dimensionless
/// quantities.

// BOOST_PREVENT_MACRO_SUBSTITUTION is needed on certain compilers that define 
// some <cmath> functions as macros; it is used for all functions even though it
// isn't necessary -- I didn't want to think :)
//
// the form using namespace detail; return(f(x)); is used
// to enable ADL for UDTs

namespace boost {

namespace units { 

template<class Unit,class Y>
inline 
bool 
isfinite BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{ 
    using namespace detail;
    return isfinite BOOST_PREVENT_MACRO_SUBSTITUTION (q.value());
}

template<class Unit,class Y>
inline 
bool 
isinf BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{ 
    using namespace detail;
    return isinf BOOST_PREVENT_MACRO_SUBSTITUTION (q.value());
}

template<class Unit,class Y>
inline
bool 
isnan BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{ 
    using namespace detail;
    return isnan BOOST_PREVENT_MACRO_SUBSTITUTION (q.value());
}

template<class Unit,class Y>
inline 
bool 
isnormal BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{ 
    using namespace detail;
    return isnormal BOOST_PREVENT_MACRO_SUBSTITUTION (q.value());
}

template<class Unit,class Y>
inline 
bool 
isgreater BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                            const quantity<Unit,Y>& q2)
{ 
    using namespace detail;
    return isgreater BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value());
}

template<class Unit,class Y>
inline 
bool 
isgreaterequal BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                                 const quantity<Unit,Y>& q2)
{ 
    using namespace detail;
    return isgreaterequal BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value());
}

template<class Unit,class Y>
inline 
bool 
isless BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                         const quantity<Unit,Y>& q2)
{ 
    using namespace detail;
    return isless BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value());
}

template<class Unit,class Y>
inline 
bool 
islessequal BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                              const quantity<Unit,Y>& q2)
{ 
    using namespace detail;
    return islessequal BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value());
}

template<class Unit,class Y>
inline 
bool 
islessgreater BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                                const quantity<Unit,Y>& q2)
{ 
    using namespace detail;
    return islessgreater BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value());
}

template<class Unit,class Y>
inline 
bool 
isunordered BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                              const quantity<Unit,Y>& q2)
{ 
    using namespace detail;
    return isunordered BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value());
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
abs BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(abs BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
ceil BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(ceil BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
copysign BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
         const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(copysign BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
fabs BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(fabs BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
floor BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(floor BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
fdim BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                       const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(fdim BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

template<class Unit1,class Unit2,class Unit3,class Y>
inline 
typename add_typeof_helper<
    typename multiply_typeof_helper<quantity<Unit1,Y>,
                                    quantity<Unit2,Y> >::type,
    quantity<Unit3,Y> >::type 
fma BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit1,Y>& q1,
                                      const quantity<Unit2,Y>& q2,
                                      const quantity<Unit3,Y>& q3)
{
    using namespace detail;

    typedef quantity<Unit1,Y>   type1;
    typedef quantity<Unit2,Y>   type2;
    typedef quantity<Unit3,Y>   type3;
    
    typedef typename multiply_typeof_helper<type1,type2>::type  prod_type;
    typedef typename add_typeof_helper<prod_type,type3>::type   quantity_type;
    
    return quantity_type::from_value(fma BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value(),q3.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
fmax BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                       const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(fmax BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
fmin BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                       const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(fmin BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

template<class Unit,class Y>
inline 
int 
fpclassify BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{ 
    using namespace detail;

    return fpclassify BOOST_PREVENT_MACRO_SUBSTITUTION (q.value());
}

template<class Unit,class Y>
inline 
typename root_typeof_helper<
    typename add_typeof_helper<
        typename power_typeof_helper<quantity<Unit,Y>,
                                     static_rational<2> >::type,
        typename power_typeof_helper<quantity<Unit,Y>,
                                     static_rational<2> >::type>::type,
        static_rational<2> >::type
hypot BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    type1;
    
    typedef typename power_typeof_helper<type1,static_rational<2> >::type   pow_type;
    typedef typename add_typeof_helper<pow_type,pow_type>::type             add_type;
    typedef typename root_typeof_helper<add_type,static_rational<2> >::type quantity_type;
        
    return quantity_type::from_value(hypot BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

// does ISO C++ support long long? g++ claims not
//template<class Unit,class Y>
//inline 
//quantity<Unit,long long> 
//llrint BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
//{
//    using namespace detail;
//
//    typedef quantity<Unit,long long>    quantity_type;
//    
//    return quantity_type::from_value(llrint BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
//}

// does ISO C++ support long long? g++ claims not
//template<class Unit,class Y>
//inline 
//quantity<Unit,long long> 
//llround BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
//{
//    using namespace detail;
//
//    typedef quantity<Unit,long long>    quantity_type;
//    
//    return quantity_type::from_value(llround BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
//}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
nearbyint BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(nearbyint BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> nextafter BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                                             const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(nextafter BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> nexttoward BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q1,
                                                              const quantity<Unit,Y>& q2)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(nexttoward BOOST_PREVENT_MACRO_SUBSTITUTION (q1.value(),q2.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
rint BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(rint BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
round BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(round BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline 
bool 
signbit BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{ 
    using namespace detail;

    return signbit BOOST_PREVENT_MACRO_SUBSTITUTION (q.value());
}

template<class Unit,class Y>
inline 
quantity<Unit,Y> 
trunc BOOST_PREVENT_MACRO_SUBSTITUTION (const quantity<Unit,Y>& q)
{
    using namespace detail;

    typedef quantity<Unit,Y>    quantity_type;
    
    return quantity_type::from_value(trunc BOOST_PREVENT_MACRO_SUBSTITUTION (q.value()));
}

template<class Unit,class Y>
inline
quantity<Unit, Y>
fmod(const quantity<Unit,Y>& q1, const quantity<Unit,Y>& q2)
{
    using std::fmod;
    
    typedef quantity<Unit,Y> quantity_type;

    return quantity_type::from_value(fmod(q1.value(), q2.value()));
}

template<class Unit, class Y>
inline
quantity<Unit, Y>
modf(const quantity<Unit, Y>& q1, quantity<Unit, Y>* q2)
{
    using std::modf;

    typedef quantity<Unit,Y> quantity_type;

    return quantity_type::from_value(modf(q1.value(), &quantity_cast<Y&>(*q2)));
}

template<class Unit, class Y, class Int>
inline
quantity<Unit, Y>
frexp(const quantity<Unit, Y>& q,Int* ex)
{
    using std::frexp;

    typedef quantity<Unit,Y> quantity_type;

    return quantity_type::from_value(frexp(q.value(),ex));
}

/// For non-dimensionless quantities, integral and rational powers 
/// and roots can be computed by @c pow<Ex> and @c root<Rt> respectively.
template<class S, class Y>
inline
quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>
pow(const quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>& q1,
    const quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>& q2)
{
    using std::pow;

    typedef quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S),Y> quantity_type;

    return quantity_type::from_value(pow(q1.value(), q2.value()));
}

template<class S, class Y>
inline
quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>
exp(const quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>& q)
{
    using std::exp;

    typedef quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y> quantity_type;

    return quantity_type::from_value(exp(q.value()));
}

template<class Unit, class Y, class Int>
inline
quantity<Unit, Y>
ldexp(const quantity<Unit, Y>& q,const Int& ex)
{
    using std::ldexp;

    typedef quantity<Unit,Y> quantity_type;

    return quantity_type::from_value(ldexp(q.value(), ex));
}

template<class S, class Y>
inline
quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>
log(const quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>& q)
{
    using std::log;

    typedef quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y> quantity_type;

    return quantity_type::from_value(log(q.value()));
}

template<class S, class Y>
inline
quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>
log10(const quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y>& q)
{
    using std::log10;

    typedef quantity<BOOST_UNITS_DIMENSIONLESS_UNIT(S), Y> quantity_type;

    return quantity_type::from_value(log10(q.value()));
}

template<class Unit,class Y>
inline 
typename root_typeof_helper<
                            quantity<Unit,Y>,
                            static_rational<2>
                           >::type
sqrt(const quantity<Unit,Y>& q)
{
    using std::sqrt;

    typedef typename root_typeof_helper<
                                        quantity<Unit,Y>,
                                        static_rational<2>
                                       >::type quantity_type;

    return quantity_type::from_value(sqrt(q.value()));
}

} // namespace units

} // namespace boost

#endif // BOOST_UNITS_CMATH_BOOST_1_35_HPP
