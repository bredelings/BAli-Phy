#ifndef TUPLE_H
#define TUPLE_H

#include "expression_ref.H"
#include "constructor.H"

std::string tuple_name(int n);
bool is_tuple_name(const std::string&);
constructor tuple_head(int n);

// Create a constructor for n-tuples.
expression_ref Tuple(int n);

template <typename First, typename Second>
auto apply_subst(const First& first, const Second& second)
{
    return first * second;
}

template <typename First, typename Second, typename Third, typename... Rest>
auto apply_subst(const First& first, const Second& second, const Third& third, const Rest&... rest)
{
    return apply_subst(first*second, third, rest...);
}

template <typename First, typename Second, typename... Rest>
auto Tuple(const First& first, const Second& second, const Rest&... rest)
{
    return apply_subst(Tuple(2+sizeof...(rest)) ,first,second,rest...);
}

template <typename T>
expression_ref get_tuple(const std::vector<T>& v)
{
    std::vector<expression_ref> v2;
    for(int i=0;i<v.size();i++)
	v2.push_back(v[i]);

    return get_tuple(v2);
}

template<> expression_ref get_tuple<>(const std::vector<expression_ref>& v);

template <typename T>
expression_ref get_tuple(const std::valarray<T>& v)
{
    std::vector<expression_ref> v2;
    for(int i=0;i<v.size();i++)
	v2.push_back(v[i]);

    return get_tuple(v2);
}



#endif
