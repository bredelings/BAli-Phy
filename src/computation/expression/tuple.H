#ifndef TUPLE_H
#define TUPLE_H

#include "expression_ref.H"
#include "constructor.H"

std::string tuple_name(int n);
int tuple_arity(const std::string&);
bool is_tuple_name(const std::string&);
constructor tuple_head(int n);

extern expression_ref fst;
extern expression_ref snd;

// Create a constructor for n-tuples.
expression_ref Tuple(int n);

inline auto append_exp(const expression_ref& first, const expression_ref& second)
{
    return first + second;
}

template <typename... Rest>
auto append_exp(const expression_ref& first, const expression_ref& second, const expression_ref& third, const Rest&... rest)
{
    return append_exp(first+second, third, rest...);
}

template <typename... Rest>
auto Tuple(const expression_ref& first, const expression_ref& second, const Rest&... rest)
{
    return append_exp(tuple_head(2+sizeof...(rest)),first,second,rest...);
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


#endif
