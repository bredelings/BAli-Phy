#ifndef EXPRESSION_LIST_H
#define EXPRESSION_LIST_H

#include <vector>
#include "expression_ref.H"
#include "constructor.H"

expression_ref List();

template <typename First>
auto List(const First& first)
{
    return first&List();
}

template <typename First, typename T2>
auto List(const First& first, const T2& t2)
{
    return first&List(t2);
}

template <typename First, typename T2, typename T3>
auto List(const First& first, const T2& t2, const T3& t3)
{
    return first&List(t2,t3);
}

template <typename First, typename T2, typename T3, typename T4>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4)
{
    return first&List(t2,t3,t4);
}

template <typename First, typename T2, typename T3, typename T4, typename T5>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4, const T5& t5)
{
    return first&List(t2,t3,t4,t5);
}


template <typename First, typename T2, typename T3, typename T4, typename T5, typename T6>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4, const T5& t5,const T6& t6)
{
    return first&List(t2,t3,t4,t5,t6);
}

template <typename T>
expression_ref get_list(const std::vector<T>& v)
{
    std::vector<expression_ref> v2;
    for(int i=0;i<v.size();i++)
	v2.push_back(v[i]);

    return get_list(v2);
}

expression_ref char_list(const std::string& s);

template<> expression_ref get_list<>(const std::vector<expression_ref>& v);

template <typename T,typename U>
std::vector<T> get_vector(const expression_ref& E) 
{
    if (not E)
	return std::vector<T>();

    if (not E.size())
    {
	std::vector<T> v2(1);
	v2[0] = E.as_<U>();
	return v2;
    }
    
    std::vector<T> v2(E.size());
    for(int i=0;i<v2.size();i++)
	v2[i] = E.sub()[i].as_<U>();
    return v2;
}

std::vector<expression_ref> get_ref_vector_from_list(const expression_ref& E);

template <typename T,typename U>
std::vector<T> get_vector_from_list(const expression_ref& E)
{
    std::vector<expression_ref> v1 = get_ref_vector_from_list(E);
    std::vector<T> v2;
    for(int i=0;i<v1.size();i++)
	v2.push_back(v1[i].as_<U>());
    return v2;
}

#endif
