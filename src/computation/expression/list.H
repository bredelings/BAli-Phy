#ifndef EXPRESSION_LIST_H
#define EXPRESSION_LIST_H

#include <vector>
#include "expression_ref.H"
#include "constructor.H"

expression_ref List();

expression_ref cons(const expression_ref& head,const expression_ref& tail);

inline expression_ref List(const expression_ref& e1)
{
    return cons(e1,List());
}

inline expression_ref List(const expression_ref& e1,const expression_ref& e2)
{
    return cons(e1,List(e2));
}

inline expression_ref List(const expression_ref& e1,
		    const expression_ref& e2,
		    const expression_ref& e3)
{
    return cons(e1,List(e2,e3));
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
