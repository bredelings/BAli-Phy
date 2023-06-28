#ifndef STRING_CONVERT_H
#define STRING_CONVERT_H

#include <vector>
#include <optional>
#include <string>
#include <sstream>
#include "util/myexception.H"

std::string demangle(const std::string&);

template <class T>
std::optional<T> can_be_converted_to(const std::string& s)
{ 
    std::istringstream i(s);
    T t;
    if (i>>std::boolalpha>>t and i.peek() == std::istream::traits_type::eof())
	return t;
    else
	return {};
}

template <class T>
T convertTo(const std::string& s) 
{ 
    auto t = can_be_converted_to<T>(s);
    if (not t)
	throw myexception()<<"String '"<<s<<"' is not of type "<<demangle(typeid(T).name());
    return *t;
}

template <class T>
std::vector<T> convertTo(const std::vector<std::string>& s) { 
    std::vector<T> v;
    for(int i=0;i<s.size();i++)
	v.push_back(convertTo<T>(s[i]));

    return v;
}

template <class T>
std::string convertToString(T t)
{
    std::ostringstream o;
    o.precision(15);
    o<<std::boolalpha;
  
    o << t;
    return o.str();
} 


#endif
