#ifndef UTIL_STRING_JOIN_H
#define UTIL_STRING_JOIN_H

#include <sstream>

// Print out elements of container v separated by string s
template<class V,typename S>
inline std::ostream& join(std::ostream& o, const V& v, const S& sep)
{
    int i=0;
    for(const auto& elem: v)
    {
	if (i) o<<sep;
	o<<elem;
	i++;
    }
    return o;
}

template<class V,typename S>
inline std::string join(const V& v, const S& sep)
{
    std::ostringstream o;
    join(o, v, sep);
    return o.str();
}

#endif 
