#ifndef UTIL_IO_VECTOR_H
#define UTIL_IO_VECTOR_H

#include <vector>
#include <iostream>

template <typename T>
std::ostream& operator<<(std::ostream& o, const std::vector<T>& v)
{
    for(int i=0;i<v.size();i++)
	o<<v[i]<<" ";
    o<<"\n";
  
    return o;
}

#endif
