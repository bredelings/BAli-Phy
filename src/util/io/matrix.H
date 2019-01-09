#ifndef UTIL_IO_MATRIX_H
#define UTIL_IO_MATRIX_H

#include <vector>
#include <iostream>
#include "util/matrix.H"

template <typename T>
std::ostream& operator<<(std::ostream& o, const matrix<T>& M)
{
    for(int i=0;i<M.size1();i++)
    {
	for(int j=0;j<M.size2();j++)
	    o<<M(i,j)<<" ";
	o<<"\n";
    }
    return o;
}

#endif
