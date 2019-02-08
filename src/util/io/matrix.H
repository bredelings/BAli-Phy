#ifndef UTIL_IO_MATRIX_H
#define UTIL_IO_MATRIX_H

#include <vector>
#include <iostream>
#include "util/matrix.H"
#include "util/string/join.H"

template <typename T>
std::ostream& print_matrix(std::ostream& o, const matrix<T>& M, char space=' ', char eol='\n')
{
    for(int i=0;i<M.size1();i++) {
	std::vector<double> v(M.size2());
	for(int j=0;j<v.size();j++)
	    v[j] = M(i,j);
	o<<join(v,space)<<eol;
    }
    return o;
}

template <typename T>
std::ostream& operator<<(std::ostream& o, const matrix<T>& M)
{
    return print_matrix(o,M);
}

#endif
