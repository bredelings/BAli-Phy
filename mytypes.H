#ifndef MYTYPES_H
#define MYTYPES_H

#include <cassert>
#include <vector>
#include <string>
#include <fstream>
#include <valarray>

using std::vector;
using std::string;
using std::ifstream;
using std::ofstream;
using std::endl;

#include <boost/numeric/ublas/matrix.hpp>

using namespace boost::numeric;
typedef ublas::matrix<double> Matrix;
typedef ublas::vector<double> Vector;
typedef ublas::zero_vector<double> zero_vector;
typedef ublas::unit_vector<double> unit_vector;
typedef ublas::matrix_row<Matrix> matrix_row;
typedef ublas::matrix_column<Matrix> matrix_column;

#endif
