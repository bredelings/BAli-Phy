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
#include <boost/numeric/ublas/symmetric.hpp>
#include <boost/numeric/ublas/banded.hpp>
//#include <boost/numeric/bindings/atlas/clapack.hpp>

namespace ublas = boost::numeric::ublas;
//namespace atlas = boost::numeric::bindings::atlas;

typedef ublas::matrix<double> Matrix;
typedef ublas::symmetric_matrix<double> SMatrix;
typedef ublas::banded_matrix<double> BMatrix;
typedef ublas::vector<double> Vector;
typedef ublas::zero_vector<double> zero_vector;
typedef ublas::unit_vector<double> unit_vector;
typedef ublas::matrix_row<Matrix> matrix_row;
typedef ublas::matrix_column<Matrix> matrix_column;

#include "log-double.H"
#include "le-double.H"

#ifdef LE_DOUBLE
typedef le_double_t efloat_t;
#else
typedef log_double_t efloat_t;
#endif

typedef ublas::matrix<efloat_t> eMatrix;

#endif
