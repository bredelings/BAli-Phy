#include "computation/object.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include <iomanip> // for std::quoted
#include <sstream>
#include "computation/runtime/ast.H"

std::string Object::print() const {
    return std::string("unprintable[")+demangle(typeid(*this).name())+"] "+ convertToString(this);
}

namespace
{

// Format an Eigen-backed runtime matrix without converting it to the general
// C++ two-dimensional storage type.
template <typename T>
std::string print_dense_matrix(const DenseMatrix<T>& native_matrix)
{
    std::vector<std::string> rows;
    for(Eigen::Index i = 0; i < native_matrix.rows(); i++)
    {
        std::vector<T> row;
        for(Eigen::Index j = 0; j < native_matrix.cols(); j++)
            row.push_back(native_matrix(i,j));
        rows.push_back("[ " + join(row, ", ") + "]");
    }
    return "[ " + join(rows, ", \n") + "]";
}

// Format a native numeric vector in the compact representation used by
// Numeric.LinearAlgebra.Vector's Show instance.
template <typename T>
std::string print_numeric_vector(const DenseVector<T>& native_vector)
{
    std::vector<T> values;
    values.reserve(native_vector.size());
    for(Eigen::Index i = 0; i < native_vector.size(); i++)
        values.push_back(native_vector(i));
    return "[" + join(values, ",") + "]";
}

}

// Format a runtime matrix using the shared Eigen-backed representation.
template<> std::string Box<DenseMatrix<double>>::print() const
{
    return print_dense_matrix(*this);
}

// Format the runtime's native Int matrix representation.
template<> std::string Box<DenseMatrix<int>>::print() const
{
    return print_dense_matrix(*this);
}

// Format the runtime's native Double vector representation.
template<> std::string Box<DenseVector<double>>::print() const
{
    return print_numeric_vector(*this);
}

// Format the runtime's native Int vector representation.
template<> std::string Box<DenseVector<int>>::print() const
{
    return print_numeric_vector(*this);
}

template<>  std::string Box<std::string>::print() const
{
    std::ostringstream s;
    s << std::quoted(value());
    return s.str();
}

template<> std::string Box<std::vector<std::pair<int,int>>>::print() const
{
    R::RVector V;
    for(auto& [x,y]: *this)
        V.push_back(R::RPair(x, y));
    return V.print();
}
