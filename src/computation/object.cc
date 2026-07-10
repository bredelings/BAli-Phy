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

// Format a native matrix in the shared row-oriented representation used by
// the runtime Show instance.
template <typename T>
std::string print_matrix(const matrix<T>& native_matrix)
{
    std::vector<std::string> rows;
    for(int i=0;i<native_matrix.size1();i++)
    {
        std::vector<T> row;
        for(int j=0;j<native_matrix.size2();j++)
            row.push_back(native_matrix(i,j));
        rows.push_back( "[ " + join(row, ", ") + "]" );
    }
    return "[ " + join(rows, ", \n") + "]";
}

}

// Format the runtime's native Double matrix representation.
template<> std::string Box<Matrix>::print() const
{
    return print_matrix(*this);
}

// Format the runtime's native Int matrix representation.
template<> std::string Box<matrix<int>>::print() const
{
    return print_matrix(*this);
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
