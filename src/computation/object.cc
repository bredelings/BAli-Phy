#include "computation/object.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include <iomanip> // for std::quoted
#include <sstream>

std::string Object::print() const {
    return std::string("unprintable[")+demangle(typeid(*this).name())+"] "+ convertToString(this);
}

template<> std::string Box<Matrix>::print() const
{
    std::vector<std::string> rows;
    for(int i=0;i<size1();i++)
    {
	std::vector<double> row;
	for(int j=0;j<size2();j++)
	    row.push_back((*this)(i,j));
	rows.push_back( "[ " + join(row, ", ") + "]" );
    }
    return "[ " + join(rows, ", \n") + "]";
}

template<>  std::string Box<std::string>::print() const
{
    std::ostringstream s;
    s << std::quoted(value());
    return s.str();
}

