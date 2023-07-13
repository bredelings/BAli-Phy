#include "computation/object.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include <iomanip> // for std::quoted
#include <sstream>
#include "computation/expression/expression_ref.H"
#include <vector>
#include <utility>

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

template<> std::string Box<std::vector<std::pair<int,int>>>::print() const
{
    EVector V;
    for(auto& [x,y]: *this)
        V.push_back(EPair(x,y));
    return V.print();
}
