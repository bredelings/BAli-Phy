#include "object.H"
#include "util.H" // for join( )

std::string Object::print() const {
    return std::string("unprintable[")+demangle(typeid(*this).name())+"] "+ convertToString(this);
}

template<> inline std::string Box<Matrix>::print() const
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

