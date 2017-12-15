#include "ptree.H"
#include "util.H"

using std::string;

std::ostream& operator<<(std::ostream& o,const monostate&) {o<<"()";return o;}

string show(const ptree& pt, int depth)
{
    string result = "";
    string indent(depth,' ');
    string indent2(depth+2,' ');
    result += "'"+convertToString(pt.value)+"'\n";
    for(auto c: pt)
    {
	result += indent2 + c.first + " : ";
	result += show(c.second,depth+4);
    }
    return result;
}

