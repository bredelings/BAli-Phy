#include "literal.H"
#include "util/string/join.H"

using std::string;
using std::optional;

namespace Haskell
{

string Literal::print() const
{
    if (literal.index() == 0)
        return "'" + std::string(1, std::get<0>(literal).value) + "'";
    else if (literal.index() == 1)
        return std::get<1>(literal).value.str();
    else if (literal.index() == 2)
        return '"' + std::get<2>(literal).value + '"';
    else if (literal.index() == 3)
        return std::to_string(std::get<3>(literal).value);
    else if (literal.index() == 4)
        return std::get<4>(literal).value.str() + "#";
    else
        std::abort();
}

std::optional<char> Literal::is_Char() const
{
    if (literal.index() == 0)
        return std::get<0>(literal).value;
    else
        return {};
}

std::optional<integer> Literal::is_Integer() const
{
    if (literal.index() == 1)
        return std::get<1>(literal).value;
    else
        return {};
}

std::optional<string> Literal::is_String() const
{
    if (literal.index() == 2)
        return std::get<2>(literal).value;
    else
        return {};
}

std::optional<double> Literal::is_Double() const
{
    if (literal.index() == 3)
        return std::get<3>(literal).value;
    else
        return {};
}

std::optional<integer> Literal::is_BoxedInteger() const
{
    if (literal.index() == 4)
        return std::get<4>(literal).value;
    else
        return {};
}


}
