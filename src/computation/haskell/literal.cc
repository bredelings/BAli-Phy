#include "literal.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include <regex>
#include "fmt/core.h"

using std::string;
using std::optional;

std::string remove_underscore(const std::string& s1)
{
    std::string s2;
    s2.reserve(s1.size());
    for(char c: s1)
    {
	if (c == '_') continue;

	s2.push_back(c);
    }

    return s2;
}

string remove_leading_zeros(const std::string& s)
{
    // remove leading zeros
    int first = 0;
    while(first + 1 < s.size() and s[first] == '0')
	first++;

    return s.substr(first);
}

string remove_underscore_and_leading_zeros(const std::string& s)
{
    return remove_leading_zeros(remove_underscore(s));
}

namespace Haskell
{

    std::string decimal("[0-9]+(_*[0-9]+)*");
    std::string exponent = fmt::format("_*[eE]([-+]?{0})",decimal);
    std::string rgx_s1 = fmt::format( R"(_*({0})\.({0})({1})?)",decimal,exponent);
    std::string rgx_s2 = fmt::format( R"(_*({0})({1}))",decimal,exponent);

    integer integerFromString(const string& s)
    {
	return integer(remove_underscore_and_leading_zeros(s));
    }

rational rationalFromString(const string& s)
{
    static const std::regex rgx1( rgx_s1 , std::regex_constants::ECMAScript);
    static const std::regex rgx2( rgx_s2 , std::regex_constants::ECMAScript);

    rational r = 42;

    std::smatch matches;
    if (std::regex_match(s, matches, rgx1))
    {
	auto decimal1 = remove_underscore(matches[1].str());
	auto decimal2 = remove_underscore(matches[3].str());

	int digits2 = decimal2.size();
	integer factor = boost::multiprecision::pow(integer(10), digits2);

	integer before(remove_leading_zeros(decimal1));
	integer after(remove_leading_zeros(decimal2));

	r = (factor*before + after);
	r /= factor;

	if (not matches[6].str().empty())
	{
	    int exp = convertTo<int>(remove_underscore_and_leading_zeros(matches[6].str()));
	    if (exp > 0)
		r *= boost::multiprecision::pow(integer(10), exp);
	    else
		r /= boost::multiprecision::pow(integer(10), -exp);
	}
    }
    else if (std::regex_match(s, matches, rgx2))
    {
	auto decimal = remove_underscore_and_leading_zeros(matches[1].str());

	r = integer(remove_leading_zeros(decimal));

	if (not matches[4].str().empty())
	{
	    int exp = convertTo<int>(remove_underscore_and_leading_zeros(matches[4].str()));
	    if (exp > 0)
		r *= boost::multiprecision::pow(integer(10), exp);
	    else
		r /= boost::multiprecision::pow(integer(10), -exp);
	}
    }
    else
    {
	throw myexception()<<"rationalFromString: string '"<<s<<"' is malformed, but somehow passed the parser!\n";
    }

    return r;
}
    
bool Integer::operator==(const Integer& I) const
{
    return (value == I.value);
}

bool Literal::operator==(const Literal& L) const
{
    return literal == L.literal;
}

bool Literal::operator==(const Object& O) const
{
    if (auto l = dynamic_cast<const Literal*>(&O))
        return (*this) == *l;
    else
        return false;
}

string Literal::print() const
{
    if (literal.index() == 0)
        return "'" + std::string(1, std::get<0>(literal).value) + "'";
    else if (literal.index() == 1)
        return std::get<1>(literal).value.str();
    else if (literal.index() == 2)
        return '"' + std::get<2>(literal).value + '"';
    else if (literal.index() == 3)
    {
	auto& r = std::get<3>(literal).value;
        return "(" + r.numerator().str() + "%" + r.denominator().str() +")";
    }
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

std::optional<rational> Literal::is_Floating() const
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
