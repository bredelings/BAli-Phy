#include "literal.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include <boost/multiprecision/cpp_dec_float.hpp>
#include <cmath>
#include <cstdint>
#include <limits>
#include <regex>
#include <fmt/format.h>

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

// Converts a finite double exactly into the rational stored by Hs::Floating.
// This is for semantic double values, not parser token text.
rational rationalFromDouble(double x)
{
    if (not std::isfinite(x))
        throw myexception()<<"rationalFromDouble: non-finite value "<<x<<" cannot be represented as a Haskell floating literal.\n";

    if (x == 0.0)
        return rational(0);

    int exponent = 0;
    auto significand = std::frexp(x, &exponent);
    auto mantissa = static_cast<std::int64_t>(std::ldexp(significand, std::numeric_limits<double>::digits));

    rational r{integer(mantissa)};
    int shift = exponent - std::numeric_limits<double>::digits;
    if (shift > 0)
    {
        integer factor = boost::multiprecision::pow(integer(2), shift);
        r *= factor;
    }
    else if (shift < 0)
    {
        integer factor = boost::multiprecision::pow(integer(2), -shift);
        r /= factor;
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

static string print_floating_decimal(const rational& r)
{
    using boost::multiprecision::cpp_dec_float_50;

    cpp_dec_float_50 x(r.numerator());
    x /= cpp_dec_float_50(r.denominator());

    return x.str(17, std::ios_base::fmtflags(0));
}

// Check the Unicode scalar-value invariant before printing a Haskell character.
static bool is_unicode_scalar_value(char32_t c)
{
    return c <= 0x10FFFF and not (0xD800 <= c and c <= 0xDFFF);
}

// Print one Haskell character literal from a Unicode scalar value, using
// numeric escapes until raw non-ASCII source output is deliberately enabled.
static string print_char_literal(char32_t c)
{
    if (not is_unicode_scalar_value(c))
        throw myexception()<<"Invalid Haskell character literal code point: "<<static_cast<std::uint32_t>(c);

    if (c == U'\a') return "'\\a'";
    if (c == U'\b') return "'\\b'";
    if (c == U'\f') return "'\\f'";
    if (c == U'\n') return "'\\n'";
    if (c == U'\r') return "'\\r'";
    if (c == U'\t') return "'\\t'";
    if (c == U'\v') return "'\\v'";
    if (c == U'\\') return "'\\\\'";
    if (c == U'\'') return "'\\''";
    if (c == U'"') return "'\\\"'";

    if (0x20 <= c and c <= 0x7E)
        return "'" + string(1, static_cast<char>(c)) + "'";

    return "'\\" + std::to_string(static_cast<std::uint32_t>(c)) + "'";
}

string Literal::print() const
{
    if (literal.index() == 0)
        return print_char_literal(std::get<0>(literal).value);
    else if (literal.index() == 1)
        return std::get<1>(literal).value.str();
    else if (literal.index() == 2)
        return '"' + std::get<2>(literal).value + '"';
    else if (literal.index() == 3)
    {
	auto& r = std::get<3>(literal).value;
        return print_floating_decimal(r);
    }
    else if (literal.index() == 4)
        return std::get<4>(literal).value.str() + "#";
    else
        std::abort();
}

std::optional<char32_t> Literal::is_Char() const
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
