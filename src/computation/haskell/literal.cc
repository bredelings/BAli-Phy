#include "literal.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include "util/unicode.H"
#include "util/utf8.H"
#include <boost/multiprecision/cpp_dec_float.hpp>
#include <cmath>
#include <cstdint>
#include <limits>
#include <regex>
#include <vector>
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

// Recognize decimal digits because numeric escapes must be separated from a
// following literal digit with Haskell's empty escape.
static bool is_ascii_digit(char32_t c)
{
    return U'0' <= c and c <= U'9';
}

// Return the short Haskell escape spelling for characters that have one in
// character or string literal context.
static optional<string> standard_escape(char32_t c, bool in_string_literal)
{
    if (c == U'\a') return "\\a";
    if (c == U'\b') return "\\b";
    if (c == U'\f') return "\\f";
    if (c == U'\n') return "\\n";
    if (c == U'\r') return "\\r";
    if (c == U'\t') return "\\t";
    if (c == U'\v') return "\\v";
    if (c == U'\\') return "\\\\";
    if (c == U'"') return "\\\"";
    if (c == U'\'' and not in_string_literal) return "\\'";
    return {};
}

// Decide whether a non-ASCII scalar can be emitted directly inside a Haskell
// source literal.  Separators and non-graphic categories stay escaped.
static bool is_raw_printable_non_ascii_literal_char(char32_t c)
{
    if (c <= 0x7E)
        return false;

    auto category = unicode::category(c);
    return unicode::is_letter(category) or
           unicode::is_mark(category) or
           unicode::is_number(category) or
           unicode::is_punctuation(category) or
           unicode::is_symbol(category);
}

// Print one escaped literal payload character, without the surrounding quotes.
static string print_literal_payload_char(char32_t c, bool in_string_literal)
{
    if (auto escaped = standard_escape(c, in_string_literal))
        return *escaped;
    if (0x20 <= c and c <= 0x7E)
        return string(1, static_cast<char>(c));
    if (is_raw_printable_non_ascii_literal_char(c))
        return utf8::encode(c);
    return "\\" + std::to_string(static_cast<std::uint32_t>(c));
}

// Print one Haskell character literal from a Unicode scalar value.
static string print_char_literal(char32_t c)
{
    if (not utf8::is_scalar_value(c))
        throw myexception()<<"Invalid Haskell character literal code point: "<<static_cast<std::uint32_t>(c);

    return "'" + print_literal_payload_char(c, false) + "'";
}

// Decode Hs::String payload bytes before printing.  Invalid bytes indicate
// broken internal construction of a Haskell source string literal.
static std::vector<char32_t> decode_string_literal_payload(const string& text)
{
    std::vector<char32_t> chars;
    for(std::size_t byte_offset = 0; byte_offset < text.size();)
    {
        auto decoded = utf8::decode_next(text, byte_offset);
        if (not decoded)
            throw myexception()<<"Invalid UTF-8 in Haskell string literal payload.";
        chars.push_back(decoded->code_point);
        byte_offset = decoded->next_byte;
    }
    return chars;
}

// Print a UTF-8-backed Haskell string literal.
static string print_string_literal(const string& text)
{
    auto chars = decode_string_literal_payload(text);

    string result = "\"";
    for(std::size_t i=0; i<chars.size(); i++)
    {
        auto c = chars[i];
        bool use_numeric_escape = not standard_escape(c, true) and not (0x20 <= c and c <= 0x7E);
        auto escaped = print_literal_payload_char(c, true);

        result += escaped;
        if (use_numeric_escape and i+1 < chars.size() and is_ascii_digit(chars[i+1]))
            result += "\\&";
    }
    result += "\"";
    return result;
}

string Literal::print() const
{
    if (literal.index() == 0)
        return print_char_literal(std::get<0>(literal).value);
    else if (literal.index() == 1)
        return std::get<1>(literal).value.str();
    else if (literal.index() == 2)
        return print_string_literal(std::get<2>(literal).value);
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
