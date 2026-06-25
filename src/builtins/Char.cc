#include <cctype>

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/utf8.H"
#include <cstdint>


//  std::isgraph;
//  No equivalent here??

// No C++ functions for:
// isMark?
// isNumber?
// isSymbol?
// isSeparator?

using std::vector;

// Data.Char predicates are temporarily ASCII-only.  This avoids passing
// char32_t values to byte-oriented ctype functions before Unicode tables exist.
static unsigned char ascii_char_arg(vector<R::Exp>& args, const char* function_name)
{
    auto c = get_arg(args).as_char();
    // FIXME-UNICODE: Temporary ASCII boundary. Replace this with Unicode
    // category predicates when identifier/category tables are available.
    if (c > 0x7F)
        throw myexception()<<function_name<<": non-ASCII Char values are not yet supported.";
    return static_cast<unsigned char>(c);
}

// Convert an Int to a Runtime Char while preserving the Unicode scalar
// invariant expected by Haskell Char.
static char32_t checked_char_code(int i, const char* function_name)
{
    if (i < 0)
        throw myexception()<<function_name<<": integer "<<i<<" is not a Unicode scalar value.";

    auto c = static_cast<char32_t>(i);
    if (not utf8::is_scalar_value(c))
        throw myexception()<<function_name<<": integer "<<i<<" is not a Unicode scalar value.";

    return c;
}

extern "C" R::Exp simple_function_isDigit(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isDigit");

    if (std::isdigit(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isControl(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isControl");

    if (std::iscntrl(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isSpace(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isSpace");

    if (std::isspace(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isLower(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isLower");

    if (std::islower(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isUpper(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isUpper");

    if (std::isupper(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isAlphaNum(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isAlphaNum");

    if (std::isalnum(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isAlpha(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isAlpha");

    if (std::isalpha(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isPrint(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isPrint");

    if (std::isprint(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isPunctuation(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isPunctuation");

    if (std::ispunct(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_isHexDigit(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "isHexDigit");

    if (std::isxdigit(c))
        return 1;
    else
        return 0;
}

extern "C" R::Exp simple_function_toLower(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "toLower");

    return static_cast<char32_t>(std::tolower(c));
}

extern "C" R::Exp simple_function_toUpper(vector<R::Exp>& args)
{
    auto c = ascii_char_arg(args, "toUpper");

    return static_cast<char32_t>(std::toupper(c));
}

extern "C" R::Exp simple_function_ord(vector<R::Exp>& args)
{
    auto c = get_arg(args).as_char();

    return int(c);
}

extern "C" R::Exp simple_function_chr(vector<R::Exp>& args)
{
    auto i = get_arg(args).as_int();

    return checked_char_code(i, "chr");
}

extern "C" R::Exp simple_function_intToDigit(vector<R::Exp>& args)
{
    auto i = get_arg(args).as_int();

    char32_t c = 0;
    if (i >= 0 and i<=9)
        c = U'0' + i;
    else if (i >= 10 and i <= 15)
        c = U'A' + i;
    else
        throw myexception()<<"intToDigit: integer "<<i<<" is not a digit!";

    return c;
}
