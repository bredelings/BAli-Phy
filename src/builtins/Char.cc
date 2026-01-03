#include <cctype>

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"


//  std::isgraph;
//  No equivalent here??

// No C++ functions for:
// isMark?
// isNumber?
// isSymbol?
// isSeparator?

using std::vector;

extern "C" expression_ref simple_function_isDigit(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isdigit(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isControl(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::iscntrl(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isSpace(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isspace(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isLower(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::islower(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isUpper(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isupper(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isAlphaNum(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isalnum(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isAlpha(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isalpha(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isPrint(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isprint(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isPunctuation(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::ispunct(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_isHexDigit(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    if (std::isxdigit(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" expression_ref simple_function_toLower(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    return {expression_ref(std::tolower(c))};
}

extern "C" expression_ref simple_function_toUpper(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    return {expression_ref(std::toupper(c))};
}

extern "C" expression_ref simple_function_ord(vector<expression_ref>& args)
{
    auto c = get_arg(args).as_char();

    return {expression_ref(int(c))};
}

extern "C" expression_ref simple_function_chr(vector<expression_ref>& args)
{
    auto i = get_arg(args).as_int();

    // complain if we can't make a valid char out of this?

    return {expression_ref(char(i))};
}

extern "C" expression_ref simple_function_intToDigit(vector<expression_ref>& args)
{
    auto i = get_arg(args).as_int();
    char d = i;

    // complain if we can't make a valid char out of this?

    char c = 0;
    if (i >= 0 and i<=9)
        c = '0' + d;
    else if (i >= 10 and i <= 15)
        c = 'A' + d;
    else
        throw myexception()<<"intToDigit: integer "<<i<<" is not a digit!";

    return {expression_ref(c)};
}
