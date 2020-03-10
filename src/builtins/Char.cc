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


extern "C" closure builtin_function_isDigit(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isdigit(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isControl(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::iscntrl(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isSpace(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isspace(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isLower(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::islower(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isUpper(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isupper(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isAlphaNum(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isalnum(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isAlpha(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isalpha(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isPrint(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isprint(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isPunctuation(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::ispunct(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_isHexDigit(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isxdigit(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}

extern "C" closure builtin_function_toLower(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    return {expression_ref(std::tolower(c))};
}

extern "C" closure builtin_function_toUpper(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    return {expression_ref(std::toupper(c))};
}

extern "C" closure builtin_function_ord(OperationArgs& Args)
{
    auto c = Args.evaluate(0).as_char();

    return {expression_ref(int(c))};
}

extern "C" closure builtin_function_chr(OperationArgs& Args)
{
    auto i = Args.evaluate(0).as_int();

    // complain if we can't make a valid char out of this?

    return {expression_ref(char(i))};
}

extern "C" closure builtin_function_intToDigit(OperationArgs& Args)
{
    auto i = Args.evaluate(0).as_int();
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
