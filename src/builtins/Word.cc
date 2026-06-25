#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/haskell/Integer.H"
#include "computation/machine/args.H"

using std::vector;

namespace
{
    // Normalize any integral value into the byte range used by the temporary
    // Int-backed Word8 representation.
    int normalize_word8(int x)
    {
        int result = x % 256;
        if (result < 0)
            result += 256;
        return result;
    }

    // Normalize arbitrary-precision integer literals into Word8's modulo-256
    // range without relying on implementation behavior for negative values.
    int normalize_word8(const integer& x)
    {
        integer result = x % 256;
        if (result < 0)
            result += 256;
        return result.convert_to<int>();
    }
}

extern "C" R::Exp simple_function_integerToWord8(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    return normalize_word8(x);
}

extern "C" R::Exp simple_function_intToWord8(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    return normalize_word8(x);
}

extern "C" R::Exp simple_function_word8ToInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    return x;
}
