#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/unicode.H"
#include "util/utf8.H"
#include <cstdint>
#include <cstdlib>

using std::vector;

// Extract one Runtime Char from a builtin call.  Char scalar validity is
// maintained by constructors and literal parsing before these predicates run.
static char32_t char_arg(vector<R::Exp>& args)
{
    return get_arg(args).as_char();
}

// Preserve the historical Int-valued foreign interface for Bool-like
// Data.Char predicates.
static int bool_int(bool b)
{
    return b ? 1 : 0;
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

// Check the Haskell isDigit range.  Unlike Unicode decimal-number categories,
// this predicate intentionally accepts only ASCII digits.
static bool is_ascii_digit(char32_t c)
{
    return c >= U'0' and c <= U'9';
}

// Check the Haskell isHexDigit range.  This mirrors GHC's ASCII-only hex digit
// predicate rather than accepting broader Unicode numeric characters.
static bool is_ascii_hex_digit(char32_t c)
{
    return is_ascii_digit(c) or (c >= U'a' and c <= U'f') or (c >= U'A' and c <= U'F');
}

// Map generic Unicode categories to the Data.Char GeneralCategory constructor
// order.  Keeping this explicit avoids coupling Data.Char to util enum order.
static int data_char_category_ordinal(unicode::Category category)
{
    switch(category)
    {
        case unicode::Category::uppercase_letter: return 0;
        case unicode::Category::lowercase_letter: return 1;
        case unicode::Category::titlecase_letter: return 2;
        case unicode::Category::modifier_letter: return 3;
        case unicode::Category::other_letter: return 4;
        case unicode::Category::non_spacing_mark: return 5;
        case unicode::Category::spacing_combining_mark: return 6;
        case unicode::Category::enclosing_mark: return 7;
        case unicode::Category::decimal_number: return 8;
        case unicode::Category::letter_number: return 9;
        case unicode::Category::other_number: return 10;
        case unicode::Category::connector_punctuation: return 11;
        case unicode::Category::dash_punctuation: return 12;
        case unicode::Category::open_punctuation: return 13;
        case unicode::Category::close_punctuation: return 14;
        case unicode::Category::initial_quote: return 15;
        case unicode::Category::final_quote: return 16;
        case unicode::Category::other_punctuation: return 17;
        case unicode::Category::math_symbol: return 18;
        case unicode::Category::currency_symbol: return 19;
        case unicode::Category::modifier_symbol: return 20;
        case unicode::Category::other_symbol: return 21;
        case unicode::Category::space: return 22;
        case unicode::Category::line_separator: return 23;
        case unicode::Category::paragraph_separator: return 24;
        case unicode::Category::control: return 25;
        case unicode::Category::format: return 26;
        case unicode::Category::surrogate: return 27;
        case unicode::Category::private_use: return 28;
        case unicode::Category::not_assigned: return 29;
    }
    std::abort();
}

extern "C" R::Exp simple_function_isDigit(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(is_ascii_digit(c));
}

extern "C" R::Exp simple_function_isControl(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(unicode::category(c) == unicode::Category::control);
}

extern "C" R::Exp simple_function_isSpace(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(c == U' ' or
                    (U'\t' <= c and c <= U'\r') or
                    c == U'\xA0' or
                    unicode::category(c) == unicode::Category::space);
}

extern "C" R::Exp simple_function_isLower(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(unicode::category(c) == unicode::Category::lowercase_letter);
}

extern "C" R::Exp simple_function_isUpper(vector<R::Exp>& args)
{
    auto c = char_arg(args);
    auto category = unicode::category(c);

    return bool_int(category == unicode::Category::uppercase_letter or
                    category == unicode::Category::titlecase_letter);
}

extern "C" R::Exp simple_function_isAlphaNum(vector<R::Exp>& args)
{
    auto c = char_arg(args);
    auto category = unicode::category(c);

    return bool_int(unicode::is_letter(category) or unicode::is_number(category));
}

extern "C" R::Exp simple_function_isAlpha(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(unicode::is_letter(unicode::category(c)));
}

extern "C" R::Exp simple_function_isPrint(vector<R::Exp>& args)
{
    auto c = char_arg(args);
    auto category = unicode::category(c);

    return bool_int(category != unicode::Category::line_separator and
                    category != unicode::Category::paragraph_separator and
                    category != unicode::Category::control and
                    category != unicode::Category::format and
                    category != unicode::Category::surrogate and
                    category != unicode::Category::private_use and
                    category != unicode::Category::not_assigned);
}

extern "C" R::Exp simple_function_isPunctuation(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(unicode::is_punctuation(unicode::category(c)));
}

extern "C" R::Exp simple_function_isHexDigit(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return bool_int(is_ascii_hex_digit(c));
}

// Return the Data.Char GeneralCategory ordinal.  Data.Char decodes this into
// constructors so Haskell code can inspect exact category subsets.
extern "C" R::Exp simple_function_generalCategory(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return data_char_category_ordinal(unicode::category(c));
}

extern "C" R::Exp simple_function_toLower(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return unicode::to_lower(c);
}

extern "C" R::Exp simple_function_toUpper(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return unicode::to_upper(c);
}

// Apply the simple Unicode titlecase mapping for a single Runtime Char.  Full
// string titlecasing remains outside this Char-level builtin.
extern "C" R::Exp simple_function_toTitle(vector<R::Exp>& args)
{
    auto c = char_arg(args);

    return unicode::to_title(c);
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
