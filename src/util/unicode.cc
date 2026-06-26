#include "util/unicode.H"

#include "util/myexception.H"
#include "util/utf8.H"

#include <utf8proc.h>

#include <cstdlib>

namespace unicode
{

// Convert utf8proc's category enum into the project-local enum so callers do
// not need to depend on utf8proc headers or category names directly.
static Category convert_category(utf8proc_category_t category)
{
    switch(category)
    {
        case UTF8PROC_CATEGORY_LU: return Category::uppercase_letter;
        case UTF8PROC_CATEGORY_LL: return Category::lowercase_letter;
        case UTF8PROC_CATEGORY_LT: return Category::titlecase_letter;
        case UTF8PROC_CATEGORY_LM: return Category::modifier_letter;
        case UTF8PROC_CATEGORY_LO: return Category::other_letter;
        case UTF8PROC_CATEGORY_MN: return Category::non_spacing_mark;
        case UTF8PROC_CATEGORY_MC: return Category::spacing_combining_mark;
        case UTF8PROC_CATEGORY_ME: return Category::enclosing_mark;
        case UTF8PROC_CATEGORY_ND: return Category::decimal_number;
        case UTF8PROC_CATEGORY_NL: return Category::letter_number;
        case UTF8PROC_CATEGORY_NO: return Category::other_number;
        case UTF8PROC_CATEGORY_PC: return Category::connector_punctuation;
        case UTF8PROC_CATEGORY_PD: return Category::dash_punctuation;
        case UTF8PROC_CATEGORY_PS: return Category::open_punctuation;
        case UTF8PROC_CATEGORY_PE: return Category::close_punctuation;
        case UTF8PROC_CATEGORY_PI: return Category::initial_quote;
        case UTF8PROC_CATEGORY_PF: return Category::final_quote;
        case UTF8PROC_CATEGORY_PO: return Category::other_punctuation;
        case UTF8PROC_CATEGORY_SM: return Category::math_symbol;
        case UTF8PROC_CATEGORY_SC: return Category::currency_symbol;
        case UTF8PROC_CATEGORY_SK: return Category::modifier_symbol;
        case UTF8PROC_CATEGORY_SO: return Category::other_symbol;
        case UTF8PROC_CATEGORY_ZS: return Category::space;
        case UTF8PROC_CATEGORY_ZL: return Category::line_separator;
        case UTF8PROC_CATEGORY_ZP: return Category::paragraph_separator;
        case UTF8PROC_CATEGORY_CC: return Category::control;
        case UTF8PROC_CATEGORY_CF: return Category::format;
        case UTF8PROC_CATEGORY_CS: return Category::surrogate;
        case UTF8PROC_CATEGORY_CO: return Category::private_use;
        case UTF8PROC_CATEGORY_CN: return Category::not_assigned;
    }
    std::abort();
}

// Look up the Unicode general category for one scalar value.  Rejecting
// non-scalars here keeps category predicates aligned with Runtime Char.
Category category(char32_t c)
{
    if (not utf8::is_scalar_value(c))
        throw myexception()<<"Invalid Unicode scalar value: "<<static_cast<std::uint32_t>(c);

    return convert_category(utf8proc_category(static_cast<utf8proc_int32_t>(c)));
}

// Apply utf8proc's simple uppercase mapping for one code point.  Multi-code
// point case mappings are deliberately outside the Runtime Char operation.
char32_t to_upper(char32_t c)
{
    return static_cast<char32_t>(utf8proc_toupper(static_cast<utf8proc_int32_t>(c)));
}

// Apply utf8proc's simple lowercase mapping for one code point.  Multi-code
// point case mappings are deliberately outside the Runtime Char operation.
char32_t to_lower(char32_t c)
{
    return static_cast<char32_t>(utf8proc_tolower(static_cast<utf8proc_int32_t>(c)));
}

// Apply utf8proc's simple titlecase mapping for one code point.  Multi-code
// point case mappings are deliberately outside the Runtime Char operation.
char32_t to_title(char32_t c)
{
    return static_cast<char32_t>(utf8proc_totitle(static_cast<utf8proc_int32_t>(c)));
}

// Recognize the five Unicode letter categories.  This is generic Unicode
// grouping, not Haskell identifier policy.
bool is_letter(Category category)
{
    return category == Category::uppercase_letter or
           category == Category::lowercase_letter or
           category == Category::titlecase_letter or
           category == Category::modifier_letter or
           category == Category::other_letter;
}

// Recognize the three Unicode mark categories.  Callers decide which mark
// categories are valid in their own syntax.
bool is_mark(Category category)
{
    return category == Category::non_spacing_mark or
           category == Category::spacing_combining_mark or
           category == Category::enclosing_mark;
}

// Recognize the three Unicode number categories.  This is broader than
// Haskell's isDigit, which remains ASCII-only.
bool is_number(Category category)
{
    return category == Category::decimal_number or
           category == Category::letter_number or
           category == Category::other_number;
}

// Recognize all Unicode punctuation categories.  Haskell operator syntax uses
// only a subset of these categories and is handled in haskell/ids.cc.
bool is_punctuation(Category category)
{
    return category == Category::connector_punctuation or
           category == Category::dash_punctuation or
           category == Category::open_punctuation or
           category == Category::close_punctuation or
           category == Category::initial_quote or
           category == Category::final_quote or
           category == Category::other_punctuation;
}

// Recognize the four Unicode symbol categories.  This generic grouping is also
// broader than Haskell's concrete operator syntax.
bool is_symbol(Category category)
{
    return category == Category::math_symbol or
           category == Category::currency_symbol or
           category == Category::modifier_symbol or
           category == Category::other_symbol;
}

// Recognize the Unicode separator categories.  Language-specific whitespace
// rules should still decide whether line/paragraph separators are accepted.
bool is_separator(Category category)
{
    return category == Category::space or
           category == Category::line_separator or
           category == Category::paragraph_separator;
}

}
