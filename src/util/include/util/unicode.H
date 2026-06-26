#ifndef UTIL_UNICODE_H
#define UTIL_UNICODE_H

#include <cstdint>

namespace unicode
{

enum class Category
{
    uppercase_letter,
    lowercase_letter,
    titlecase_letter,
    modifier_letter,
    other_letter,
    non_spacing_mark,
    spacing_combining_mark,
    enclosing_mark,
    decimal_number,
    letter_number,
    other_number,
    connector_punctuation,
    dash_punctuation,
    open_punctuation,
    close_punctuation,
    initial_quote,
    final_quote,
    other_punctuation,
    math_symbol,
    currency_symbol,
    modifier_symbol,
    other_symbol,
    space,
    line_separator,
    paragraph_separator,
    control,
    format,
    surrogate,
    private_use,
    not_assigned
};

Category category(char32_t c);

char32_t to_upper(char32_t c);
char32_t to_lower(char32_t c);
char32_t to_title(char32_t c);

bool is_letter(Category category);
bool is_mark(Category category);
bool is_number(Category category);
bool is_punctuation(Category category);
bool is_symbol(Category category);
bool is_separator(Category category);

}

#endif
