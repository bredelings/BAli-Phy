#include "computation/parser/driver.hh"
#include "test-util.H"

#include <string>
#include <string_view>

namespace bali_phy_test
{
namespace
{
// Replace the LF markers in a test module with one Haskell newline representation.
std::string replace_newlines(std::string_view source, std::string_view newline)
{
    std::string result;
    for(char c: source)
        if (c == '\n')
            result += newline;
        else
            result += c;
    return result;
}

// Parse one newline representation and check that declaration locations retain logical line numbers.
void check_newline_form(std::string_view newline)
{
    constexpr std::string_view source =
        "module Main where\n"
        "-- line comment\n"
        "{- outer comment\n"
        "   {- nested comment -}\n"
        "-}\n"
        "{-# INLINE f #-}\n"
        "f = let\n"
        "      x = 1\n"
        "      y = 2\n"
        "    in x + y\n";

    auto module = parse_module_file(replace_newlines(source, newline), "Newlines.hs", LanguageExtensions{});
    require(module.topdecls.has_value(), "newline test module has no declarations");
    require(module.topdecls->size() == 2, "newline test module has the wrong declaration count");

    const auto& function = module.topdecls->back();
    require(function.loc.has_value(), "newline test function has no source location");
    require(function.loc->begin.line == 7, "newline normalization produced the wrong declaration line");
}
}

// Checks LF, CRLF, CR, and form-feed source with layout, comments, pragmas, and locations.
void run_lexer_tests()
{
    check_newline_form("\n");
    check_newline_form("\r\n");
    check_newline_form("\r");
    check_newline_form("\f");
}
}
