#include "computation/parser/driver.hh"
#include "test-util.hh"

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
        "{-# LANGUAGE NoImplicitPrelude #-}\n"
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

    auto parsed = parse_module_file(replace_newlines(source, newline), "Newlines.hs", LanguageExtensions{});
    require(parsed.source == source, "newline test did not retain normalized source");

    const auto& module = parsed.module;
    require(module.topdecls.has_value(), "newline test module has no declarations");
    require(module.topdecls->size() == 2, "newline test module has the wrong declaration count");

    const auto& function = module.topdecls->back();
    require(function.loc.has_value(), "newline test function has no source location");
    require(function.loc->begin.line == 8, "newline normalization produced the wrong declaration line");
}

// Execution tests cannot detect lost filename ownership or range formatting. Keep these checks
// while locations borrow full pathnames and diagnostics shorten them only when rendering.
void check_location_printing()
{
    namespace fs = std::filesystem;
    const std::string pathname = (fs::path("project") / "haskell" / "Data" / "Bad.hs").string();
    const std::string other = (fs::path("other") / "haskell" / "Data" / "Bad.hs").string();
    const std::string display = (fs::path("Data") / "Bad.hs").string();
    require(pretty_module_path(fs::path("project") / "Main.hs") == "Main.hs",
            "ordinary source paths should display only the basename");
    const yy::location loc{yy::position(&pathname, 2, 2), yy::position(&pathname, 2, 5)};
    require(print_location(loc) == display + ":2.2-4", "single-line diagnostic range changed");
    require(print_location({yy::position(&pathname, 2, 2), yy::position(&pathname, 3, 5)}) == display + ":2.2-3.4",
            "multiline diagnostic range changed");
    require(print_location({yy::position(&pathname, 2, 2), yy::position(&pathname, 2, 2)}) == display + ":2.2",
            "empty diagnostic range changed");
    require(print_location({yy::position(nullptr, 2, 2), yy::position(nullptr, 2, 5)}) == "2.2-4",
            "locations without filenames should still display line and column ranges");
    require(print_location({yy::position(&pathname, 2, 2), yy::position(&other, 3, 5)}) == display + ":2.2-" + display + ":3.4",
            "distinct full pathnames must remain distinct even if display names match");
    auto rendered = warning(loc, "test warning").print({pathname, "\nabcde\n"});
    require(rendered.find(display + ":2.2-4:") != std::string::npos and
            rendered.find(pathname) == std::string::npos, "diagnostics should use the location formatter");
    require(loc.begin.filename == &pathname and loc.end.filename == &pathname and
            pathname == (fs::path("project") / "haskell" / "Data" / "Bad.hs").string(),
            "printing must not change stored filenames or location pointers");
}
}

// Checks LF, CRLF, CR, and form-feed source with layout, comments, pragmas, and locations.
void run_lexer_tests()
{
    check_location_printing();
    check_newline_form("\n");
    check_newline_form("\r\n");
    check_newline_form("\r");
    check_newline_form("\f");
}
}
