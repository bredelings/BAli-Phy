#include "util/text.H"
#include <cstdlib>
#include <iostream>
#include <string>

// Compare wrapped text against the expected byte sequence so regressions that
// split UTF-8 scalar values show up as direct test failures.
static void check_equal(const std::string& name, const std::string& observed, const std::string& expected)
{
    if (observed == expected)
        return;

    std::cerr<<"Unexpected "<<name<<" wrapping:\n"
             <<"Observed: "<<observed<<"\n"
             <<"Expected: "<<expected<<"\n";
    std::exit(1);
}

// Exercise text wrapping with UTF-8 text both with and without ANSI escape
// sequences, since diagnostics use the same wrapping path.
int main()
{
    check_equal("UTF-8", indent_and_wrap(0, 4, "α β γ"), "α β\nγ");

    auto colored_alpha = red("α");
    check_equal("ANSI UTF-8",
                indent_and_wrap(0, 4, colored_alpha + " β γ"),
                colored_alpha + " β\nγ");
}
