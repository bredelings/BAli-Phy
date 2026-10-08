#include "util/text.hh"
#include "util/file-readers.hh"
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

// Exercise utility behavior used by diagnostics and sampled-data readers.
int main()
{
    check_equal("UTF-8", indent_and_wrap(0, 4, "α β γ"), "α β\nγ");

    auto colored_alpha = red("α");
    check_equal("ANSI UTF-8",
                indent_and_wrap(0, 4, colored_alpha + " β γ"),
                colored_alpha + " β\nγ");

    // Check thinning boundaries and retained order, including exhaustion before the last element.
    // Tool tests do not cover these sample sizes reliably; retire if this thinning helper is replaced.
    for (int limit: {-1, 2, 3, 4})
    {
        std::list<int> samples = {0, 1, 2};
        bool changed = thin_down_to(samples, limit);
        const std::list<int> expected = limit == 2 ? std::list<int>{0, 2} : std::list<int>{0, 1, 2};
        if (samples != expected or changed != (limit == 2))
        {
            std::cerr<<"Unexpected sample thinning at limit "<<limit<<"\n";
            return 1;
        }
    }
}
