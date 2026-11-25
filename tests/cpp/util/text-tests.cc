#include "util/text.hh"
#include "util/concat_view.hh"
#include "util/file-readers.hh"
#include "util/string/format-double.hh"
#include <bit>
#include <charconv>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <string>

// Compare exact output and show both strings when their spelling differs.
static void check_equal(const std::string& name, const std::string& observed, const std::string& expected)
{
    if (observed == expected)
        return;

    std::cerr<<"Unexpected "<<name<<":\n"
             <<"Observed: "<<observed<<"\n"
             <<"Expected: "<<expected<<"\n";
    std::exit(1);
}

// Exercise utility behavior used by diagnostics and sampled-data readers.
int main()
{
    // Text consumers need intact exponents and distinguishable adjacent doubles. The Newick
    // examples exercise ordinary branch lengths; these also check the limits of the formatter.
    check_equal("integral double", format_double(1.0), "1");
    check_equal("integer ending in zero", format_double(120.0), "120");
    check_equal("fraction", format_double(1.25), "1.25");
    check_equal("negative exponent", format_double(1.25e-10), "1.25e-10");
    check_equal("positive exponent", format_double(1.25e100), "1.25e+100");
    check_equal("next double after one", format_double(std::nextafter(1.0, 2.0)), "1.0000000000000002");
    for (double value: {-0.0, std::numeric_limits<double>::denorm_min(), std::numeric_limits<double>::max()})
    {
        auto text = format_double(value);
        double parsed = 0;
        auto [end, error] = std::from_chars(text.data(), text.data() + text.size(), parsed);
        if (error != std::errc() or end != text.data() + text.size() or
            std::bit_cast<std::uint64_t>(parsed) != std::bit_cast<std::uint64_t>(value))
        {
            std::cerr<<"Double failed to round-trip through '"<<text<<"'\n";
            return 1;
        }
    }

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

    // Check reference preservation, empty boundaries, and ownership directly: application tests do
    // not isolate these iterator properties. Retire when std::views::concat replaces this view.
    std::vector<int> empty, first{1, 2}, last{3};
    auto joined = concat_view(empty, first, empty, last, empty);
    static_assert(std::ranges::forward_range<decltype(joined)>);
    for (auto& value: joined)
        value *= 2;
    const std::vector<int> expected{2, 4, 6};
    const auto owned = concat_view(std::vector<int>{2, 4}, std::vector<int>{6});
    const auto mixed = concat_view(first, std::as_const(last));
    static_assert(std::same_as<std::ranges::range_reference_t<decltype(mixed)>, const int&>);
    auto current = joined.begin();
    auto previous = current++;
    if (first != std::vector<int>{2, 4} or last != std::vector<int>{6} or
        std::ranges::to<std::vector>(joined) != expected or
        std::ranges::to<std::vector>(owned) != expected or
        std::ranges::to<std::vector>(mixed) != expected or
        concat_view(empty, empty).begin() != std::default_sentinel or
        *previous != 2 or *current != 4)
    {
        std::cerr<<"Unexpected concat_view traversal or reference behavior\n";
        return 1;
    }

}
