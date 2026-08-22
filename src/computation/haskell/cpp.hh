#ifndef HASKELL_CPP_H
#define HASKELL_CPP_H

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include "computation/message.hh"

namespace Haskell::CPP
{
    struct Definition
    {
        std::string name;
        std::optional<std::vector<std::string>> parameters;
        std::string replacement;
    };

    struct Options
    {
        std::vector<Definition> definitions;
        std::vector<std::string> undefinitions;
        unsigned maximum_conditional_depth = 200;
        unsigned maximum_expansion_depth = 200;
        unsigned maximum_expression_depth = 200;
        std::size_t maximum_expanded_tokens = 1'000'000;
        std::size_t maximum_integer_bits = 1'000'000;
    };

    struct Result
    {
        std::string source;
        std::vector<Message> messages;
    };

    // Parse command-line definition and undefinition operands using the same
    // portable macro-name and parameter rules as source directives.
    std::optional<Definition> parse_initial_definition(std::string_view text,
                                                       std::string& diagnostic);
    bool parse_initial_undefinition(std::string_view text, std::string& name,
                                    std::string& diagnostic);

    Result conditionals(const std::string& source,
                        const std::string& display_name,
                        const Options& options);
}

#endif
