#include "computation/haskell/cpp.hh"
#include "test-util.hh"

#include <string>

namespace bali_phy_test
{
namespace
{
    using Haskell::CPP::Options;

    std::string notes(const Haskell::CPP::Result& result)
    {
        std::string text;
        for(const auto& message: result.messages)
            for(const auto& note: message.notes)
                text += note.print() + "\n";
        return text;
    }

    // Direct checks preserve source coordinates and selection semantics that
    // become obsolete only if CPP is delegated to another tested implementation.
    void check_selection_and_lines()
    {
        const std::string source =
            "first\n"
            "#define PICK 1\n"
            "#if PICK\n"
            "chosen\n"
            "#elif 1\n"
            "wrong\n"
            "#endif\n"
            "last";
        auto result = Haskell::CPP::conditionals(source, "Selection.hs", {});
        require(result.messages.empty(), "selection produced a diagnostic");
        require(result.source == "first\n\n\nchosen\n\n\n\nlast",
                "selection did not preserve physical lines");

        auto nested = Haskell::CPP::conditionals(
            "#if 0\ninvalid {\n#if 1 / 0\nno\n#endif\n#else\nyes\n#endif\n",
            "Nested.hs", {});
        require(nested.messages.empty(), "inactive condition was evaluated");
        require(nested.source == "\n\n\n\n\n\nyes\n\n", "nested branch selection failed");

        Options configured;
        configured.definitions.push_back({"ON", {}, "1"});
        configured.definitions.push_back({"REMOVED", {}, "1"});
        configured.undefinitions.push_back("REMOVED");
        auto definitions = Haskell::CPP::conditionals(
            "#if ON && !defined(REMOVED)\nkept\n#endif\n"
            "#undef ON /* accepted trailing comment */\n#ifdef ON\nwrong\n#endif\n",
            "Definitions.hs", configured);
        require(definitions.messages.empty(), "definition ordering produced a diagnostic");
        require(definitions.source == "\nkept\n\n\n\n\n\n",
                "initial or source definition ordering failed");

        std::string diagnostic;
        auto parsed_definition = Haskell::CPP::parse_initial_definition("VALUE= 3 ", diagnostic);
        require(parsed_definition and parsed_definition->replacement == " 3 ",
                "command-line definition did not preserve its replacement text");
    }

    // The expression table fixes deterministic arithmetic, macro expansion,
    // and short-circuit behavior until a separately tested implementation replaces it.
    void check_expressions()
    {
        const std::string source =
            "#define THREE 3\n"
            "#define AT_LEAST(a,b,c) ((a)*10000 + (b)*100 + (c) >= 40100)\n"
            "#define HAS defined(THREE)\n"
            "#if 1 + 2 * 3 == 7 && 0x10 == 020 && 0b10U == 2 && AT_LEAST(4,1,0) && HAS\n"
            "ok\n"
            "#endif\n"
            "#if 0 && (1 / 0)\n"
            "bad\n"
            "#endif\n"
            "#if 1 ? 1 : (1 % 0)\n"
            "also_ok\n"
            "#endif\n";
        auto result = Haskell::CPP::conditionals(source, "Expressions.hs", {});
        require(result.messages.empty(), "valid expression table produced a diagnostic: " + notes(result));
        require(result.source.find("ok\n") != std::string::npos and
                result.source.find("also_ok\n") != std::string::npos and
                result.source.find("bad\n") == std::string::npos,
                "expression selection was incorrect");

        const std::string arithmetic =
            "#if (-3 / 2 == -1) && (-3 % 2 == -1) && ((-3 >> 1) == -2) && "
            "(~0 == -1) && (1 << 4 == 16) && (3 < 4 ? 5 : 0)\n"
            "arithmetic_ok\n#endif\n";
        auto arithmetic_result = Haskell::CPP::conditionals(arithmetic, "Arithmetic.hs", {});
        require(arithmetic_result.messages.empty() and
                arithmetic_result.source.find("arithmetic_ok") != std::string::npos,
                "deterministic signed arithmetic was incorrect");

        auto recursive = Haskell::CPP::conditionals(
            "#define A B\n#define B A\n#if A\nwrong\n#else\nrecursive_ok\n#endif\n",
            "Recursive.hs", {});
        require(recursive.messages.empty() and recursive.source.find("recursive_ok") != std::string::npos,
                "recursive macro suppression failed");

        auto arity = Haskell::CPP::conditionals("#define F(x) x\n#if F(1,2)\nx\n#endif\n", "Arity.hs", {});
        require(arity.messages.size() == 1 and notes(arity).find("expects 1 arguments") != std::string::npos,
                "macro arity error was not diagnosed");
        auto character = Haskell::CPP::conditionals("#if 'x'\nx\n#endif\n", "Char.hs", {});
        require(notes(character).find("character constants") != std::string::npos,
                "character constants were not explicitly rejected");
        auto malformed_short_circuit = Haskell::CPP::conditionals("#if 0 && 09\nx\n#endif\n", "Octal.hs", {});
        require(notes(malformed_short_circuit).find("base-8") != std::string::npos,
                "short-circuiting suppressed malformed literal validation");
    }

    // Shielding cases protect Haskell text from directive recognition and are
    // unnecessary only if preprocessing moves to another tested implementation.
    void check_haskell_shielding()
    {
        const std::string source =
            "text = \"#if 0\"\n"
            "character = '#'\n"
            "{- outer\n"
            "   {- nested -}\n"
            "#error hidden\n"
            "-}\n"
            "gap = \"a\\\n"
            "  \\b\"\n"
            "-- #error hidden too\n"
            "{-# INLINE gap #-}\n";
        auto result = Haskell::CPP::conditionals(source, "Shield.hs", {});
        require(result.messages.empty(), "shielded directive text was interpreted");
        require(result.source == source, "ordinary Haskell source was changed");
    }

    // Diagnostics are checked directly because loader fixtures cannot compactly
    // cover structural errors, warnings, and deterministic resource limits.
    void check_diagnostics()
    {
        auto unmatched = Haskell::CPP::conditionals("#endif\n", "Unmatched.hs", {});
        require(notes(unmatched).find("without a matching") != std::string::npos,
                "unmatched endif was not diagnosed");

        auto unterminated = Haskell::CPP::conditionals("#if 1\nx\n", "Unterminated.hs", {});
        require(notes(unterminated).find("unterminated") != std::string::npos,
                "unterminated conditional was not diagnosed");

        auto warning = Haskell::CPP::conditionals("#warning careful\nx\n", "Warning.hs", {});
        require(warning.messages.size() == 1 and warning.messages.front().is_warning(),
                "#warning did not produce a warning");

        auto structural = Haskell::CPP::conditionals(
            "#if 1\n#else\n#else\n#endif\n#include \"x.h\"\n#define 9BAD 1\n#error stopped\n",
            "Diagnostics.hs", {});
        auto structural_notes = notes(structural);
        require(structural_notes.find("duplicate #else") != std::string::npos and
                structural_notes.find("#include is not supported") != std::string::npos and
                structural_notes.find("expected a macro name") != std::string::npos and
                structural_notes.find("stopped") != std::string::npos,
                "structural directive diagnostics were incomplete");

        Options limited;
        limited.maximum_integer_bits = 3;
        auto bits = Haskell::CPP::conditionals("#if 8\nx\n#endif\n", "Bits.hs", limited);
        require(notes(bits).find("integer-bit budget") != std::string::npos,
                "integer budget was not enforced");

        Options shallow;
        shallow.maximum_conditional_depth = 1;
        auto depth = Haskell::CPP::conditionals("#if 1\n#if 1\n#endif\n#endif\n", "Depth.hs", shallow);
        require(notes(depth).find("nesting exceeds") != std::string::npos,
                "conditional depth budget was not enforced");

        Options shallow_expression;
        shallow_expression.maximum_expression_depth = 2;
        auto expression_depth = Haskell::CPP::conditionals(
            "#if (((1)))\nx\n#endif\n", "ExpressionDepth.hs", shallow_expression);
        require(notes(expression_depth).find("expression-depth budget") != std::string::npos,
                "expression depth budget was not enforced");

        Options few_tokens;
        few_tokens.maximum_expanded_tokens = 2;
        auto tokens = Haskell::CPP::conditionals("#if 1 + 1\nx\n#endif\n", "Tokens.hs", few_tokens);
        require(notes(tokens).find("expanded-token budget") != std::string::npos,
                "expanded-token budget was not enforced");

        auto continued = Haskell::CPP::conditionals("#if \\\n1 / 0\nx\n#endif\n", "Continued.hs", {});
        require(continued.messages.size() == 1 and continued.messages.front().loc and
                continued.messages.front().loc->begin.line == 2 and
                continued.messages.front().loc->begin.column == 3,
                "continued-directive diagnostic did not retain its physical source location");
    }
}

// Runs the cohesive conditional-CPP component groups.
void run_cpp_tests()
{
    check_selection_and_lines();
    check_expressions();
    check_haskell_shielding();
    check_diagnostics();
}
}
