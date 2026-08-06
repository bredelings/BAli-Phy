#include "cpp.H"

#include <algorithm>
#include <cctype>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <utility>
#include <boost/multiprecision/cpp_int.hpp>
#include "util/utf8.H"

using boost::multiprecision::cpp_int;
using std::optional;
using std::string;
using std::string_view;
using std::vector;

namespace Haskell::CPP
{
namespace
{
    struct ParseFailure
    {
        string message;
        std::size_t column = 1;
    };

    enum class TokenKind { Integer, Identifier, Operator, LParen, RParen, Comma, Question, Colon, End };

    struct Token
    {
        TokenKind kind;
        string text;
        std::size_t column;
    };

    struct ConditionalFrame
    {
        bool parent_active;
        bool branch_taken;
        bool current_active;
        bool saw_else;
        yy::location opening_location;
    };

    enum class ShieldState { Normal, String, Character, StringGap, LineComment };

    struct Shield
    {
        ShieldState state = ShieldState::Normal;
        unsigned block_comment_depth = 0;
    };

    struct PhysicalLine
    {
        string_view text;
        bool has_newline;
        unsigned number;
    };

    struct SourcePoint
    {
        unsigned line;
        std::size_t column;
    };

    struct LogicalDirective
    {
        string text;
        vector<SourcePoint> origins;
    };

    bool macro_name_start(char c)
    {
        auto u = static_cast<unsigned char>(c);
        return std::isalpha(u) or c == '_';
    }

    bool macro_name_char(char c)
    {
        auto u = static_cast<unsigned char>(c);
        return std::isalnum(u) or c == '_';
    }

    string_view trim_horizontal(string_view text)
    {
        while(not text.empty() and (text.front() == ' ' or text.front() == '\t'))
            text.remove_prefix(1);
        while(not text.empty() and (text.back() == ' ' or text.back() == '\t'))
            text.remove_suffix(1);
        return text;
    }

    // Parse one portable identifier and leave pos immediately after it.
    optional<string> parse_name(string_view text, std::size_t& pos)
    {
        if (pos >= text.size() or not macro_name_start(text[pos])) return {};
        auto start = pos++;
        while(pos < text.size() and macro_name_char(text[pos])) pos++;
        return string(text.substr(start, pos - start));
    }

    void skip_horizontal(string_view text, std::size_t& pos)
    {
        while(pos < text.size() and (text[pos] == ' ' or text[pos] == '\t')) pos++;
    }

    // Accept horizontal whitespace and conventional C or Haskell comments after a directive operand.
    bool only_directive_trivia(string_view text, std::size_t pos)
    {
        while(true)
        {
            skip_horizontal(text, pos);
            if (pos == text.size()) return true;
            if (text.substr(pos).starts_with("//") or text.substr(pos).starts_with("--")) return true;
            string_view close;
            if (text.substr(pos).starts_with("/*")) close = "*/";
            else if (text.substr(pos).starts_with("{-")) close = "-}";
            else return false;
            auto end = text.find(close, pos + 2);
            if (end == string_view::npos) return false;
            pos = end + 2;
        }
    }

    // Validate the common NAME[(parameters)] portion used by source and command-line definitions.
    optional<Definition> parse_definition_parts(string_view head, string_view replacement,
                                                bool trim_replacement, string& diagnostic)
    {
        std::size_t pos = 0;
        auto name = parse_name(head, pos);
        if (not name)
        {
            diagnostic = "expected a macro name matching [A-Za-z_][A-Za-z0-9_]*";
            return {};
        }
        if (*name == "defined")
        {
            diagnostic = "the conditional operator 'defined' cannot be used as a macro name";
            return {};
        }

        optional<vector<string>> parameters;
        if (pos < head.size() and head[pos] == '(')
        {
            parameters.emplace();
            pos++;
            skip_horizontal(head, pos);
            if (pos < head.size() and head[pos] != ')')
            {
                while(true)
                {
                    if (head.substr(pos).starts_with("..."))
                    {
                        diagnostic = "variadic macros are not supported";
                        return {};
                    }
                    auto parameter = parse_name(head, pos);
                    if (not parameter)
                    {
                        diagnostic = "expected a macro parameter name";
                        return {};
                    }
                    if (std::ranges::find(*parameters, *parameter) != parameters->end())
                    {
                        diagnostic = "duplicate macro parameter '" + *parameter + "'";
                        return {};
                    }
                    parameters->push_back(*parameter);
                    skip_horizontal(head, pos);
                    if (pos < head.size() and head[pos] == ')') break;
                    if (pos >= head.size() or head[pos] != ',')
                    {
                        diagnostic = "expected ',' or ')' in macro parameter list";
                        return {};
                    }
                    pos++;
                    skip_horizontal(head, pos);
                }
            }
            if (pos >= head.size() or head[pos] != ')')
            {
                diagnostic = "unterminated macro parameter list";
                return {};
            }
            pos++;
        }

        skip_horizontal(head, pos);
        if (pos != head.size())
        {
            diagnostic = "unexpected text after macro name or parameter list";
            return {};
        }

        auto body = trim_replacement ? trim_horizontal(replacement) : replacement;
        if (body.find("##") != string_view::npos)
        {
            diagnostic = "macro token concatenation with '##' is not supported";
            return {};
        }
        if (body.find('#') != string_view::npos)
        {
            diagnostic = "macro stringification with '#' is not supported";
            return {};
        }
        if (body.find("...") != string_view::npos)
        {
            diagnostic = "variadic macros are not supported";
            return {};
        }
        return Definition{*name, std::move(parameters), string(body)};
    }

    // Split a source #define at the first whitespace after its name or parameter list.
    optional<Definition> parse_source_definition(string_view text, string& diagnostic)
    {
        text = trim_horizontal(text);
        std::size_t pos = 0;
        auto name = parse_name(text, pos);
        if (not name)
            return parse_definition_parts(text, {}, true, diagnostic);

        std::size_t head_end = pos;
        if (pos < text.size() and text[pos] == '(')
        {
            unsigned depth = 0;
            do
            {
                if (text[pos] == '(') depth++;
                else if (text[pos] == ')') depth--;
                pos++;
            }
            while(pos < text.size() and depth);
            head_end = pos;
        }
        auto replacement = pos < text.size() ? text.substr(pos) : string_view{};
        return parse_definition_parts(text.substr(0, head_end), replacement, true, diagnostic);
    }

    // Tokenize the deliberately small integer-expression language accepted by #if.
    vector<Token> tokenize(string_view text, std::size_t base_column)
    {
        vector<Token> result;
        std::size_t i = 0;
        while(i < text.size())
        {
            auto c = text[i];
            if (c == ' ' or c == '\t')
            {
                i++;
                continue;
            }
            if ((text.substr(i).starts_with("//")) or text.substr(i).starts_with("--"))
                break;
            if (text.substr(i).starts_with("/*"))
            {
                auto end = text.find("*/", i + 2);
                if (end == string_view::npos)
                    throw ParseFailure{"unterminated comment in conditional expression", base_column + i};
                i = end + 2;
                continue;
            }
            if (text.substr(i).starts_with("{-"))
            {
                auto end = text.find("-}", i + 2);
                if (end == string_view::npos)
                    throw ParseFailure{"unterminated comment in conditional expression", base_column + i};
                i = end + 2;
                continue;
            }
            if (c == '\'')
                throw ParseFailure{"character constants are not supported in CPP expressions", base_column + i};
            if (macro_name_start(c))
            {
                auto start = i++;
                while(i < text.size() and macro_name_char(text[i])) i++;
                result.push_back({TokenKind::Identifier, string(text.substr(start, i - start)), base_column + start});
                continue;
            }
            if (std::isdigit(static_cast<unsigned char>(c)))
            {
                auto start = i++;
                while(i < text.size() and std::isalnum(static_cast<unsigned char>(text[i]))) i++;
                result.push_back({TokenKind::Integer, string(text.substr(start, i - start)), base_column + start});
                continue;
            }

            auto two = i + 1 < text.size() ? text.substr(i, 2) : string_view{};
            if (two == "||" or two == "&&" or two == "==" or two == "!=" or
                two == "<=" or two == ">=" or two == "<<" or two == ">>")
            {
                result.push_back({TokenKind::Operator, string(two), base_column + i});
                i += 2;
                continue;
            }

            TokenKind kind = TokenKind::Operator;
            if (c == '(') kind = TokenKind::LParen;
            else if (c == ')') kind = TokenKind::RParen;
            else if (c == ',') kind = TokenKind::Comma;
            else if (c == '?') kind = TokenKind::Question;
            else if (c == ':') kind = TokenKind::Colon;
            else if (string("|^&<>+-*/%!~").find(c) == string::npos)
                throw ParseFailure{"invalid character in conditional expression", base_column + i};
            result.push_back({kind, string(1, c), base_column + i});
            i++;
        }
        result.push_back({TokenKind::End, {}, base_column + text.size()});
        return result;
    }

    struct Expander
    {
        const std::map<string, Definition>& definitions;
        const Options& options;
        std::size_t produced_tokens = 0;

        void count(std::size_t n, std::size_t column)
        {
            if (n > options.maximum_expanded_tokens - std::min(produced_tokens, options.maximum_expanded_tokens))
                throw ParseFailure{"CPP expression exceeds the expanded-token budget", column};
            produced_tokens += n;
        }

        // Expand directive-expression macros while suppressing the currently expanding definition.
        vector<Token> expand(const vector<Token>& input, std::set<string> disabled,
                             unsigned depth, vector<string> chain)
        {
            if (depth > options.maximum_expansion_depth)
            {
                auto detail = chain.empty() ? string() : " while expanding '" + chain.back() + "'";
                throw ParseFailure{"CPP macro expansion depth exceeded" + detail,
                                   input.empty() ? 1 : input.front().column};
            }

            vector<Token> output;
            for(std::size_t i = 0; i < input.size() and input[i].kind != TokenKind::End; i++)
            {
                const auto& token = input[i];
                if (token.kind == TokenKind::Identifier and token.text == "defined")
                {
                    auto before = output.size();
                    output.push_back(token);
                    if (i + 1 < input.size() and input[i + 1].kind == TokenKind::LParen)
                    {
                        output.push_back(input[++i]);
                        if (i + 1 < input.size()) output.push_back(input[++i]);
                        if (i + 1 < input.size() and input[i + 1].kind == TokenKind::RParen)
                            output.push_back(input[++i]);
                    }
                    else if (i + 1 < input.size())
                        output.push_back(input[++i]);
                    count(output.size() - before, token.column);
                    continue;
                }

                auto definition = token.kind == TokenKind::Identifier ? definitions.find(token.text) : definitions.end();
                if (definition == definitions.end() or disabled.count(token.text))
                {
                    output.push_back(token);
                    count(1, token.column);
                    continue;
                }

                const auto& macro = definition->second;
                vector<vector<Token>> arguments;
                std::size_t after = i + 1;
                if (macro.parameters)
                {
                    if (after >= input.size() or input[after].kind != TokenKind::LParen)
                    {
                        output.push_back(token);
                        count(1, token.column);
                        continue;
                    }
                    after++;
                    vector<Token> argument;
                    int nesting = 0;
                    if (after < input.size() and input[after].kind == TokenKind::RParen)
                    {
                        if (not macro.parameters->empty()) arguments.emplace_back();
                        after++;
                    }
                    else
                    {
                        for(; after < input.size(); after++)
                        {
                            auto part = input[after];
                            if (part.kind == TokenKind::End)
                                throw ParseFailure{"unterminated macro invocation", token.column};
                            if (part.kind == TokenKind::LParen) nesting++;
                            if (part.kind == TokenKind::RParen)
                            {
                                if (nesting == 0)
                                {
                                    arguments.push_back(argument);
                                    after++;
                                    break;
                                }
                                nesting--;
                            }
                            if (part.kind == TokenKind::Comma and nesting == 0)
                            {
                                arguments.push_back(argument);
                                argument.clear();
                            }
                            else
                                argument.push_back(std::move(part));
                        }
                    }
                    if (arguments.size() != macro.parameters->size())
                        throw ParseFailure{"macro '" + macro.name + "' expects " +
                                           std::to_string(macro.parameters->size()) + " arguments but received " +
                                           std::to_string(arguments.size()), token.column};
                }

                auto replacement = tokenize(macro.replacement, token.column);
                vector<Token> substituted;
                for(const auto& part: replacement)
                {
                    if (part.kind == TokenKind::End) continue;
                    optional<std::size_t> parameter;
                    if (macro.parameters and part.kind == TokenKind::Identifier)
                        for(std::size_t p = 0; p < macro.parameters->size(); p++)
                            if ((*macro.parameters)[p] == part.text) parameter = p;
                    if (parameter)
                    {
                        auto expanded_argument = expand(arguments[*parameter], disabled, depth + 1, chain);
                        substituted.insert(substituted.end(), expanded_argument.begin(), expanded_argument.end());
                    }
                    else
                        substituted.push_back(part);
                }
                substituted.push_back({TokenKind::End, {}, token.column});
                auto next_disabled = disabled;
                next_disabled.insert(macro.name);
                auto next_chain = chain;
                next_chain.push_back(macro.name);
                auto expanded = expand(substituted, std::move(next_disabled), depth + 1, std::move(next_chain));
                output.insert(output.end(), expanded.begin(), expanded.end());
                if (macro.parameters) i = after - 1;
            }
            return output;
        }
    };

    // Resolve defined only after macro expansion so a replacement may introduce the operator.
    vector<Token> resolve_defined(vector<Token> tokens, const std::map<string, Definition>& definitions)
    {
        vector<Token> result;
        for(std::size_t i = 0; i < tokens.size(); i++)
        {
            auto token = tokens[i];
            if (token.kind == TokenKind::Identifier and token.text == "defined")
            {
                auto column = token.column;
                i++;
                bool parenthesized = i < tokens.size() and tokens[i].kind == TokenKind::LParen;
                if (parenthesized) i++;
                if (i >= tokens.size() or tokens[i].kind != TokenKind::Identifier)
                    throw ParseFailure{"expected a macro name after 'defined'", column};
                auto name = tokens[i].text;
                if (parenthesized)
                {
                    i++;
                    if (i >= tokens.size() or tokens[i].kind != TokenKind::RParen)
                        throw ParseFailure{"expected ')' after defined macro name", column};
                }
                result.push_back({TokenKind::Integer, definitions.count(name) ? "1" : "0", column});
            }
            else if (token.kind == TokenKind::Identifier)
                result.push_back({TokenKind::Integer, "0", token.column});
            else
                result.push_back(std::move(token));
        }
        result.push_back({TokenKind::End, {}, tokens.empty() ? 1 : tokens.back().column});
        return result;
    }

    class ExpressionParser
    {
        const vector<Token>& tokens;
        const Options& options;
        std::size_t pos = 0;

        const Token& peek() const { return tokens[pos]; }
        bool take(TokenKind kind, string_view text = {})
        {
            if (peek().kind != kind or (not text.empty() and peek().text != text)) return false;
            pos++;
            return true;
        }

        void check_bits(const cpp_int& value, std::size_t column) const
        {
            cpp_int magnitude = value < 0 ? -value : value;
            std::size_t bits = magnitude == 0 ? 0 : boost::multiprecision::msb(magnitude) + 1;
            if (bits > options.maximum_integer_bits)
                throw ParseFailure{"CPP integer exceeds the integer-bit budget", column};
        }

        cpp_int literal(const Token& token) const
        {
            auto text = token.text;
            std::size_t suffix = text.size();
            while(suffix and (text[suffix - 1] == 'u' or text[suffix - 1] == 'U' or
                              text[suffix - 1] == 'l' or text[suffix - 1] == 'L')) suffix--;
            auto suffix_text = text.substr(suffix);
            unsigned u_count = 0;
            unsigned l_count = 0;
            for(char c: suffix_text)
                if (c == 'u' or c == 'U') u_count++;
                else l_count++;
            auto normalized_suffix = suffix_text;
            std::ranges::transform(normalized_suffix, normalized_suffix.begin(),
                                   [](char c) { return std::tolower(static_cast<unsigned char>(c)); });
            static const std::set<string> valid_suffixes = {"", "u", "l", "ll", "ul", "ull", "lu", "llu"};
            if (u_count > 1 or l_count > 2 or not valid_suffixes.count(normalized_suffix))
                throw ParseFailure{"invalid integer suffix '" + suffix_text + "'", token.column};

            auto digits = text.substr(0, suffix);
            unsigned base = 10;
            std::size_t start = 0;
            if (digits.starts_with("0x") or digits.starts_with("0X")) { base = 16; start = 2; }
            else if (digits.starts_with("0b") or digits.starts_with("0B")) { base = 2; start = 2; }
            else if (digits.size() > 1 and digits[0] == '0') { base = 8; start = 1; }
            if (start == digits.size())
                throw ParseFailure{"integer literal has no digits", token.column};

            cpp_int value = 0;
            for(std::size_t i = start; i < digits.size(); i++)
            {
                unsigned digit;
                if (digits[i] >= '0' and digits[i] <= '9') digit = digits[i] - '0';
                else if (digits[i] >= 'a' and digits[i] <= 'f') digit = digits[i] - 'a' + 10;
                else if (digits[i] >= 'A' and digits[i] <= 'F') digit = digits[i] - 'A' + 10;
                else throw ParseFailure{"invalid digit in integer literal '" + text + "'", token.column};
                if (digit >= base)
                    throw ParseFailure{"invalid digit in base-" + std::to_string(base) + " integer literal", token.column};
                value *= base;
                value += digit;
                check_bits(value, token.column);
            }
            return value;
        }

        cpp_int primary(bool evaluate)
        {
            if (peek().kind == TokenKind::Integer)
            {
                auto token = tokens[pos++];
                auto value = literal(token);
                return evaluate ? value : cpp_int(0);
            }
            if (take(TokenKind::LParen))
            {
                auto value = conditional(evaluate);
                if (not take(TokenKind::RParen))
                    throw ParseFailure{"expected ')' in conditional expression", peek().column};
                return value;
            }
            throw ParseFailure{"expected an integer or '(' in conditional expression", peek().column};
        }

        cpp_int unary(bool evaluate)
        {
            if (peek().kind == TokenKind::Operator and
                (peek().text == "+" or peek().text == "-" or peek().text == "!" or peek().text == "~"))
            {
                auto op = tokens[pos++];
                auto value = unary(evaluate);
                if (not evaluate) return 0;
                if (op.text == "+") return value;
                if (op.text == "-") value = -value;
                else if (op.text == "!") value = value == 0;
                else value = ~value;
                check_bits(value, op.column);
                return value;
            }
            return primary(evaluate);
        }

        template <typename Next, typename Apply>
        cpp_int left_associative(bool evaluate, Next next, const std::set<string>& operators, Apply apply)
        {
            auto value = (this->*next)(evaluate);
            while(peek().kind == TokenKind::Operator and operators.count(peek().text))
            {
                auto op = tokens[pos++];
                auto right = (this->*next)(evaluate);
                if (evaluate)
                {
                    value = apply(op, value, right);
                    check_bits(value, op.column);
                }
            }
            return value;
        }

        cpp_int multiplicative(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::unary, {"*", "/", "%"},
                [](const Token& op, const cpp_int& a, const cpp_int& b) -> cpp_int {
                    if ((op.text == "/" or op.text == "%") and b == 0)
                        throw ParseFailure{op.text == "/" ? "division by zero" : "remainder by zero", op.column};
                    if (op.text == "*") return a * b;
                    if (op.text == "/") return a / b;
                    return a % b;
                });
        }

        cpp_int additive(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::multiplicative, {"+", "-"},
                [](const Token& op, const cpp_int& a, const cpp_int& b) -> cpp_int {
                    return op.text == "+" ? cpp_int(a + b) : cpp_int(a - b);
                });
        }

        cpp_int shift(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::additive, {"<<", ">>"},
                [this](const Token& op, const cpp_int& a, const cpp_int& b) -> cpp_int {
                    if (b < 0) throw ParseFailure{"negative shift count", op.column};
                    if (b > options.maximum_integer_bits)
                    {
                        if (op.text == ">>") return a < 0 ? cpp_int(-1) : cpp_int(0);
                        throw ParseFailure{"left shift exceeds the integer-bit budget", op.column};
                    }
                    auto count = b.convert_to<unsigned>();
                    if (op.text == "<<") return a << count;
                    // cpp_int right shift is arithmetic, matching an infinite two's-complement value.
                    return a >> count;
                });
        }

        cpp_int relational(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::shift, {"<", "<=", ">", ">="},
                [](const Token& op, const cpp_int& a, const cpp_int& b) -> cpp_int {
                    if (op.text == "<") return a < b;
                    if (op.text == "<=") return a <= b;
                    if (op.text == ">") return a > b;
                    return a >= b;
                });
        }

        cpp_int equality(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::relational, {"==", "!="},
                [](const Token& op, const cpp_int& a, const cpp_int& b) -> cpp_int {
                    return op.text == "==" ? a == b : a != b;
                });
        }

        cpp_int bit_and(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::equality, {"&"},
                [](const Token&, const cpp_int& a, const cpp_int& b) { return a & b; });
        }

        cpp_int bit_xor(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::bit_and, {"^"},
                [](const Token&, const cpp_int& a, const cpp_int& b) { return a ^ b; });
        }

        cpp_int bit_or(bool evaluate)
        {
            return left_associative(evaluate, &ExpressionParser::bit_xor, {"|"},
                [](const Token&, const cpp_int& a, const cpp_int& b) { return a | b; });
        }

        cpp_int logical_and(bool evaluate)
        {
            auto value = bit_or(evaluate);
            while(take(TokenKind::Operator, "&&"))
            {
                bool right_evaluated = evaluate and value != 0;
                auto right = bit_or(right_evaluated);
                if (evaluate) value = (value != 0 and right != 0);
            }
            return value;
        }

        cpp_int logical_or(bool evaluate)
        {
            auto value = logical_and(evaluate);
            while(take(TokenKind::Operator, "||"))
            {
                bool right_evaluated = evaluate and value == 0;
                auto right = logical_and(right_evaluated);
                if (evaluate) value = (value != 0 or right != 0);
            }
            return value;
        }

        cpp_int conditional(bool evaluate)
        {
            auto condition_value = logical_or(evaluate);
            if (not take(TokenKind::Question)) return condition_value;
            bool choose_true = evaluate and condition_value != 0;
            auto true_value = conditional(choose_true);
            if (not take(TokenKind::Colon))
                throw ParseFailure{"expected ':' in conditional expression", peek().column};
            auto false_value = conditional(evaluate and not choose_true);
            return not evaluate ? cpp_int(0) : choose_true ? true_value : false_value;
        }

    public:
        ExpressionParser(const vector<Token>& tokens_arg, const Options& options_arg)
            :tokens(tokens_arg), options(options_arg) {}

        // Parse through EOF; arithmetic is signed and unbounded except for the deterministic bit budget.
        cpp_int parse(bool evaluate = true)
        {
            auto value = conditional(evaluate);
            if (peek().kind != TokenKind::End)
                throw ParseFailure{"unexpected token '" + peek().text + "' in conditional expression", peek().column};
            return value;
        }
    };

    // Expand and evaluate one condition without depending on host integer width.
    bool evaluate_condition(string_view expression, std::size_t column,
                            const std::map<string, Definition>& definitions,
                            const Options& options)
    {
        auto tokens = tokenize(expression, column);
        Expander expander{definitions, options};
        auto expanded = expander.expand(tokens, {}, 0, {});
        auto resolved = resolve_defined(std::move(expanded), definitions);
        return ExpressionParser(resolved, options).parse() != 0;
    }

    yy::location source_location(const string& filename, unsigned line,
                                 std::size_t column, std::size_t width = 1)
    {
        yy::position begin(&filename, line, static_cast<int>(column));
        yy::position end(&filename, line, static_cast<int>(column + std::max<std::size_t>(1, width)));
        return {begin, end};
    }

    // Map byte boundaries to RE/flex source columns, including UTF-8 scalars and tab stops.
    vector<std::size_t> source_columns(string_view text)
    {
        vector<std::size_t> columns(text.size() + 1, 1);
        std::size_t column = 1;
        for(std::size_t i = 0; i < text.size();)
        {
            auto next = i + 1;
            char32_t code_point = static_cast<unsigned char>(text[i]);
            if (auto decoded = utf8::decode_next(text, i))
            {
                code_point = decoded->code_point;
                next = decoded->next_byte;
            }
            for(auto byte = i; byte < next; byte++) columns[byte] = column;
            if (code_point == U'\t') column += 8 - ((column - 1) % 8);
            else column++;
            i = next;
        }
        columns.back() = column;
        return columns;
    }

    // Advance the persistent Haskell shielding state across one ordinary physical line.
    void scan_haskell_line(Shield& shield, string_view line)
    {
        if (shield.state == ShieldState::LineComment or shield.state == ShieldState::String or
            shield.state == ShieldState::Character)
            shield.state = ShieldState::Normal;

        for(std::size_t i = 0; i < line.size();)
        {
            if (shield.block_comment_depth)
            {
                if (line.substr(i).starts_with("{-")) { shield.block_comment_depth++; i += 2; }
                else if (line.substr(i).starts_with("-}")) { shield.block_comment_depth--; i += 2; }
                else if (auto decoded = utf8::decode_next(line, i)) i = decoded->next_byte;
                else i++;
                continue;
            }
            if (shield.state == ShieldState::StringGap)
            {
                if (line[i] == '\\') { shield.state = ShieldState::String; i++; }
                else if (line[i] == ' ' or line[i] == '\t') i++;
                else shield.state = ShieldState::Normal;
                continue;
            }
            if (shield.state == ShieldState::String or shield.state == ShieldState::Character)
            {
                char closing = shield.state == ShieldState::String ? '"' : '\'';
                if (line[i] == closing) { shield.state = ShieldState::Normal; i++; }
                else if (line[i] == '\\')
                {
                    auto rest = line.substr(i + 1);
                    if (std::ranges::all_of(rest, [](char c) { return c == ' ' or c == '\t'; }))
                    {
                        shield.state = ShieldState::StringGap;
                        break;
                    }
                    i += std::min<std::size_t>(2, line.size() - i);
                }
                else if (auto decoded = utf8::decode_next(line, i)) i = decoded->next_byte;
                else i++;
                continue;
            }

            if (line.substr(i).starts_with("--")) { shield.state = ShieldState::LineComment; break; }
            if (line.substr(i).starts_with("{-")) { shield.block_comment_depth++; i += 2; continue; }
            if (line[i] == '"') { shield.state = ShieldState::String; i++; continue; }
            if (line[i] == '\'') { shield.state = ShieldState::Character; i++; continue; }
            if (auto decoded = utf8::decode_next(line, i)) i = decoded->next_byte;
            else i++;
        }

        if (shield.state == ShieldState::LineComment or shield.state == ShieldState::String or
            shield.state == ShieldState::Character)
            shield.state = ShieldState::Normal;
    }

    vector<PhysicalLine> physical_lines(const string& source)
    {
        vector<PhysicalLine> result;
        std::size_t start = 0;
        unsigned number = 1;
        while(start < source.size())
        {
            auto newline = source.find('\n', start);
            bool has_newline = newline != string::npos;
            auto end = has_newline ? newline : source.size();
            result.push_back({string_view(source).substr(start, end - start), has_newline, number++});
            start = has_newline ? end + 1 : source.size();
        }
        return result;
    }

    bool active(const vector<ConditionalFrame>& stack)
    {
        return stack.empty() or stack.back().current_active;
    }

    struct Processor
    {
        const string& source;
        const string& filename;
        const Options& options;
        std::map<string, Definition> definitions;
        vector<Message> messages;
        vector<ConditionalFrame> conditionals;
        Shield shield;
        string output;

        Processor(const string& source_arg, const string& filename_arg, const Options& options_arg)
            :source(source_arg), filename(filename_arg), options(options_arg) {}

        void report(MessageType type, unsigned line, std::size_t column, string text)
        {
            messages.emplace_back(type, source_location(filename, line, column), Notes{Note(std::move(text))});
        }

        void report_logical(MessageType type, const LogicalDirective& logical,
                            std::size_t logical_column, string text)
        {
            auto index = logical_column == 0 ? 0 : logical_column - 1;
            index = std::min(index, logical.origins.size() - 1);
            const auto& origin = logical.origins[index];
            report(type, origin.line, origin.column, std::move(text));
        }

        // Handle structural directives even in skipped branches so nesting remains recoverable.
        void directive(const LogicalDirective& logical_source, unsigned line, std::size_t hash_column)
        {
            string_view logical = logical_source.text;
            std::size_t pos = 0;
            skip_horizontal(logical, pos);
            auto name = parse_name(logical, pos);
            if (not name)
            {
                if (active(conditionals)) report(ErrorMsg, line, hash_column, "expected a directive name after '#'");
                return;
            }
            auto rest_column = pos + 1;
            auto rest = logical.substr(pos);
            auto trimmed_rest = trim_horizontal(rest);
            auto here = source_location(filename, line, hash_column, name->size() + 1);

            auto condition = [&](bool evaluate) -> bool
            {
                if (not evaluate) return false;
                try { return evaluate_condition(rest, rest_column, definitions, options); }
                catch(const ParseFailure& failure)
                {
                    report_logical(ErrorMsg, logical_source, failure.column, failure.message);
                    return false;
                }
            };

            if (*name == "if" or *name == "ifdef" or *name == "ifndef")
            {
                bool parent_active = active(conditionals);
                bool selected = false;
                if (*name == "if")
                    selected = condition(parent_active);
                else if (parent_active)
                {
                    std::size_t p = 0;
                    skip_horizontal(rest, p);
                    auto macro = parse_name(rest, p);
                    if (not macro or not only_directive_trivia(rest, p))
                        report_logical(ErrorMsg, logical_source, rest_column + p,
                                       "expected exactly one macro name after #" + *name);
                    else
                        selected = definitions.count(*macro);
                    if (*name == "ifndef") selected = not selected;
                }
                if (conditionals.size() >= options.maximum_conditional_depth)
                    report(ErrorMsg, line, hash_column, "CPP conditional nesting exceeds the depth limit");
                conditionals.push_back({parent_active, parent_active and selected,
                                        parent_active and selected, false, here});
                return;
            }

            if (*name == "elif")
            {
                if (conditionals.empty())
                {
                    report(ErrorMsg, line, hash_column, "#elif without a matching #if");
                    return;
                }
                auto& frame = conditionals.back();
                if (frame.saw_else)
                {
                    report(ErrorMsg, line, hash_column, "#elif after #else");
                    frame.current_active = false;
                    return;
                }
                bool selected = condition(frame.parent_active and not frame.branch_taken);
                frame.current_active = frame.parent_active and not frame.branch_taken and selected;
                frame.branch_taken = frame.branch_taken or frame.current_active;
                return;
            }
            if (*name == "else")
            {
                if (not only_directive_trivia(rest, 0) and
                    (conditionals.empty() or conditionals.back().parent_active))
                    report_logical(ErrorMsg, logical_source, rest_column, "unexpected text after #else");
                if (conditionals.empty())
                {
                    report(ErrorMsg, line, hash_column, "#else without a matching #if");
                    return;
                }
                auto& frame = conditionals.back();
                if (frame.saw_else)
                {
                    report(ErrorMsg, line, hash_column, "duplicate #else");
                    return;
                }
                frame.saw_else = true;
                frame.current_active = frame.parent_active and not frame.branch_taken;
                frame.branch_taken = frame.branch_taken or frame.current_active;
                return;
            }
            if (*name == "endif")
            {
                if (not only_directive_trivia(rest, 0) and
                    (conditionals.empty() or conditionals.back().parent_active))
                    report_logical(ErrorMsg, logical_source, rest_column, "unexpected text after #endif");
                if (conditionals.empty())
                {
                    report(ErrorMsg, line, hash_column, "#endif without a matching #if");
                    return;
                }
                conditionals.pop_back();
                return;
            }

            if (not active(conditionals)) return;
            if (*name == "define")
            {
                string diagnostic;
                if (auto definition = parse_source_definition(rest, diagnostic))
                    definitions[definition->name] = std::move(*definition);
                else
                    report_logical(ErrorMsg, logical_source, rest_column, diagnostic);
            }
            else if (*name == "undef")
            {
                std::size_t p = 0;
                skip_horizontal(rest, p);
                auto macro = parse_name(rest, p);
                if (macro and *macro != "defined" and only_directive_trivia(rest, p))
                    definitions.erase(*macro);
                else
                    report_logical(ErrorMsg, logical_source, rest_column,
                                   "expected exactly one macro name after #undef");
            }
            else if (*name == "error")
                report(ErrorMsg, line, hash_column, trimmed_rest.empty() ? "#error" : string(trimmed_rest));
            else if (*name == "warning")
                report(WarningMsg, line, hash_column, trimmed_rest.empty() ? "#warning" : string(trimmed_rest));
            else if (*name == "include" or *name == "line" or *name == "pragma")
                report(ErrorMsg, line, hash_column, "#" + *name + " is not supported by conditional CPP");
            else
                report(ErrorMsg, line, hash_column, "unknown CPP directive '#" + *name + "'");
        }

        Result run()
        {
            for(const auto& definition: options.definitions) definitions[definition.name] = definition;
            for(const auto& name: options.undefinitions) definitions.erase(name);

            auto lines = physical_lines(source);
            for(std::size_t index = 0; index < lines.size(); index++)
            {
                const auto& line = lines[index];
                std::size_t first = 0;
                while(first < line.text.size() and (line.text[first] == ' ' or line.text[first] == '\t')) first++;
                bool is_directive = shield.block_comment_depth == 0 and shield.state == ShieldState::Normal and
                                    first < line.text.size() and line.text[first] == '#';
                if (not is_directive)
                {
                    if (active(conditionals)) output.append(line.text);
                    if (line.has_newline) output += '\n';
                    scan_haskell_line(shield, line.text);
                    continue;
                }

                LogicalDirective logical;
                std::size_t consumed = 0;
                for(;;)
                {
                    const auto& part = lines[index + consumed];
                    auto columns = source_columns(part.text);
                    auto begin = consumed == 0 ? first + 1 : 0;
                    auto end = part.text.size();
                    bool continuation = part.has_newline and end > begin and part.text[end - 1] == '\\';
                    if (continuation) end--;
                    logical.text.append(part.text.substr(begin, end - begin));
                    for(auto byte = begin; byte < end; byte++)
                        logical.origins.push_back({part.number, columns[byte]});
                    consumed++;
                    if (not continuation or index + consumed >= lines.size()) break;
                }
                const auto& final_part = lines[index + consumed - 1];
                auto final_columns = source_columns(final_part.text);
                auto final_end = final_part.text.size();
                if (final_part.has_newline and final_end and final_part.text.back() == '\\') final_end--;
                logical.origins.push_back({final_part.number, final_columns[final_end]});
                directive(logical, line.number, source_columns(line.text)[first]);
                for(std::size_t n = 0; n < consumed; n++)
                    if (lines[index + n].has_newline) output += '\n';
                index += consumed - 1;
            }

            if (not conditionals.empty())
            {
                Message message{ErrorMsg, conditionals.back().opening_location,
                                {Note("unterminated CPP conditional")}};
                for(std::size_t i = 0; i + 1 < conditionals.size(); i++)
                {
                    const auto& loc = conditionals[i].opening_location;
                    message.notes.push_back(Note("outer conditional opened at line " +
                                                 std::to_string(loc.begin.line)));
                }
                messages.push_back(std::move(message));
            }
            return {std::move(output), std::move(messages)};
        }
    };
}

std::optional<Definition> parse_initial_definition(string_view text, string& diagnostic)
{
    auto equals = text.find('=');
    auto head = trim_horizontal(text.substr(0, equals));
    auto replacement = equals == string_view::npos ? string_view("1") : text.substr(equals + 1);
    return parse_definition_parts(head, replacement, false, diagnostic);
}

bool parse_initial_undefinition(string_view text, string& name, string& diagnostic)
{
    text = trim_horizontal(text);
    std::size_t pos = 0;
    auto parsed = parse_name(text, pos);
    skip_horizontal(text, pos);
    if (not parsed or pos != text.size())
    {
        diagnostic = "expected exactly one macro name matching [A-Za-z_][A-Za-z0-9_]*";
        return false;
    }
    if (*parsed == "defined")
    {
        diagnostic = "the conditional operator 'defined' cannot be used as a macro name";
        return false;
    }
    name = std::move(*parsed);
    return true;
}

Result conditionals(const string& source, const string& display_name, const Options& options)
{
    return Processor{source, display_name, options}.run();
}
}
