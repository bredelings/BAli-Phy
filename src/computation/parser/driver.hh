#ifndef DRIVER_HH
# define DRIVER_HH
# include <string>
# include <string_view>
# include <map>
# include <optional>
# include <set>
# include <utility>
# include "parser.hh"
# include "computation/haskell/extensions.H"
# include "computation/message.H"

class driver;

struct TokenEffects
{
    bool closes_atom = false;
};

struct LexedToken
{
    yy::parser::symbol_type symbol;
    TokenEffects effects;

    LexedToken(const yy::parser::symbol_type& symbol_arg)
        : symbol(symbol_arg)
    {}

    LexedToken(yy::parser::symbol_type&& symbol_arg)
        : symbol(std::move(symbol_arg))
    {}

    LexedToken(const yy::parser::symbol_type& symbol_arg, TokenEffects effects_arg)
        : symbol(symbol_arg),
          effects(effects_arg)
    {}

    LexedToken(yy::parser::symbol_type&& symbol_arg, TokenEffects effects_arg)
        : symbol(std::move(symbol_arg)),
          effects(effects_arg)
    {}
};

yy::parser::symbol_type yylex(driver& drv);

// Tell Flex the raw lexer's prototype ...
# define YY_DECL \
  LexedToken raw_yylex (driver& drv)
// ... and declare it for the wrapper's sake.
YY_DECL;

struct LayoutContext
{
    int offset;
    bool gen_semis;
};

enum class LayoutIntent
{
    None,
    Layout,
    LayoutDo,
    LayoutIf
};

struct ClassifiedVarId
{
    std::string text;
    std::optional<yy::parser::token_type> token;
    LayoutIntent layout_after = LayoutIntent::None;
};

enum class SymbolOccurrence
{
    LooseInfix,
    Prefix,
    Suffix,
    TightInfix
};

struct ClassifiedVarSym
{
    std::string text;
    std::optional<yy::parser::token_type> token;
};

// Conducting the whole scanning and parsing of Calc++.
class driver
{
    typedef yy::parser::location_type location_type;
    typedef yy::parser::symbol_type symbol_type;
    typedef yy::parser::token_type token_type;

    std::vector<Message> messages;
    std::vector<std::optional<LayoutContext>> contexts;

    LanguageExtensions lang_exts;

    std::map<std::string,yy::parser::token_type> reserved_words;
    std::map<std::string,yy::parser::token_type> tight_infix_reserved_symbols;
    std::map<std::string,yy::parser::token_type> prefix_reserved_symbols;
    std::map<std::string,yy::parser::token_type> reserved_symbols;

public:
    driver (const LanguageExtensions& lang_exts);

    LayoutContext get_offside(const location_type& loc);

    void pop_context();
    std::optional<LayoutContext> get_context();
    void push_context(const std::optional<LayoutContext>&);
    void push_context(const LayoutContext&);
    void push_module_context();
    void push_context();

    symbol_type hopefully_open_brace(const location_type& loc);
    std::optional<symbol_type> do_bol(const location_type& loc);
    symbol_type new_layout_context(const location_type& loc, bool gen_semis, token_type tok);
    symbol_type do_layout_left(const location_type& loc);
    void pop() {}
    ClassifiedVarId classify_varid(std::string_view text) const;
    ClassifiedVarSym classify_varsym(std::string_view text, SymbolOccurrence occurrence) const;
    void commit_token(const LexedToken& token);
    yy::parser::symbol_type consym(std::string_view text, const yy::parser::location_type& loc) const;
    std::optional<yy::parser::symbol_type> prag(std::string_view text, const yy::parser::location_type& loc);

    void push_warning_message(const location_type& loc, const Note& w);
    void push_error_message(const location_type& loc, const Note& e);
    void pop_error_message();

    // Store the result
    Haskell::Module result;
    // Run the parser on file F.  Return 0 on success.
    int parse_file (const std::string& filename);
    int parse_string (const std::string& content, const std::string& input_name);
    // The name of the file being parsed.
    std::string file;
    // Whether to generate parser debug traces.
    bool trace_parsing;

    // Handling the scanner.
    void scan_begin (const std::string& content);
    void scan_end ();
    // Whether to generate scanner debug traces.
    bool trace_scanning;
    // The token's location used by the scanner.
    yy::location location;

    static constexpr int left_adjacency_window_size = 2;

    // YY_USER_ACTION runs before each lexer action, including whitespace/comment
    // actions. This window is visible only to an immediately adjacent token.
    int left_adjacency_window = 0;
    void advance_left_adjacency_window() {if (left_adjacency_window > 0) left_adjacency_window--;}
    void mark_token_closes_atom() {left_adjacency_window = left_adjacency_window_size;}
    bool left_adjacent_closes_atom() const {return left_adjacency_window > 0;}
};

Haskell::Module parse_module_file(const std::string& content, const std::string& input_name, const LanguageExtensions& lang_exts);
#endif // ! DRIVER_HH
