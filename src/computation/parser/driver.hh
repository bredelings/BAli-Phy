#ifndef DRIVER_HH
# define DRIVER_HH
# include <string>
# include <string_view>
# include <deque>
# include <map>
# include <memory>
# include <optional>
# include <set>
# include <utility>
# include "parser.hh"
# include "computation/haskell/extensions.H"
# include "computation/message.H"

class driver;
class RawLexerState;

struct RawLexerStateDeleter
{
    void operator()(RawLexerState* state) const;
};

enum class LayoutIntent
{
    None,
    Layout,
    LayoutDo,
    LayoutIf
};

struct TokenEffects
{
    bool closes_atom = false;
    bool push_no_layout_context = false;
    bool pop_context = false;
    LayoutIntent layout_after = LayoutIntent::None;
};

struct LexedToken
{
    yy::parser::symbol_type symbol;
    TokenEffects effects;
    bool starts_line = false;

    LexedToken(const yy::parser::symbol_type& symbol_arg, TokenEffects effects_arg = {})
        : symbol(symbol_arg),
          effects(effects_arg)
    {}

    LexedToken(yy::parser::symbol_type&& symbol_arg, TokenEffects effects_arg = {})
        : symbol(std::move(symbol_arg)),
          effects(effects_arg)
    {}
};

yy::parser::symbol_type yylex(driver& drv);

LexedToken raw_yylex(driver& drv);

struct LayoutContext
{
    int offset;
    bool gen_semis;
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

    bool next_real_token_starts_line = false;
    LayoutIntent pending_layout_intent = LayoutIntent::None;
    std::deque<symbol_type> pending_virtual_tokens;
    std::optional<LexedToken> pending_real_token;
    bool previous_committed_token_closes_atom = false;
    yy::position previous_committed_token_end;
    std::unique_ptr<RawLexerState, RawLexerStateDeleter> raw_lexer;

    std::optional<symbol_type> virtual_after_keyword(const location_type& loc);
    void virtual_after_if(const location_type& loc);
    std::optional<symbol_type> virtual_at_bol(const location_type& loc);
    void commit_token(const LexedToken& token);
    bool take_next_real_token_starts_line();
    std::optional<symbol_type> take_pending_virtual_token();
    std::optional<symbol_type> next_virtual_token();
    LexedToken take_pending_real_token();

public:
    driver (const LanguageExtensions& lang_exts);
    ~driver();

    friend LexedToken raw_yylex(driver& drv);

    LayoutContext get_offside(const location_type& loc);

    void pop_context();
    std::optional<LayoutContext> get_context();
    void push_context(const std::optional<LayoutContext>&);
    void push_context(const LayoutContext&);
    void push_module_context();
    void push_context();

    ClassifiedVarId classify_varid(std::string_view text) const;
    ClassifiedVarSym classify_varsym(std::string_view text, SymbolOccurrence occurrence) const;
    bool has_extension(LangExt ext) const { return lang_exts.has_extension(ext); }
    void mark_next_real_token_starts_line() {next_real_token_starts_line = true;}
    yy::parser::symbol_type consym(std::string_view text, const yy::parser::location_type& loc) const;
    std::optional<yy::parser::symbol_type> prag(std::string_view text, const yy::parser::location_type& loc);

    void push_warning_message(const location_type& loc, const Note& w);
    void push_error_message(const location_type& loc, const Note& e);
    void pop_error_message();

    yy::parser::symbol_type next_parser_token();

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

    bool left_adjacent_closes_atom(const location_type& loc) const;
};

Haskell::Module parse_module_file(const std::string& content, const std::string& input_name, const LanguageExtensions& lang_exts);
#endif // ! DRIVER_HH
