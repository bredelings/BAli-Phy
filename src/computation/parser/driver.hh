#ifndef DRIVER_HH
# define DRIVER_HH
# include <string>
# include <map>
# include <set>
# include "parser.hh"
# include "computation/haskell/extensions.H"
# include "computation/message.H"

// Tell Flex the lexer's prototype ...
# define YY_DECL \
  yy::parser::symbol_type yylex (driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

struct LayoutContext
{
    int offset;
    bool gen_semis;
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
    symbol_type new_layout_context(const location_type& loc, bool strict, bool gen_semis, token_type tok);
    symbol_type do_layout_left(const location_type& loc);
    void pop() {}

    void push_error_message(const location_type& loc, const std::string& err);
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

    int prec_close_count = 0;
    void step_closing_token() {if (prec_close_count > 0) prec_close_count--;}
    void set_closing_token() {prec_close_count = 2;}
    bool check_closing_token() {return prec_close_count > 0;}
};

Haskell::Module parse_module_file(const std::string& content, const std::string& input_name, const LanguageExtensions& lang_exts);
#endif // ! DRIVER_HH

