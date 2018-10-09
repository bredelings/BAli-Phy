#ifndef DRIVER_HH
# define DRIVER_HH
# include <string>
# include <map>
# include "parser.hh"
# include <boost/optional.hpp>

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

    std::vector<std::pair<location_type,std::string>> error_messages;
    std::vector<boost::optional<LayoutContext>> contexts;

public:
    driver ();

    LayoutContext get_offside(const location_type& loc);

    void pop_context();
    boost::optional<LayoutContext> get_context();
    void push_context(const boost::optional<LayoutContext>&);
    void push_context(const LayoutContext&);
    void push_module_context();
    void push_context();

    symbol_type hopefully_open_brace(const location_type& loc);
    boost::optional<symbol_type> do_bol(const location_type& loc);
    symbol_type new_layout_context(const location_type& loc, bool strict, bool gen_semis, token_type tok);
    symbol_type do_layout_left(const location_type& loc);
    void pop() {}

    void push_error_message(const std::pair<location_type,std::string>&);
    void pop_error_message();

    // Store the result
    expression_ref result;
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
};

expression_ref parse_module_file(const std::string& content, const std::string& input_name);
#endif // ! DRIVER_HH

