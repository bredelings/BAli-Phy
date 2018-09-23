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
    std::vector<boost::optional<LayoutContext>> contexts;

public:
    driver ();

    void pop_context();
    boost::optional<LayoutContext> get_context();
    void push_context(const boost::optional<LayoutContext>&);
    void push_context(const LayoutContext&);
    void push_context();

    void hopefully_open_brace(const yy::parser::location_type& loc) {};
    void do_bol(const yy::parser::location_type& loc);
    void new_layout_context(const yy::parser::location_type& loc, bool);
    yy::parser::symbol_type do_layout_left(const yy::parser::location_type& loc);
    void pop() {}

    // Run the parser on file F.  Return 0 on success.
    int parse (const std::string& f);
    // The name of the file being parsed.
    std::string file;
    // Whether to generate parser debug traces.
    bool trace_parsing;


    // Handling the scanner.
    void scan_begin ();
    void scan_end ();
    // Whether to generate scanner debug traces.
    bool trace_scanning;
    // The token's location used by the scanner.
    yy::location location;
};
#endif // ! DRIVER_HH

