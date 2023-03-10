#ifndef ZZ_DRIVER_HH
# define ZZ_DRIVER_HH
# include <string>
# include <map>
# include <set>
# include "parser.hh"
# include "computation/message.H"
# include "util/ptree.H"

// Tell Flex the lexer's prototype ...
# define YY_DECL \
  zz::parser::symbol_type zzlex (zz_driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

// Conducting the whole scanning and parsing of Calc++.
class zz_driver
{
    typedef zz::parser::location_type location_type;
    typedef zz::parser::symbol_type symbol_type;
    typedef zz::parser::token_type token_type;

    std::vector<Message> messages;

    std::map<std::string,std::pair<zz::parser::token_type,int>> reserved_words;

public:
    zz_driver ();

    zz::parser::symbol_type varid(const zz::parser::location_type& loc) const;

    void push_error_message(const location_type& loc, const std::string& err);
    void pop_error_message();

    // Store the result
    ptree result;
    // Run the parser on file F.  Return 0 on success.
    int parse_file (const std::string& filename);
    int parse_string (const std::string& content, const std::string& input_name);
    // The name of the file being parsed.
    std::string file;
    // Whether to generate parser debug traces.
    bool trace_parsing = false;

    // Handling the scanner.
    void scan_begin (const std::string& content);
    void scan_end ();
    // Whether to generate scanner debug traces.
    bool trace_scanning = false;
    // The token's location used by the scanner.
    yy::location location;

    int prec_close_count = 0;
    void step_closing_token() {if (prec_close_count > 0) prec_close_count--;}
    void set_closing_token() {prec_close_count = 2;}
    bool check_closing_token() {return prec_close_count > 0;}
};

ptree parse(const std::string&);
#endif // ! DRIVER_HH

