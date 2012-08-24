#include "BUGS.H"

#include <vector>
#include "io.H"
#include "util.H"

using std::vector;
using std::string;
using std::endl;

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/include/io.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/foreach.hpp>

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace phoenix = boost::phoenix;

//-----------------------------------------------------------------------//

// Make a more interpretable program structure:
//   It should contain an UNTRANSLATED and unsimplified representation of the program source.
//     It should contain (for example) readable function bodies.
//        ... perhaps a notes collection?
//   It should contain a list of identifiers and parameters.

// A symbol table for parameters and vars.
qi::symbols<char,expression_ref> identifiers;

struct bugs_cmd
{
  string var;
  string dist;
  vector<expression_ref> arguments;
};

BOOST_FUSION_ADAPT_STRUCT(
    bugs_cmd,
    (std::string, var)
    (std::string, dist)
    (std::vector<expression_ref>, arguments)
)

template <typename Iterator>
struct bugs_grammar : qi::grammar<Iterator, bugs_cmd(), ascii::space_type>
{
    bugs_grammar() : bugs_grammar::base_type(bugs_line)
    {
        using qi::lit;
        using qi::lexeme;
	using qi::on_error;
	using qi::fail;
        using ascii::char_;
        using qi::double_;
	using qi::eps;
	using qi::eoi;
        using ascii::string;
        using namespace qi::labels;

        using phoenix::at_c;
        using phoenix::push_back;
	using phoenix::construct;
	using phoenix::val;

        text %= lexeme[+(char_ - ' ' -'(')];
	h_expression %= double_;
	arguments %= lit('(')>>h_expression%','>>lit(')')|lit("()");
	bugs_line %= text > '~' > text > arguments >> eoi ;

	small %= char_("a-z");
	large %= char_("A-Z");
	digit %= char_("0-9");
	symbol %= lit('!') | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '-' | '~' | ':';
	special %= lit('(') | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}';
	graphic %= small | large | symbol | digit | special | '"' | '\'';

	dashes %= lit("--")>>*lit("-");

	varid %= (small>>(+(small|large|digit|"'"))) - reservedid;
	conid %= large>>(+(small|large|digit|"'"));
	reservedid %= lit("case") | "class" | "data" | "default" | "deriving" | "do" | "else" |	"foreign" | "if" | "import" | "in" | "infix" | "infixl" | 	"infixr" | "instance" | "let" | "module" | "newtype" | "of" | 	"then" | "type" | "where" | "_";

	varsym %= ((symbol-lit(':'))>>*symbol)-reservedop-dashes;
	consym %= (lit(':')>>*symbol)-reservedop;
	reservedop %= lit("..") | ":" | "::" | "=" | "\\" | "|" | "<-" | "->" | "@" | "~" | "=>";

	tyvar %= varid;
	tycon %= conid;
	tycls %= conid;
	modid %= *(conid >> ".") >> conid;

	qvarid %= -(modid>>".") >> varid;
	qconid %= -(modid>>".") >> conid;
	qtycon %= -(modid>>".") >> tycon;
	qtycls %= -(modid>>".") >> tycls;
	qvarsym %= -(modid>>".") >> qvarsym;
	qconsym %= -(modid>>".") >> qconsym;

	decimal %= +digit;
	h_integer %= decimal;

	h_float %= decimal >> "." >> decimal >> -exponent | decimal >> exponent;
	exponent %= ( lit('\'') | "E" ) >> -(lit("+")|"-") >> decimal;

	h_char %= lit('\'') >> ((graphic - '\'' - '\\')|char_(' ')|escape) >> lit('\'');
	h_string %= lit('\"') >> *((graphic - '\'' - '\\')|char_(' ')|escape) >> lit('\"');
	escape = lit("\\a") [_val = '\a'] |
	  lit("\\b") [_val = '\b'] |
	  lit("\\f") [_val = '\f'] |
	  lit("\\n") [_val = '\n'] |
	  lit("\\r") [_val = '\r'] |
	  lit("\\t") [_val = '\t'] |
	  lit("\\v") [_val = '\v'] |
	  lit("\\\\") [_val = '\\'] |
	  lit("\\\"") [_val = '"'] |
	  lit("'") [_val = '\''];

	on_error<fail>
	  (
	   bugs_line
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	// Add some error messages to see what's failing!
	on_error<fail>
	  (
	   arguments
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   text
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   h_expression
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );



	// Add some error messages to see what's failing!

	/*
        node = (xml | text)                 [_val = _1];

        start_tag =
                '<'
            >>  !lit('/')
            >>  lexeme[+(char_ - '>')       [_val += _1]]
            >>  '>'
        ;

        end_tag =
                "</"
            >>  string(_r1)
            >>  '>'
        ;

        xml =
                start_tag                   [at_c<0>(_val) = _1]
            >>  *node                       [push_back(at_c<1>(_val), _1)]
            >>  end_tag(at_c<0>(_val))
        ;
	*/
	bugs_line.name("bugs_line");
	text.name("text");
	h_expression.name("h_expression");
	arguments.name("arguments");
	reservedid.name("reserved_id");

    }

  qi::rule<Iterator, bugs_cmd(), ascii::space_type> bugs_line;
  qi::rule<Iterator, std::string(), ascii::space_type> text;
  qi::rule<Iterator, expression_ref(), ascii::space_type> h_expression;
  qi::rule<Iterator, vector<expression_ref>(), ascii::space_type> arguments;

  qi::rule<Iterator, char(), ascii::space_type> small;
  qi::rule<Iterator, char(), ascii::space_type> large;
  qi::rule<Iterator, char(), ascii::space_type> digit;
  qi::rule<Iterator, char(), ascii::space_type> symbol;
  qi::rule<Iterator, char(), ascii::space_type> special;
  qi::rule<Iterator, char(), ascii::space_type> graphic;

  qi::rule<Iterator, std::string(), ascii::space_type> dashes;

  qi::rule<Iterator, std::string(), ascii::space_type> varid;
  qi::rule<Iterator, std::string(), ascii::space_type> conid;
  qi::rule<Iterator, std::string(), ascii::space_type> reservedid;

  qi::rule<Iterator, std::string(), ascii::space_type> varsym;
  qi::rule<Iterator, std::string(), ascii::space_type> consym;
  qi::rule<Iterator, std::string(), ascii::space_type> reservedop; // reserved operator

  qi::rule<Iterator, std::string(), ascii::space_type> tyvar;
  qi::rule<Iterator, std::string(), ascii::space_type> tycon;
  qi::rule<Iterator, std::string(), ascii::space_type> tycls;
  qi::rule<Iterator, std::string(), ascii::space_type> modid; // module id

  qi::rule<Iterator, std::string(), ascii::space_type> qvarid; // qualified variable id
  qi::rule<Iterator, std::string(), ascii::space_type> qconid; // qualified constructor id
  qi::rule<Iterator, std::string(), ascii::space_type> qtycon; // qualified type constructor
  qi::rule<Iterator, std::string(), ascii::space_type> qtycls; // qualified type class
  qi::rule<Iterator, std::string(), ascii::space_type> qvarsym;
  qi::rule<Iterator, std::string(), ascii::space_type> qconsym;

  qi::rule<Iterator, std::string(), ascii::space_type> decimal;
  qi::rule<Iterator, std::string(), ascii::space_type> h_integer;
  qi::rule<Iterator, std::string(), ascii::space_type> h_float;
  qi::rule<Iterator, std::string(), ascii::space_type> exponent;

  qi::rule<Iterator, char(), ascii::space_type> h_char;
  qi::rule<Iterator, char(), ascii::space_type> escape;
  qi::rule<Iterator, std::string(), ascii::space_type> h_string;
  qi::rule<Iterator, std::string(), ascii::space_type> charesc;
};

//-----------------------------------------------------------------------//

vector<string> tokenize(const string& line)
{
  const string delimiters = "!#$%&*~|^@.?()[]{}/\\,;:=*`'\"+-<>";
  const string whitespace = " \t\n\r";

  vector<string> tokens;

  int i=0;
  string token;

  while(get_word(token,i,line,delimiters,whitespace))
    tokens.push_back(token);

  return tokens;
}

void add_BUGS(const Parameters& P, const string& filename)
{
  // Um, so what is the current program?
  // 1. Well, its got a collection of identifiers.
  //   (a) Some of these are functions
  //   (b) Some of these are parameters
  // 2. We've got a collection of heads.

  checked_ifstream file(filename,"BUGS file");
  vector<string> lines;

  {
    string line;
    while(getline(file,line))
      lines.push_back(line);
  }

  std::cerr<<"Read "<<lines.size()<<" lines from Hierarchical Model Description file '"<<filename<<"'\n";

  for(const auto& line: lines)
  {
    using boost::spirit::ascii::space;

    string::const_iterator iter = line.begin();
    bugs_grammar<string::const_iterator> bugs_parser;
    bugs_cmd cmd;
    if (phrase_parse(iter, line.end(), bugs_parser, space, cmd) and iter == line.end())
    {
      std::cerr<<"BUGS phrase parse: "<<cmd.var<<" ~ "<<cmd.dist<<"(";
      for(int i=0;i<cmd.arguments.size();i++)
      {
	std::cerr<<cmd.arguments[i];
	if (i != cmd.arguments.size()-1)
	  std::cerr<<",";
      }
      std::cerr<<")\n";
    }
    else
      std::cerr<<"BUGS pharse parse: only parsed "<<line.substr(0, iter-line.begin())<<endl;
    

    // Here, we want to convert the stream of tokens to an expression ref of the form (distributed,x,(D,args)) where
    //  D is of the form (prob_density,name,density,quantile)
    // The line should look like "x ~ name(args).
    // - x should be a parameter or a tuple of parameters.
    // - args should be empty, or a comma-separated list of haskell expressions.
  }
  exit(0);
}

