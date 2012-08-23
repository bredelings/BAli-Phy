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

struct employee
{
    int age;
    std::string surname;
    std::string forename;
    double salary;
};

BOOST_FUSION_ADAPT_STRUCT(
    employee,
    (int, age)
    (std::string, surname)
    (std::string, forename)
    (double, salary)
)

template <typename Iterator>
struct employee_parser : qi::grammar<Iterator, employee(), ascii::space_type>
{
    employee_parser() : employee_parser::base_type(start)
    {
        using qi::int_;
        using qi::lit;
        using qi::double_;
        using qi::lexeme;
        using ascii::char_;

        quoted_string %= lexeme['"' >> +(char_ - '"') >> '"'];

        start %=
            lit("employee")
            >> '{'
            >>  int_ >> ','
            >>  quoted_string >> ','
            >>  quoted_string >> ','
            >>  double_
            >>  '}'
            ;
    }

    qi::rule<Iterator, std::string(), ascii::space_type> quoted_string;
    qi::rule<Iterator, employee(), ascii::space_type> start;
};

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
	//	arguments %= eps | lit('(')>>h_expression%','>>lit(')');
	arguments %= lit('(')>>h_expression%','>>lit(')');
	bugs_line %= text > '~' > text > arguments >> eoi ;
	//	bugs_line = text[at_c<0>(_val) = _1] >> '~' >> text[at_c<1>(_val) = _1];

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

    }

  qi::rule<Iterator, bugs_cmd(), ascii::space_type> bugs_line;
  qi::rule<Iterator, std::string(), ascii::space_type> text;
  qi::rule<Iterator, expression_ref(), ascii::space_type> h_expression;
  qi::rule<Iterator, vector<expression_ref>(), ascii::space_type> arguments;


  /*
    qi::rule<Iterator, mini_xml(), ascii::space_type> xml;
    qi::rule<Iterator, mini_xml_node(), ascii::space_type> node;
    qi::rule<Iterator, std::string(), ascii::space_type> start_tag;
    qi::rule<Iterator, void(std::string), ascii::space_type> end_tag;
  */
};

//-----------------------------------------------------------------------//

struct mini_xml;

typedef
    boost::variant<
        boost::recursive_wrapper<mini_xml>
      , std::string
    >
mini_xml_node;

struct mini_xml
{
    std::string name;                           // tag name
    std::vector<mini_xml_node> children;        // children
};

BOOST_FUSION_ADAPT_STRUCT(
			  mini_xml,
    (std::string, name)
    (std::vector<mini_xml_node>, children)
)

template <typename Iterator>
struct mini_xml_grammar : qi::grammar<Iterator, mini_xml(), ascii::space_type>
{
    mini_xml_grammar() : mini_xml_grammar::base_type(xml)
    {
        using qi::lit;
        using qi::lexeme;
        using ascii::char_;
        using ascii::string;
        using namespace qi::labels;

        using phoenix::at_c;
        using phoenix::push_back;

        text = lexeme[+(char_ - '<')        [_val += _1]];
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
    }

    qi::rule<Iterator, mini_xml(), ascii::space_type> xml;
    qi::rule<Iterator, mini_xml_node(), ascii::space_type> node;
    qi::rule<Iterator, std::string(), ascii::space_type> text;
    qi::rule<Iterator, std::string(), ascii::space_type> start_tag;
    qi::rule<Iterator, void(std::string), ascii::space_type> end_tag;
};


///////////////////////////////////////////////////////////////////////////////
//  roman (numerals) grammar
//
//      Note the use of the || operator. The expression
//      a || b reads match a or b and in sequence. Try
//      defining the roman numerals grammar in YACC or
//      PCCTS. Spirit rules! :-)
///////////////////////////////////////////////////////////////////////////////
//[tutorial_roman_grammar
template <typename Iterator>
struct summer : qi::grammar<Iterator, double()>
{
  summer() : summer::base_type(start)
  {
    using qi::eps;
    using qi::double_;
    using qi::lit;
    using qi::_val;
    using qi::_1;
    using ascii::char_;

    start = eps[_val = 0.0] >> double_ [ _val += _1 ] % ',';
  }

  qi::rule<Iterator, double()> start;
};


template <typename Iterator>
bool parse_numbers(Iterator first, Iterator last, vector<double>& v)
{
    using qi::double_;
    using qi::phrase_parse;
    using qi::_1;
    using ascii::space;
    using phoenix::push_back;

    bool r = phrase_parse(first,last,
			  // Begin grammar
			  (
			   double_ % ','
			   ),
			  // End grammar

			  space, v);


    if (first != last) // fail if we did not get a full match
        return false;

    return r;
}

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
    vector<string> tokens = tokenize(line);
    std::cerr<<join(tokens," : ")<<"\n";
    double sum;
    summer<string::const_iterator> sum_parser;
    if (qi::parse(line.begin(), line.end(), sum_parser, sum))
      std::cerr<<"Parsing succeeded: sum = "<<sum<<"\n";

    employee_parser<string::const_iterator> g;
    employee E;
    string::const_iterator iter = line.begin();
    using boost::spirit::ascii::space;
    if (phrase_parse(iter, line.end(), g, space, E) and iter == line.end())
    {
      std::cerr<<"Employee phrase parse: "<<boost::fusion::as_vector(E)<<std::endl;
    }

    iter = line.begin();
    mini_xml_grammar<string::const_iterator> xml_parser;
    mini_xml ast;
    if (phrase_parse(iter, line.end(), xml_parser, space, ast) and iter == line.end())
    {
      std::cerr<<"XML phrase parse: "<<std::endl; //boost::fusion::as_vector(ast)<<std::endl;
    }
   
    
    iter = line.begin();
    bugs_grammar<string::const_iterator> bugs_parser;
    bugs_cmd cmd;
    if (phrase_parse(iter, line.end(), bugs_parser, space, cmd) and iter == line.end())
    {
      std::cerr<<"BUGS phrase parse: "<<boost::fusion::as_vector(cmd)<<std::endl;
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

