#include "BUGS.H"

#include <vector>
#include "io.H"
#include "util.H"

using std::vector;
using std::string;

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace phoenix = boost::phoenix;

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
    else
      std::cerr<<"Parsing failed!\n";

    

    // Here, we want to convert the stream of tokens to an expression ref of the form (distributed,x,(D,args)) where
    //  D is of the form (prob_density,name,density,quantile)
    // The line should look like "x ~ name(args).
    // - x should be a parameter or a tuple of parameters.
    // - args should be empty, or a comma-separated list of haskell expressions.
  }
  exit(0);
}

