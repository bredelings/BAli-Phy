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

template <typename Iterator>
bool parse_numbers(Iterator first, Iterator last, vector<double>& v)
{
    using qi::double_;
    using qi::phrase_parse;
    using qi::_1;
    using ascii::space;
    using phoenix::push_back;

    bool r = phrase_parse(first,last,
			  (
			   double_[push_back(phoenix::ref(v), _1)]
			   >> *(',' >> double_[push_back(phoenix::ref(v), _1)])
			   ),

        space
    );

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
    vector<double> v;
    if (parse_numbers(line.begin(), line.end(), v))
      std::cerr<<"Parsing succeeded: "<<join(v,",")<<"\n";
    else
      std::cerr<<"Parsing failed!\n";

    

    // Here, we want to convert the stream of tokens to an expression ref of the form (distributed,x,(D,args)) where
    //  D is of the form (prob_density,name,density,quantile)
    // The line should look like "x ~ name(args).
    // - x should be a parameter or a tuple of parameters.
    // - args should be empty, or a comma-separated list of haskell expressions.
  }
}

