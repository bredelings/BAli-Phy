#include "BUGS.H"

#include <vector>
#include "io.H"
#include "util.H"

using std::vector;
using std::string;

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

    

    // Here, we want to convert the stream of tokens to an expression ref of the form (distributed,x,(D,args)) where
    //  D is of the form (prob_density,name,density,quantile)
    // The line should look like "x ~ name(args).
    // - x should be a parameter or a tuple of parameters.
    // - args should be empty, or a comma-separated list of haskell expressions.
  }
}

