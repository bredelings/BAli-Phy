#ifndef BUGS_H
#define BUGS_H
#include <string>
#include <vector>
#include <map>
#include "computation/expression.H"

class Parameters;
class Program;

struct bugs_cmd
{
  expression_ref var;
  std::string dist;
  std::vector<expression_ref> arguments;
};

expression_ref parse_haskell_line(const std::string& line);
bugs_cmd parse_bugs_line(const std::string& line);
bugs_cmd parse_bugs_line(const Program& P, const std::string& line);
expression_ref postprocess(const Program& p, const expression_ref&);
void add_BUGS(const Parameters& P, const std::string& filename);
#endif
