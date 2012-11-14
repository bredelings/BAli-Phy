#ifndef PARSE_H
#define PARSE_H
#include <string>
#include <vector>
#include "computation/expression.H"

struct bugs_cmd
{
  expression_ref var;
  std::string dist;
  std::vector<expression_ref> arguments;
};

expression_ref parse_haskell_line(const std::string& line);
expression_ref parse_haskell_decls(const std::string& line);
bugs_cmd parse_bugs_line(const std::string& line);
#endif
