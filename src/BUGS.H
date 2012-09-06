#ifndef BUGS_H
#define BUGS_H
#include <string>
#include <vector>
#include <map>
#include "parameters.H"

struct symbol_info
{
  std::string name;
  int type = -1; // variable = 0, constructor = 1, parameter = 2
  int arity = -1;
  int precedence = -1;
  int fixity = -1; // 0 = none, 1 = left, 2 = right
};

struct module
{
  std::vector<std::string> name;
  std::map<std::string, symbol_info> symbols;

  symbol_info get_operator(const std::string& name) const;
};

void add_BUGS(const Parameters& P, const std::string& filename);
expression_ref postprocess(const module& m, const expression_ref&);
#endif
