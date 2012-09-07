#ifndef BUGS_H
#define BUGS_H
#include <string>
#include <vector>
#include <map>
#include "parameters.H"

enum fixity_t {non_fix=1, left_fix=2, right_fix=3};

struct symbol_info
{
  std::string name;
  int type = -1; // variable = 0, constructor = 1, parameter = 2
  int arity = -1;
  int precedence = -1;
  fixity_t fixity;
  symbol_info() {};
  symbol_info(const std::string&, int, int, int, fixity_t);
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
