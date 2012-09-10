#ifndef PROGRAM_H
#define PROGRAM_H

#include <map>
#include <string>
#include <vector>
#include "computation/expression.H"
#include <algorithm>
#include <iostream>

enum fixity_t {unknown_fix=0, non_fix=1, left_fix=2, right_fix=3};
enum symbol_type_t {variable_symbol = 0, constructor_symbol = 1, parameter_symbol = 2, type_symbol = 3};

struct symbol_info
{
  std::string name;
  int symbol_type;
  int arity = -1;
  int precedence = -1;
  fixity_t fixity = unknown_fix;
  expression_ref body;
  expression_ref type;

  symbol_info() {};
  symbol_info(const std::string&, symbol_type_t, int, int, fixity_t);
  symbol_info(const std::string&, symbol_type_t, int, int, fixity_t, const expression_ref&);
  symbol_info(const std::string&, symbol_type_t, int, int, fixity_t, const expression_ref&, const expression_ref&);
};

struct Def
{
  std::vector<expression_ref> patterns;
  std::vector<expression_ref> bodies;
  Def& operator()(const expression_ref&, const expression_ref&);

  Def(const expression_ref&, const expression_ref&);
};

std::ostream& operator<<(std::ostream&, const Def&);

struct Program
{
  std::map<std::string, symbol_info> symbols;

  /// Add a function
  void def_function(const std::string& name, int arity, int precedence, fixity_t f, const expression_ref& b, const expression_ref& type);

  /// Add a function
  void def_function(const std::string& name, int arity, int precedence, fixity_t f, const expression_ref& b);

  /// Add a function
  void def_function(const std::string& name, const expression_ref& b);

  // Add a function
  void def_function(const std::vector<expression_ref>&, const std::vector<expression_ref>&);

  Program& operator+=(const Def&);

  Program& operator+=(const symbol_info&);

  Program& operator+=(const Program&);

  bool is_declared(const std::string&) const;
  
  expression_ref get_function(const std::string&) const;

  symbol_info get_operator(const std::string& name) const;
};

std::ostream& operator<<(std::ostream&, const Program&);


#endif
