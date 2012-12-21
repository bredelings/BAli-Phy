#ifndef PROGRAM_H
#define PROGRAM_H

#include <map>
#include <string>
#include <vector>
#include "computation/expression.H"
#include <algorithm>
#include <iostream>

enum fixity_t {unknown_fix=0, non_fix=1, left_fix=2, right_fix=3};
enum symbol_type_t {variable_symbol = 0, constructor_symbol = 1, parameter_symbol = 2, type_symbol = 3, unknown_symbol = 4};
enum scope_t {local_scope, external_scope, global_scope, unknown_scope};

struct symbol_info
{
  std::string name;
  symbol_type_t symbol_type;
  scope_t scope = unknown_scope;
  int arity = -1;
  int precedence = -1;
  fixity_t fixity = unknown_fix;
  expression_ref body;
  expression_ref type;

  symbol_info() {};
  symbol_info(const std::string&, symbol_type_t, scope_t, int);
  symbol_info(const std::string&, symbol_type_t, scope_t, int, const expression_ref&);
  symbol_info(const std::string&, symbol_type_t, scope_t, int, int, fixity_t);
  symbol_info(const std::string&, symbol_type_t, scope_t, int, int, fixity_t, const expression_ref&);
  symbol_info(const std::string&, symbol_type_t, scope_t, int, int, fixity_t, const expression_ref&, const expression_ref&);
};

bool is_haskell_varid(const std::string& s);
bool is_haskell_conid(const std::string& s);
bool is_haskell_varsym(const std::string& s);
bool is_haskell_consym(const std::string& s);
bool is_haskell_normal_con_name(const std::string& s);
bool is_haskell_builtin_con_name(const std::string& s);

bool is_haskell_var_name(const std::string& s);
bool is_haskell_con_name(const std::string& s);
bool is_haskell_module_name(const std::string& s);

bool is_qualified_symbol(const std::string&);

std::string get_module_name(const std::string&);
std::string get_unqualified_name(const std::string&);

std::vector<std::string> get_haskell_identifier_path(const std::string& s);

class Program
{
  std::map<std::string, symbol_info> symbols;

  std::map<std::string, std::string> aliases;

public:
  std::string module_name;

  const std::map<std::string, symbol_info>& get_symbols() const {return symbols;}

  /// Add a function
  void def_function(const std::string& name, const expression_ref& b, const expression_ref& type);

  /// Add a function
  void def_function(const std::string& name, const expression_ref& b);

  // Add a function
  void def_function(const std::vector<expression_ref>&, const std::vector<expression_ref>&);

  /// Add a constructor
  void def_constructor(const std::string& name, int arity);

  Program& operator+=(const char*);

  Program& operator+=(const std::string&);

  Program& operator+=(const expression_ref&);

  void add_alias(const std::string&);

  void add_symbol(const symbol_info&, scope_t);

  void add_symbol(const symbol_info&);

  void declare_fixity(const std::string&, int precedence, fixity_t f);

  void declare_symbol(const symbol_info&);

  void declare_parameter(const std::string& name);

  void declare_parameter(const std::string& name, const expression_ref& type);

  void import_symbol(const symbol_info&, bool qualified);

  void import_module(const Program& P2, bool qualified);

  void import_module(const std::vector<std::string>& path, const std::string&, bool qualified);

  bool is_declared_qualified(const std::string&) const;

  bool is_declared_unqualified(const std::string&) const;

  bool is_declared(const std::string&) const;
  
  const symbol_info& lookup_qualified_symbol(const std::string&) const;

  const symbol_info& lookup_unqualified_symbol(const std::string&) const;

  symbol_info lookup_builtin_symbol(const std::string&) const;

  symbol_info lookup_symbol(const std::string&) const;

  expression_ref get_function(const std::string&) const;

  symbol_info get_operator(const std::string& name) const;

  Program(const std::string&);
};

std::ostream& operator<<(std::ostream&, const Program&);

expression_ref resolve_refs(const Program&, const expression_ref&);

#endif
