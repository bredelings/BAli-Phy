#ifndef PROGRAM_H
#define PROGRAM_H

#include <map>
#include <string>
#include <vector>
#include "computation/expression.H"
#include <algorithm>
#include <iostream>

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
  std::map<std::string, std::pair<expression_ref,expression_ref> > functions;

  /// Add a function called 'name', with body 'E', and type 'T'
  void def_function(const std::string& name, const expression_ref& E, const expression_ref& T);

  /// Add a function called 'name', with body 'E', and type *
  void def_function(const std::string& name, const expression_ref& E);
  
  /// Add a function called 'name' with body parameter(name) and type 'T'
  void def_parameter(const std::string&, const expression_ref& T);
  
  /// Add a function called 'name' with body parameter(name).
  void def_parameter(const std::string&);
  
  void def_function(const std::vector<expression_ref>&, const std::vector<expression_ref>&);

  Program& operator+=(const Def&);

  Program& operator+=(const Program&);

  bool is_declared(const std::string&) const;
  
  expression_ref get_function(const std::string&) const;
};

std::ostream& operator<<(std::ostream&, const Program&);


#endif
