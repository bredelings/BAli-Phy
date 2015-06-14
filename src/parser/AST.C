#include "AST.H"

using std::string;

bool is_AST(const expression_ref& E, const string& type)
{
  if (not is_a<AST_node>(E)) return false;
  return as_<AST_node>(E).type == type;
}

bool is_AST(const expression_ref& E, const string& type, const string& value)
{
  if (not is_a<AST_node>(E)) return false;
  return (as_<AST_node>(E).type == type) and (as_<AST_node>(E).value == value);
}

