#include "AST.H"

using std::string;

bool is_AST(const expression_ref& E, const string& type)
{
  if (not E.is_a<AST_node>()) return false;
  return E.as_<AST_node>().type == type;
}

bool is_AST(const expression_ref& E, const string& type, const string& value)
{
  if (not E.is_a<AST_node>()) return false;
  return (E.as_<AST_node>().type == type) and (E.as_<AST_node>().value == value);
}

