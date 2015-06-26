#include "AST.H"

using std::string;

bool is_AST(const expression_ref& E, const string& type)
{
  if (not E.head().is_a<AST_node>()) return false;
  return E.head().as_<AST_node>().type == type;
}

bool is_AST(const expression_ref& E, const string& type, const string& value)
{
  if (not E.head().is_a<AST_node>()) return false;
  return (E.head().as_<AST_node>().type == type) and (E.head().as_<AST_node>().value == value);
}

