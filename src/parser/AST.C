#include "AST.H"

using std::string;

bool is_AST(const expression_ref& E, const string& type)
{
  auto ast = E.is_a<AST_node>();
  if (not ast) return false;
  return ast->type == type;
}

bool is_AST(const expression_ref& E, const string& type, const string& value)
{
  auto ast = E.is_a<AST_node>();
  if (not ast) return false;
  return ast->type == type and ast->value == value;
}

