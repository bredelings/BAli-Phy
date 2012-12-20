#ifndef AST_H
#define AST_H
#include "computation/expression.H"
#include <string>
bool is_AST(const expression_ref& E, const std::string& type);
bool is_AST(const expression_ref& E, const std::string& type, const std::string& value);
#endif
