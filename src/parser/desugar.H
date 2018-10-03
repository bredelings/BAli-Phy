#ifndef DESUGAR_H
#define DESUGAR_H
#include "parser/parse.H"

#include <set>
#include <string>
#include "computation/expression/expression_ref.H"

class Module;
class Model;

std::set<std::string> find_bound_vars(const expression_ref& E);
std::set<std::string> find_all_ids(const expression_ref& E);
std::string get_func_name(const expression_ref& decl);
bool is_pattern_binding(const expression_ref& decl);
bool is_function_binding(const expression_ref& decl);

expression_ref parse_haskell_line(const Module& P, const std::string& line);
expression_ref rename_infix(const Module& p, const expression_ref&);
expression_ref rename(const Module& p, const expression_ref&);
expression_ref rename(const Module& p, const expression_ref&,const std::set<std::string>&);
expression_ref desugar(const Module& p, const expression_ref&);
expression_ref desugar(const Module& p, const expression_ref&,const std::set<std::string>&);
Module read_model(const std::string& filename);
void read_add_model(Model& M, const std::string& filename);
void add_model(Model& M, const std::string& filename);

#endif
