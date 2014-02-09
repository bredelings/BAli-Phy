#ifndef DESUGAR_H
#define DESUGAR_H
#include "parser/parse.H"

#include <set>
#include <string>
#include "computation/expression.H"

class Module;
class Parameters;

expression_ref parse_haskell_line(const Module& P, const std::string& line);
expression_ref parse_bugs_line(const Module& P, const std::string& line);
expression_ref desugar(const Module& p, const expression_ref&);
expression_ref desugar(const Module& p, const expression_ref&,const std::set<std::string>&);
Module read_model(const Parameters& P, const std::string& filename);
void add_model(Parameters& P, const std::string& filename);

#endif
