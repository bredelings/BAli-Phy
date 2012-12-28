#ifndef DESUGAR_H
#define DESUGAR_H
#include "parser/parse.H"

#include <set>
#include <string>
#include "computation/expression.H"
#include "computation/model_notes.H"

class Module;
class Parameters;

expression_ref parse_haskell_line(const Module& P, const std::string& line);
expression_ref parse_bugs_line(const Module& P, const std::string& line);
expression_ref desugar(const Module& p, const expression_ref&);
expression_ref desugar(const Module& p, const expression_ref&,const std::set<std::string>&);
Model_Notes read_BUGS(const std::vector<std::string>& path, const Parameters& P, const std::string& filename, const std::string& module_name);
void add_BUGS(const std::vector<std::string>& path, Parameters& P, const std::string& filename, const std::string& module_name);

#endif
