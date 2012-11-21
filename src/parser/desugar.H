#ifndef DESUGAR_H
#define DESUGAR_H
#include "parser/parse.H"

#include <set>
#include <string>
#include "computation/expression.H"

class Program;
class Parameters;

expression_ref parse_haskell_line(const Program& P, const std::string& line);
expression_ref parse_bugs_line(const Program& P, const std::string& line);
expression_ref desugar(const Program& p, const expression_ref&);
expression_ref desugar(const Program& p, const expression_ref&,const std::set<std::string>&);
void add_BUGS(Parameters& P, const std::string& filename);

#endif
