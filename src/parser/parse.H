#ifndef PARSE_H
#define PARSE_H
#include <string>
#include <vector>
#include "computation/expression.H"

expression_ref parse_haskell_line(const std::string& line);
expression_ref parse_haskell_decls(const std::string& line);
expression_ref parse_bugs_line(const std::string& line);
expression_ref parse_bugs_file(const std::string& line);
#endif
