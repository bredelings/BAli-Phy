#ifndef LET_FLOAT_H
#define LET_FLOAT_H
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"

expression_ref let_float(const expression_ref& E);

var strip_level(var x);
expression_ref strip_level_from_pattern(const expression_ref& pattern);
void set_level_for_module(std::vector<CDecls>& module);
void float_out_from_module(std::vector<CDecls>& module);

#endif
