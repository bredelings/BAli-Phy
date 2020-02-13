#ifndef LET_FLOAT_H
#define LET_FLOAT_H
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"

expression_ref let_float(const expression_ref& E);

expression_ref set_level(const expression_ref& E);
void float_out_from_module(std::vector<CDecls>& module);

#endif
