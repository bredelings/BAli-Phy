#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include "computation/expression/expression_ref.H"

struct simplifier_options
{
    bool pre_inline_unconditionally = true;
    bool post_inline_unconditionally = true;
    bool let_float_from_case = true;
    bool let_float_from_apply = true;
    bool let_float_from_let = true;
    bool case_of_constant = true;
    bool case_of_variable = true;
    bool beta_reduction = true;
    int max_iterations = 4;
};

class module_loader;

expression_ref simplifier(const simplifier_options&, const expression_ref&);

#endif
