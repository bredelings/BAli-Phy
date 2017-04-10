#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include <set>
#include <map>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/dummy.H"

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

std::pair<expression_ref,std::set<dummy>> occurrence_analyzer(const expression_ref& E, var_context context=var_context::unknown);

expression_ref simplifier(const simplifier_options&, const std::map<dummy, expression_ref>&, const std::set<dummy>&, const expression_ref&);

#endif
