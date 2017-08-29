#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include <set>
#include <map>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
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
    bool case_of_case = true;
    bool beta_reduction = true;
    int max_iterations = 4;
    int inline_threshhold = 8;
    double keenness = 1.5;
};

class module_loader;

std::pair<expression_ref,std::set<dummy>> occurrence_analyzer(const expression_ref& E, var_context context=var_context::unknown);

CDecls simplify_module(const simplifier_options&, const std::map<dummy, expression_ref>&, const std::set<dummy>&, CDecls);

#endif
