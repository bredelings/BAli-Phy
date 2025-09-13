#ifndef SIMPLIFIER_OPTIONS_H
#define SIMPLIFIER_OPTIONS_H

#include "inliner.H"

struct pass_options
{
    bool typecheck = true;
    bool optimize = true;
};

struct dump_options
{
    bool dump_parsed = false;
    bool dump_renamed = false;
    bool dump_typechecked = false;
    bool dump_desugared = false;
    bool dump_optimized = false;
};

struct simplifier_options: public pass_options,
                           public dump_options,
                           public inliner_options
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

    bool fully_lazy = true;
};

#endif /*SIMPLIFIER_OPTIONS_H */
