#ifndef TYPECHECK_MATCH_H
#define TYPECHECK_MATCH_H

#include "typecheck.H"

template <typename A>
Core::wrapper typechecker_state::tcMatchesFunInfer(std::vector<Expected>& arg_types, int arity, const Expected& extra_fun_type, const A& a)
{
    std::vector<Expected> extra_arg_types;
    // pad out with FlexiTyVars ... what's that?
    while(arg_types.size() < arity)
    {
        auto arg_type = Infer();
        arg_types.push_back( arg_type );
        extra_arg_types.push_back( arg_type );
    }
    Expected result_type = Infer();

    a(arg_types, result_type)(*this);

    auto unif_fun_type = Hs::function_type( read_types(extra_arg_types), result_type.read_type());

    if (extra_fun_type.infer() and not extra_fun_type.inferred_type())
    {
        extra_fun_type.infer_type(unif_fun_type);
        return Core::wrapper_id;
    }
    else
        return subsumptionCheck(unif_fun_type, extra_fun_type.read_type());
}

template <typename A>
Core::wrapper typechecker_state::tcMatchesFunCheck(std::vector<Expected>& arg_types, int arity, Hs::Type type, const A& a)
{
    // There should also be a wrapper for skolemize -- gen_wrapper.
    // It will handle cases like forall a. C1 => forall b. C2 => E.
    // Until we add type arguments, it should be Core::wrapper_id though.

    if (not is_rho_type(type))
    {
        auto [wrap_gen, tcs, givens, rho_type, decls, match_wrapper] =
            skolemize_and_result<Core::wrapper>
            (
                type,
                [&](const Hs::Type& rho_type, auto& tc2) {
                    return tc2.tcMatchesFunCheck(arg_types, arity, rho_type, a);
                }
            );

        return wrap_gen * match_wrapper;
    }
    else if (arg_types.size() == arity)
    {
        // If we got here, then the Check type had enough arguments.
        // We don't need to construct a result type, we just need to check things.
        a(arg_types, Check(type))(*this);
        return Core::wrapper_id;
    }
    // look through type synonyms
    else if (auto type2 = is_type_synonym(type))
    {
        return tcMatchesFunCheck(arg_types, arity, *type2, a);
    }
    // get the next argument type
    else if (auto ftype = Hs::is_function_type(type))
    {
        auto [arg_type, result_type] = *ftype;
        arg_types.push_back(Check(arg_type));
        return tcMatchesFunCheck(arg_types, arity, result_type, a);
    }
    // look through filled MetaTypeVars
    else if (auto mtv = type.to<Hs::MetaTypeVar>())
    {
        if (mtv->filled())
            return tcMatchesFunCheck(arg_types, arity, *mtv->filled(), a);
        else
        {
            // We don't know the remaining argument types.
            return tcMatchesFunInfer(arg_types, arity, Check(type), a);
        }
    }
    // 
    else
    {
        // This is different from the case above, because we know that the number
        // of arguments doesn't match the arity.  Which is not NECESSARILY wrong.

        return tcMatchesFunInfer(arg_types, arity, Check(type), a);
    }

    std::abort();
}

template <typename A>
Core::wrapper typechecker_state::tcMatchesFun(int arity, const Expected& fun_type, const A& action)
{
    std::vector<Expected> arg_types;
    if (fun_type.check())
        return tcMatchesFunCheck(arg_types, arity, fun_type.read_type(), action);
    else
        return tcMatchesFunInfer(arg_types, arity, fun_type, action);
}

#endif
