#ifndef TYPECHECK_MATCH_H
#define TYPECHECK_MATCH_H

#include "typecheck.H"

/*
tcSubType actual expected = addSubTypeCtxt actual expected $ tcSubTypeNC actual expected

tcSubTypeNC actual expected = if expected.check then tc_sub_type actual expected.read_type
                              if expected.infer then (wrap, rho) = instantiate actual
                                            fillInferResult rho expected.infer

tc_sub_type actual expected = tc_sub_type_shallow actual expected

tc_sub_type_shallow actual expected = if definitely_poly expected and definitely_mono_shallow actual then
                                         unify actual expected
                                      else
                                         (sk_rho,sk_wrap) = skolemize expected $ \sk_rho ->
                                                      (i_rho,i_wrap) <- instantiate actual
                                                      unify(i_rho, sk_rho)
                                                      return inner_wrap
                                         return (sk_wrap * inner_wrap)
*/

template <typename A>
Core::wrapper TypeChecker::tcMatchesFunInfer(std::vector<Expected>& arg_types, int arity, const Expected& extra_fun_type, const A& a)
{
    std::vector<Expected> extra_arg_types;
    // pad out with FlexiTyVars ... what's that?
    while(arg_types.size() < arity)
    {
        auto arg_type = newInfer();
        arg_types.push_back( arg_type );
        extra_arg_types.push_back( arg_type );
    }
    Expected result_type = newInfer();

    a(arg_types, result_type)(*this);

    auto unif_fun_type = function_type( read_types(extra_arg_types), result_type.read_type());

    if (auto I = extra_fun_type.infer(); I and not I->type())
    {
        fillInfer(unif_fun_type, *I);
        return Core::WrapId;
    }
    else
        return subsumptionCheck(TypeConvertOrigin(), unif_fun_type, extra_fun_type.read_type());
}

template <typename A>
Core::wrapper TypeChecker::tcMatchesFunCheck(std::vector<Expected>& arg_types, int arity, Type type, const A& a)
{
    if (not is_rho_type(type))
    {
        auto [wrap_gen, tcs, givens, rho_type, wrap_match] =
            skolemize_and_result<Core::wrapper>
            (
                type,
                [&](const Type& rho_type, auto& tc2) {
                    return tc2.tcMatchesFunCheck(arg_types, arity, rho_type, a);
                }
            );

	// It seems like combining wrap_gen and wrap_match like this moves all the
	// invisible arguments (type args and dictionary args) to the front!
	//
	// Q1: Is that what we want?  Shouldn't the extra args be interleaved with the
	//     visible arguments?
	// Q2: Are we calling them correctly?
	// Q3: How would we apply a Int -> forall.t C t => t function to one argument?
	// Q4: If we used the right wrappers, could we make it work?

        return wrap_gen * wrap_match;
    }
    else if (arg_types.size() == arity)
    {
        // If we got here, then the Check type had enough arguments.
        // We don't need to construct a result type, we just need to check things.
        a(arg_types, Check(type))(*this);
        return Core::WrapId;
    }
    // look through type synonyms
    else if (auto type2 = expand_type_synonym(type))
    {
        return tcMatchesFunCheck(arg_types, arity, *type2, a);
    }
    // get the next argument type
    else if (auto ftype = is_function_type(type))
    {
        auto [arg_type, result_type] = *ftype;
        arg_types.push_back(Check(arg_type));
        return tcMatchesFunCheck(arg_types, arity, result_type, a);
    }
    // look through filled MetaTypeVars
    else if (auto mtv = type.to<MetaTypeVar>())
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
Core::wrapper TypeChecker::tcMatchesFun(int arity, const Expected& fun_type, const A& action)
{
    std::vector<Expected> arg_types;
    if (fun_type.check())
        return tcMatchesFunCheck(arg_types, arity, fun_type.read_type(), action);
    else
        return tcMatchesFunInfer(arg_types, arity, fun_type, action);
}

#endif
