#include "core.H"

#include "expression/tuple.H"
#include "expression/apply.H"
#include "expression/lambda.H"
#include "expression/case.H"

using std::vector;

namespace Core
{

    // Expression stuff

    Exp Tuple(const std::vector<Exp>& es)
    {
        return get_tuple(es);
    }

    Exp Lambda(const std::vector<Var>& args, const Exp& body)
    {
        if (args.empty())
            return body;
        else
            return lambda_quantify(args, body);
    }

    Exp Let(const Decls& decls, const Exp& body)
    {
        if (decls.empty())
            return body;
        else
            return let_expression(decls, body);
    }

    Exp Apply(const Exp& fun, const std::vector<Exp>& args)
    {
        if (args.empty())
            return fun;
        else
            return apply_expression(fun, args);
    }

    Exp Apply(const Exp& fun, const std::vector<Var>& args)
    {
        if (args.empty())
            return fun;

        vector<Exp> args2;
        for(auto& arg: args)
            args2.push_back(arg);

        return apply_expression(fun, args2);
    }

    Exp Case(const Exp& object, const std::vector<Pat>& patterns, const std::vector<Exp>& bodies)
    {
        return make_case_expression(object, patterns, bodies);
    }
    // wrapper stuff

    wrapper wrapper_id = [](const Exp& x) {return x;};

    wrapper WrapLet(const Decls& args)
    {
        return [=](const Core::Exp& x)
            {
                return Let(args, x);
            };
    }

    wrapper WrapLambda(const std::vector<Var>& args)
    {
        return [=](const Core::Exp& x)
            {
                return Lambda(args,x);
            };
    }

    wrapper WrapApply(const std::vector<Exp>& args)
    {
        return [=](const Core::Exp& x)
            {
                return Apply(x, args);
            };
    }

    wrapper WrapApply(const std::vector<Var>& args)
    {
        return [=](const Core::Exp& x)
            {
                return Apply(x, args);
            };
    }

}


Core::wrapper operator*(const Core::wrapper& w1, const Core::wrapper& w2)
{
    return [=](const Core::Exp& x)
        {
            return w1(w2(x));
        };
}
