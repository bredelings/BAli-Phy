#include "core.H"

#include "expression/tuple.H"
#include "expression/apply.H"
#include "expression/lambda.H"
#include "expression/case.H"

#include <fstream>
#include <cereal/archives/binary.hpp>

#include "computation/fresh_vars.H"

#include "computation/expression/convert.H"

using std::vector;
using std::tuple;

namespace Core
{

    // Expression stuff
    Exp ConExp(const std::string& name, const std::vector<Exp>& es)
    {
        return expression_ref{constructor(name,es.size()), es};
    }

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


    tuple<Decls, vector<Exp>>
    args_to_vars(const vector<Exp>& args, FreshVarSource& source)
    {
        vector<Exp> vars;
        CDecls decls;
        for(auto& arg: args)
        {
            if (auto v = arg.to<var>())
                vars.push_back(*v);
            else
            {
                auto a = source.get_fresh_var("a");
                decls.push_back({a,arg});
                vars.push_back(a);
            }
        }
        return {decls, vars};
    }

    Exp safe_apply(const Exp& head, const vector<Exp>& args, FreshVarSource& source)
    {
        auto [decls, vars] = args_to_vars(args, source);

        return Let(decls, Apply(head,vars));
    }
}

