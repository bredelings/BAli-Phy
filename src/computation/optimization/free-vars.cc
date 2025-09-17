#include "free-vars.H"

#include "util/string/join.H"

using std::string;
using std::vector;


FreeVars erase(const FreeVars& s, const vector<FV::Var>& xs)
{
    auto s2 = s;
    for(auto& x: xs)
        s2 = s2.erase(x);
    return s2;
}

FreeVars get_union(const FreeVars& s1, const FreeVars& s2)
{
    if (s1.size() >= s2.size())
    {
        auto s3 = s1;
        for(auto& x: s2)
            if (not s3.count(x))
                s3 = s3.insert(x);
        return s3;
    }
    else
    {
        auto s3 = s2;
        for(auto& x: s1)
            if (not s3.count(x))
                s3 = s3.insert(x);
        return s3;
    }
}

const FreeVars& get_free_vars(const FV::Exp& E)
{
    return E.note();
}

FreeVars& get_free_vars(FV::Exp& E)
{
    return E.note();
}

FV::Exp
add_free_variable_annotations(const Core2::Exp<>& E)
{
    // 1. Var
    if (auto V = E.to_var())
    {
        return FV::Exp(*V, {*V});
    }
    // 2. Lambda
    else if (auto L = E.to_lambda())
    {
        auto body = add_free_variable_annotations(L->body);
        auto free_vars = get_free_vars(body);
        free_vars = free_vars.erase(L->x);

        return FV::Exp(FV::Lambda{L->x, body}, free_vars);
    }
    // 3. Apply
    else if (auto A = E.to_apply())
    {
        auto head = add_free_variable_annotations(A->head);
        auto head_free_vars = get_free_vars(head);
        auto arg  = add_free_variable_annotations(A->arg);
        auto arg_free_vars = get_free_vars(arg);
        return FV::Exp(FV::Apply{head, arg}, get_union(head_free_vars, arg_free_vars));
    }
    // 4. Let
    else if (auto L = E.to_let())
    {
        auto body = add_free_variable_annotations(L->body);
        FreeVars free_vars = get_free_vars(body);

        FV::Decls decls;
        for(auto& [x,rhs]: L->decls)
        {
            auto fv_rhs = add_free_variable_annotations(rhs);
            free_vars = get_union(free_vars, get_free_vars(fv_rhs));
            decls.push_back({x,fv_rhs});
        }

        for(auto& [x,_]: L->decls)
            free_vars = free_vars.erase(x);

        return FV::Exp(FV::Let{decls, body}, free_vars);
    }
    // 5. Case
    else if (auto C = E.to_case())
    {
        auto object = add_free_variable_annotations(C->object);

        auto free_vars = get_free_vars(object);

        vector<FV::Alt> alts;
        for(auto& [pat, body]: C->alts)
        {
            auto fv_body = add_free_variable_annotations(body);
            auto alt_free_vars = get_free_vars(fv_body);

            for(auto arg: pat.args)
                alt_free_vars = alt_free_vars.erase(arg);

            alts.push_back({pat, fv_body});

            free_vars = get_union(free_vars, alt_free_vars);
        }
        return FV::Exp(FV::Case{object, alts}, free_vars);
    }
    // 6. ConApp
    else if (auto CA = E.to_conApp())
    {
        vector<FV::Exp> fv_args;
        FreeVars free_vars;
        for(auto& arg: CA->args)
        {
            auto fv_arg = add_free_variable_annotations(arg);
            fv_args.push_back(fv_arg);
            free_vars = get_union(free_vars, get_free_vars(fv_arg));
        }
        return FV::Exp(FV::ConApp(CA->head, fv_args), free_vars);
    }
    // 7. BuiltinOp
    else if (auto B = E.to_builtinOp())
    {
        vector<FV::Exp> fv_args;
        FreeVars free_vars;
        for(auto& arg: B->args)
        {
            auto fv_arg = add_free_variable_annotations(arg);
            fv_args.push_back(fv_arg);
            free_vars = get_union(free_vars, get_free_vars(fv_arg));
        }
        return FV::Exp(FV::BuiltinOp{B->lib_name, B->func_name,fv_args,B->op}, free_vars);
    }
    else if (auto C = E.to_constant())
        return *C;
    else
        throw myexception()<<"add_free_variable_annotations: I don't recognize expression '"+ E.print() + "'";
}

