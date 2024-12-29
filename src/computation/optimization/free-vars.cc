#include "free-vars.H"

#include "computation/expression/var.H"
#include "computation/expression/let.H"
#include "computation/expression/lambda.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/expression/apply.H"
#include "computation/operation.H"                // for is_non_apply_op_exp( )

#include "util/string/join.H"

using std::string;
using std::vector;


template <>
string annot_expression_ref<FreeVarSet>::print() const
{
    if (note.empty())
        return exp.print();

    vector<string> xs;
    for(auto& x: note)
        xs.push_back(x.print());

    string s = "{"+join(xs,",")+"|" + exp.print()+"}";
    return s;
}

FreeVarSet erase(const FreeVarSet& s, const vector<var>& xs)
{
    FreeVarSet s2 = s;
    for(auto& x: xs)
        s2 = s2.erase(x);
    return s2;
}

void get_vars_(const expression_ref& pattern, vector<var>& vars)
{
    if (is_var(pattern))
        vars.push_back(pattern.as_<var>());
    else if (pattern.is_expression())
        for(auto& Evar: pattern.sub())
            get_vars_(Evar, vars);
}

vector<var> get_vars(const expression_ref& pattern)
{
    vector<var> vars;
    get_vars_(pattern, vars);
    return vars;
}

FreeVarSet get_union(const FreeVarSet& s1, const FreeVarSet& s2)
{
    if (s1.size() >= s2.size())
    {
        FreeVarSet s3 = s1;
        for(auto& x: s2)
            if (not s3.count(x))
                s3 = s3.insert(x);
        return s3;
    }
    else
    {
        FreeVarSet s3 = s2;
        for(auto& x: s1)
            if (not s3.count(x))
                s3 = s3.insert(x);
        return s3;
    }
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

const FreeVarSet& get_free_vars(const annot_expression_ref<FreeVarSet>& e)
{
    return e.note;
}

const FreeVarSet& get_free_vars(const expression_ref& E)
{
    return get_free_vars(E.as_<annot_expression_ref<FreeVarSet>>());
}


annot_expression_ref<FreeVarSet>
add_free_variable_annotations(const expression_ref& E)
{
    // 1. Var
    if (is_var(E))
    {
        const auto& x = E.as_<var>();
        FreeVarSet free_vars;
        return {free_vars.insert(x), E};
    }

    // 3. Lambda
    else if (is_lambda_exp(E))
    {
        const auto& x = E.sub()[0].as_<var>();
        const auto& body = E.sub()[1];

        auto body2 = add_free_variable_annotations(body);
        auto free_vars = get_free_vars(body2);

        return {free_vars.erase(x), lambda_quantify(x, body2)};
    }

    // 4. Case
    else if (auto C = parse_case_expression(E))
    {
        auto& [object, alts] = *C;
        object = add_free_variable_annotations(object);
        auto free_vars = get_free_vars(object);

        for(auto& [pattern, body]: alts)
        {
            body = add_free_variable_annotations(body);
            auto free_vars_i = erase(get_free_vars(body), get_vars(pattern));
            free_vars = get_union(free_vars, free_vars_i);
        }

        return {free_vars, make_case_expression(object, alts)};
    }

    // 5. Let
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

        L.body = add_free_variable_annotations(L.body);
        FreeVarSet free_vars = get_free_vars(L.body);
        vector<var> binders;

        for(auto& [var,rhs]: L.binds)
        {
            binders.push_back(var);
            rhs = add_free_variable_annotations(rhs);
            free_vars = get_union(free_vars, get_free_vars(rhs));
        }
        free_vars = erase(free_vars, binders);

        return {free_vars, L};
    }

    // 2. Constant
    else if (not E.size())
    {
        return {FreeVarSet{},E};
    }

    // 6. Apply or constructor or Operation
    else if (is_apply_exp(E) or is_constructor_exp(E) or is_non_apply_op_exp(E))
    {
        object_ptr<expression> V2 = E.as_expression().clone();

        FreeVarSet free_vars;
        for(int i=0;i<E.size();i++)
        {
            V2->sub[i] = add_free_variable_annotations(V2->sub[i]);
            auto free_vars_i = get_free_vars(V2->sub[i]);
            free_vars = get_union(free_vars, free_vars_i);
        }
        return {free_vars,V2};
    }

    std::abort();
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
        auto free_vars = get_free_vars(head);
        for(auto& arg: A->args)
            free_vars = free_vars.insert(arg);
        return FV::Exp(FV::Apply{head, A->args}, free_vars);
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

        FV::Alts alts;
        for(auto& [pat, body]: C->alts)
        {
            auto fv_body = add_free_variable_annotations(body);
            auto alt_free_vars = get_free_vars(fv_body);

            if (auto CP = pat.to_con_pat())
            {
                for(auto arg: CP->args)
                    alt_free_vars = alt_free_vars.erase(arg);
            }

            alts.push_back({pat, fv_body});

            free_vars = get_union(free_vars, alt_free_vars);
        }
        return FV::Exp(FV::Case{object, alts}, free_vars);
    }
    // 6. ConApp
    else if (auto CA = E.to_conApp())
    {
        FreeVars free_vars;
        for(auto& arg: CA->args)
            free_vars = free_vars.insert(arg);
        return FV::Exp(*CA, free_vars);
    }
    // 7. BuiltinOp
    else if (auto B = E.to_builtinOp())
    {
        FreeVars free_vars;
        for(auto& arg: B->args)
            free_vars = free_vars.insert(arg);
        return FV::Exp(*B, free_vars);
    }
    else if (auto C = E.to_constant())
        return *C;
    else
        throw myexception()<<"add_free_variable_annotations: I don't recognize expression '"+ E.print() + "'";
}

