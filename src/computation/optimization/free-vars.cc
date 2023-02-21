#include "free-vars.H"

#include "computation/expression/var.H"
#include "computation/expression/let.H"
#include "computation/expression/lambda.H"
#include "computation/expression/case.H"
#include "computation/expression/operator.H"
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

