#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

tuple<expression_ref, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, expression_ref E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto& x_name = unloc(x->name);
        auto sigma = env.find( x_name );

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x->print()<<"'";

        auto [_, constraints, type] = instantiate(*sigma);

        for(auto& constraint: constraints)
        {
            auto dvar = add_dvar(constraint);
            E = {E, dvar};
        }

        return {E, type};
    }
    else if (E.is_int())
    {
        auto [dvar, type] = fresh_num_type();
        E = { find_prelude_var("fromInteger"), dvar, E };
        return { E, type };
    }
    else if (E.is_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        E = { find_prelude_var("fromRational"), dvar, E };
        return { E, type };
    }
    else if (E.is_char())
    {
        return { E, char_type() };
    }
    else if (E.is_log_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        return { E, type };
    }
    else if (auto L = E.to<Hs::Literal>())
    {
        if (auto c = L->is_Char())
        {
            return { *c, char_type() };
        }
        else if (auto i = L->is_Integer())
        {
            auto [dvar, type] = fresh_num_type();
            expression_ref E = { find_prelude_var("fromInteger"), dvar, *i };
            return { E, type };
        }
        else if (auto s = L->is_String())
        {
            std::abort();
        }
        else if (auto d = L->is_Double())
        {
            auto [dvar, type] = fresh_fractional_type();
            expression_ref E = { find_prelude_var("fromRational"), dvar, *d };
            return { E, type };
        }
        else if (auto i = L->is_BoxedInteger())
        {
            return { *i, int_type() };
        }
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        // So, ( e :: tau ) should be equivalent to ( let x :: tau ; x = e in x )
        // according to the 2010 report.

        // Example: (\x -> x) :: Num a => a -> a
        // In this example, we should rewrite this to \dNum -> \x -> x

        // texp->exp;
        // texp->type

        // FIXME: For better error messages, we should inline the code for inferring types of LetExp.
        //        We will know we will call infer_type_for_single_fundecl_with_sig

        auto x = get_fresh_Var("tmp", false);
        Hs::Decls decls;
        decls.push_back(simple_decl(x,texp->exp));
        Hs::Binds binds;
        // By making a LetExp, we rely on the Let code to handle the type here.
        binds.signatures.insert({unloc(x.name), texp->type});
        binds.push_back(decls);
        expression_ref E2 = Hs::LetExp({noloc,binds},{noloc,x});

        return infer_type(env, E2);
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_meta_type_var( kind_star() );
        auto L = *l;
        for(auto& element: L.elements)
        {
            auto [element1, t1] = infer_type(env, element);
            element = element1;
            unify(t1, element_type);
        }
        return { L, Hs::ListType(element_type) };
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [element1, element_type] = infer_type(env, element);
            element = element1;
            element_types.push_back( element_type );
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        return {T, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];

        auto [f, t1] = infer_type(env,e1);

        vector<expression_ref> args;
        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_meta_type_var( kind_star() );

            auto [arg2, t2] = infer_type(env, e2);
            args.push_back(arg2);

            unify (t1, make_arrow_type(t2,tv));

            t1 = tv;
        }
        E = apply_expression(f, args);

        return {E, t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto rule = Hs::MRule{Lam.args, Lam.body};
        auto [rule2, t] = infer_type(env, rule);
        Lam.args = rule.patterns;
        Lam.body = rule.rhs;
        return {Lam, t};
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;

        // 1. Extend environment with types for decls, get any substitutions
        auto [binds, binders] = infer_type_for_binds(env, unloc(Let.binds));
        unloc(Let.binds) = binds;
        auto env2 = plus_prefer_right(env, binders);

        // 2. Compute type of let body
        auto [body, t_body] = infer_type(env2, unloc(Let.body));
        unloc(Let.body) = body;

        // return (s1 `compose` s2, t2)
        return {Let, t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        auto [type, field_types] = constr_types(*con);

        vector<Hs::Type> arg_types;
        vector<Hs::Exp> args = E.copy_sub();
        for(int i=0; i < args.size(); i++)
        {
            auto& arg = args[i];
            auto [arg_i, t_i] = infer_type(env, arg);
            arg = arg_i;
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            unify( field_types[i], t_i);
        }
        E = expression_ref(*con, args);
        
        return { E, type };
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;

        // 1. Determine object type
        auto [object, object_type] = infer_type(env, Case.object);
        Case.object = object;
        
        // 2. Determine data type for object from patterns.
        Hs::Match match;
        for(auto& alt: Case.alts)
        {
            auto& [pattern, body] = unloc(alt);
            match.rules.push_back(Hs::MRule{{pattern},body});
        }

        auto [match2, match_type] = infer_type(env, match);

        for(int i=0;i<Case.alts.size();i++)
        {
            unloc(Case.alts[i]) = {match2.rules[i].patterns[0], match2.rules[i].rhs};
        }

        Hs::Type result_type = fresh_meta_type_var( kind_star() );

        unify( make_arrow_type(object_type,result_type), match_type );

        return { Case, result_type };
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto If = *if_exp;
        auto [cond, cond_type ] = infer_type(env, unloc(If.condition));
        auto [tbranch, tbranch_type] = infer_type(env, unloc(If.true_branch));
        auto [fbranch, fbranch_type] = infer_type(env, unloc(If.false_branch));
        unloc(If.condition) = If;
        unloc(If.true_branch) = tbranch;
        unloc(If.false_branch) = fbranch;

        unify(cond_type, bool_type());
        unify(tbranch_type, fbranch_type);

        return {If, tbranch_type};
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto [quals, quals_binders] = infer_quals_type(env, LComp.quals);
        auto [body, exp_type] = infer_type(plus_prefer_right(env, quals_binders), LComp.body);
        LComp.quals = quals;
        LComp.body = body;

        Hs::Type result_type = Hs::ListType(exp_type);

        return { LComp, result_type };
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();

        // PROBLEM: Do we need to desugar these here, in order to plug in the dictionary?
        auto [from, t_from] = infer_type(env, L.from);
        L.from = from;
        unify(t,t_from);

        return {L, Hs::ListType(t) };
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, L.from);
        unify(t,t_from);

        auto [then, t_then] = infer_type(env, L.then);
        L.from = from;
        L.then = then;
        
        return {L, Hs::ListType(t)};
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, l->from);
        unify(t,t_from);

        auto [to, t_to] = infer_type(env, l->to);
        L.from = from;
        L.to = to;
        
        return {L, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, L.from);
        unify(t,t_from);

        auto [then, t_then] = infer_type(env, L.then);
        unify(t,t_then);

        auto [to, t_to] = infer_type(env, l->to);
        unify(t,t_to);

        L.from = from;
        L.then = then;
        L.to = to;

        return {L, Hs::ListType(t)};
    }
    else
        throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

