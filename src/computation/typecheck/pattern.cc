#include "typecheck.H"
#include "kindcheck.H"

using std::tuple;
using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

tuple<Hs::Var, Hs::Type, local_value_env>
typechecker_state::infer_var_pattern_type(Hs::Var V, const map<string, Hs::Type>& sigs)
{
    auto& name = unloc(V.name);
    local_value_env lve;
    Hs::Type type;

    if (sigs.count(name))
    {
        auto sig_type = sigs.at(name);
        auto [tvs, constraints, monotype] = instantiate(sig_type);
        if (constraints.size())
            throw myexception()<<"variable '"<<name<<"' cannot have constrained type '"<<sig_type<<"' due to monomorphism restriction";
        type = monotype;
    }
    else
    {
        auto tv = fresh_meta_type_var( kind_star() );
        type = tv;
    }
    V.type = type;
    lve = lve.insert({name,type});
    return {V, type, lve};
}

// Figure 24. Rules for patterns
tuple<Hs::Pattern, Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        return infer_var_pattern_type(*v, sigs);
    }
    // CONSTR-PAT
    else if (auto con = pat.head().to<Hs::Con>())
    {
        local_value_env lve;
        vector<Hs::Type> types;
        auto sub_pats = pat.copy_sub();

        for(auto& sub_pat: sub_pats)
        {
            auto [p1, t1, lve1] = infer_pattern_type(sub_pat, sigs);
            sub_pat = p1;
            types.push_back(t1);
            lve += lve1;
        }
        substitution_t s;
        auto [type,field_types] = constr_types(*con);

        assert(field_types.size() == pat.size());

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
            unify(types[i], field_types[i]);

        Hs::Pattern pat2 = pat.head();
        if (pat.size())
            pat2 = expression_ref(pat.head(), sub_pats);

        return { pat2, type, lve };
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto [p1, t1, lve1] = infer_pattern_type(ap->pattern, sigs);

        auto [v2, t2, lve2] = infer_var_pattern_type(ap->var.as_<Hs::Var>(), sigs);

        unify(t1, t2);

        return {Hs::AsPattern(v2, p1), t1, lve1 + lve2};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(lp->pattern, sigs);
        return {Hs::LazyPattern(p), t, lve};
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(sp->pattern, sigs);
        return {Hs::StrictPattern(p), t, lve};
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_meta_type_var( kind_star() );
        return {pat, tv, {}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        local_value_env lve;
        Hs::Type t = fresh_meta_type_var( kind_star() );
        for(auto& element: L.elements)
        {
            auto [p1, t1, lve1] = infer_pattern_type(element, sigs);
            element = p1;

            unify(t, t1);
            lve += lve1;
        }

        return {L, Hs::ListType(t), lve};
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        auto T = *t;
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: T.elements)
        {
            auto [p, t1, lve1] = infer_pattern_type(element, sigs);
            element = p;
            types.push_back(t1);
            lve += lve1;
        }
        return {T, Hs::TupleType(types), lve};
    }
    else if (auto l = pat.to<Hs::Literal>())
    {
        auto L = *l;

        if (L.is_BoxedInteger())
        {
            return {L, int_type(), {}};
        }

        // 1. Typecheck (==)
//        auto [equals, equals_type] = infer_type(gve, Hs::Var({noloc,"Data.Eq.=="}));
//        L.equalsOp = equals;

        if (L.is_Char())
        {
            return {L, char_type(), {}};
        }
        else if (auto i = L.is_Integer())
        {
            // 1. Typecheck fromInteger
            auto [fromInteger, fromInteger_type] = infer_type(gve, Hs::Var({noloc,"Compiler.Num.fromInteger"}));

            // 2. Determine result type
            auto result_type = fresh_meta_type_var( kind_star() );
            unify(fromInteger_type, Hs::make_arrow_type(int_type(), result_type));

            L.literal = Hs::Integer(*i, fromInteger);

            return {L, result_type, {}};
        }
        else if (L.is_String())
        {
            return {L, Hs::ListType(char_type()), {}};
        }
        else if (auto d = L.is_Double())
        {
            // 1. Typecheck fromRational
            auto [fromRational, fromRational_type] = infer_type(gve, Hs::Var({noloc,"Compiler.Num.fromRational"}));

            // 2. Determine result type
            auto result_type = fresh_meta_type_var( kind_star() );
            unify(fromRational_type, Hs::make_arrow_type(double_type(), result_type));

            L.literal = Hs::Double(*d, fromRational);

            return {L, result_type, {}};
        }
        else
            std::abort();
    }
    else if (pat.is_log_double())
        throw myexception()<<"log_double literal should be impossible: '"<<pat<<"'!";
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

Hs::Var
rename_var_pattern_from_bindinfo(Hs::Var V, const map<string, Hs::BindInfo>& bind_info)
{
    auto& name = unloc(V.name);
    auto it = bind_info.find(name);
    assert(it != bind_info.end());
    return it->second.inner_id;
}

// Figure 24. Rules for patterns
Hs::Pattern
rename_pattern_from_bindinfo(const Hs::Pattern& pat, const map<string, Hs::BindInfo>& bind_info)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        return rename_var_pattern_from_bindinfo(*v, bind_info);
    }
    // CONSTR-PAT
    else if (pat.head().to<Hs::Con>())
    {
        auto sub_pats = pat.copy_sub();

        for(auto& sub_pat: sub_pats)
            sub_pat = rename_pattern_from_bindinfo(sub_pat, bind_info);

        Hs::Pattern pat2 = pat.head();
        if (pat.size())
            pat2 = expression_ref(pat.head(), sub_pats);

        return pat2;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto p1 = rename_pattern_from_bindinfo(ap->pattern, bind_info);

        auto v2 = rename_var_pattern_from_bindinfo(ap->var.as_<Hs::Var>(), bind_info);

        return Hs::AsPattern(v2, p1);
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        return Hs::LazyPattern( rename_pattern_from_bindinfo(lp->pattern, bind_info) );
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        return Hs::StrictPattern( rename_pattern_from_bindinfo(sp->pattern, bind_info) );
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        return pat;
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        for(auto& element: L.elements)
            element = rename_pattern_from_bindinfo(element, bind_info);

        return L;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        auto T = *t;
        for(auto& element: T.elements)
            element = rename_pattern_from_bindinfo(element, bind_info);

        return T;
    }
    // ???
    else if (pat.is_int())
    {
        return pat;
    }
    else if (pat.is_double())
    {
        return pat;
    }
    else if (pat.is_char())
    {
        return pat;
    }
    else if (false) // Literal string
    {
        return pat;
    }
    else if (pat.is_a<Hs::Literal>())
    {
        return pat;
    }
    else if (pat.is_log_double())
        throw myexception()<<"log_double literal should be impossible: '"<<pat<<"'!";
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}


