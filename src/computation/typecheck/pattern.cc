#include "typecheck.H"
#include "kindcheck.H"

using std::tuple;
using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

local_value_env
typechecker_state::tcVarPat(Hs::Var& V, const Expected& exp_type, const map<string, Hs::Type>& sigs)
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
    exp_type.infer_type() = type;
    lve = lve.insert({name,type});
    return lve;
}

tuple<Hs::Type, local_value_env>
typechecker_state::infer_var_pattern_type(Hs::Var& V, const map<string, Hs::Type>& sigs)
{
    Hs::SigmaType type;
    auto gve = tcVarPat(V, Infer(type), sigs);
    return {type, gve};
}

local_value_env
typechecker_state::checkPat(Hs::Pattern& pat, const Hs::SigmaType& exp_type, const map<string, Hs::Type>& sigs)
{
    auto [type, env] = infer_pattern_type(pat, sigs);
    unify(type, exp_type);
    return env;
}


// Figure 24. Rules for patterns
local_value_env
typechecker_state::tcPat(Hs::Pattern& pat, const Expected& exp_type, const map<string, Hs::Type>& sigs)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        auto V = *v;
        auto lve = tcVarPat(V, exp_type, sigs);
        pat = V;
        return lve;
    }
    // CONSTR-PAT
    else if (auto con = pat.to<Hs::ConPattern>())
    {
        auto Con = *con;
        auto [type,field_types] = constructor_pattern_types(Con.head);

        local_value_env lve;
        vector<Hs::Type> types;

        assert(field_types.size() == Con.args.size());

        for(auto& sub_pat: Con.args)
        {
            auto [t1, lve1] = infer_pattern_type(sub_pat, sigs);
            types.push_back(t1);
            lve += lve1;
        }
        pat = Con;

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
            unify(types[i], field_types[i]);

        exp_type.infer_type() = type;
        return lve;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto Ap = *ap;

        auto [t1, lve1] = infer_pattern_type(Ap.pattern, sigs);

        auto [t2, lve2] = infer_var_pattern_type(Ap.var, sigs);

        unify(t1, t2);
        pat = Ap;
        exp_type.infer_type() = t1;
        return lve1 + lve2;
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto Lp = *lp;
        auto lve = tcPat(Lp.pattern, exp_type, sigs);
        pat = Lp;
        return lve;
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto Sp = *sp;
        auto lve = tcPat(Sp.pattern, exp_type, sigs);
        pat = Sp;
        return lve;
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_meta_type_var( kind_star() );
        exp_type.infer_type() = tv;
        return {};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        local_value_env lve;
        Hs::Type element_type = fresh_meta_type_var( kind_star() );
        for(auto& element: L.elements)
            lve += checkPat(element, element_type, sigs);

        pat = L;
        exp_type.infer_type() = Hs::ListType(element_type);
        return lve;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        auto T = *t;
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: T.elements)
        {
            auto [t1, lve1] = infer_pattern_type(element, sigs);
            types.push_back(t1);
            lve += lve1;
        }
        pat = T;
        exp_type.infer_type() = Hs::TupleType(types);
        return lve;
    }
    else if (auto l = pat.to<Hs::Literal>())
    {
        auto L = *l;

        if (L.is_BoxedInteger())
        {
            exp_type.infer_type() = int_type();
            return {};
        }

        // 1. Typecheck (==)
//        auto [equals, equals_type] = inferRho(gve, Hs::Var({noloc,"Data.Eq.=="}));
//        L.equalsOp = equals;

        if (L.is_Char())
        {
            exp_type.infer_type() = char_type();
            return {};
        }
        else if (auto i = L.is_Integer())
        {
            // 1. Typecheck fromInteger
            expression_ref fromInteger = Hs::Var({noloc,"Compiler.Num.fromInteger"});
            auto fromInteger_type = inferRho(fromInteger);

            // 2. Determine result type
            auto result_type = fresh_meta_type_var( kind_star() );
            unify(fromInteger_type, Hs::make_arrow_type(int_type(), result_type));

            L.literal = Hs::Integer(*i, fromInteger);

            pat = L;
            exp_type.infer_type() = result_type;
            return {};
        }
        else if (L.is_String())
        {
            exp_type.infer_type() = Hs::ListType(char_type());
            return {};
        }
        else if (auto d = L.is_Double())
        {
            // 1. Typecheck fromRational
            expression_ref fromRational = Hs::Var({noloc,"Compiler.Num.fromRational"});
            auto fromRational_type = inferRho(fromRational);

            // 2. Determine result type
            auto result_type = fresh_meta_type_var( kind_star() );
            unify(fromRational_type, Hs::make_arrow_type(double_type(), result_type));

            L.literal = Hs::Double(*d, fromRational);
            pat = L;
            exp_type.infer_type() = result_type;
            return {};
        }
        else
            std::abort();
    }
    else if (pat.is_log_double())
        throw myexception()<<"log_double literal should be impossible: '"<<pat<<"'!";
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

tuple<Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
{
    Hs::SigmaType type;
    auto gve = tcPat(pat, Infer(type), sigs);
    return {type, gve};
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
    else if (auto con = pat.to<Hs::ConPattern>())
    {
        auto Con = *con;

        for(auto& sub_pat: Con.args)
            sub_pat = rename_pattern_from_bindinfo(sub_pat, bind_info);

        return Con;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto p1 = rename_pattern_from_bindinfo(ap->pattern, bind_info);

        auto v2 = rename_var_pattern_from_bindinfo(ap->var, bind_info);

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
    else if (pat.is_a<Hs::Literal>())
        return pat;
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}


