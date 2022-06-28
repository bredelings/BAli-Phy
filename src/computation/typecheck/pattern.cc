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
typechecker_state::checkVarPat(Hs::Var& v, const Hs::SigmaType& exp_type, const signature_env& sigs)
{
    auto [type, env] = infer_var_pattern_type(v, sigs);
    unify(type, exp_type);
    return env;
}


// OK, so if we have a signature x :: sigma1 and we do checkPat(x, sigma2)
// then sigma2 is the type of the case object, and sigma1 is the type of a
// binder that we create from the case object.
//
// So, it should be equivalent to
//- tcPat(x :: sigma1, Check(sigma2)) = checkPat x sigma1
//                                      instantiatePatSigma sigma1 Check(sigma2)
//
//                                    = wrapper <- subsCheck sigma2 sigma1
//                                      return (wrapper, {x :: sigma1})
//

local_value_env
typechecker_state::tcVarPat(Hs::Var& V, const Expected& exp_type, const signature_env& sigs)
{
    auto& name = unloc(V.name);
    local_value_env lve;
    Hs::Type type;

    if (sigs.count(name))
    {
        // We can only have signatures for pattern binders in a let-context, not a lambda context.
        auto sig_type = sigs.at(name);
        if (exp_type.infer())
        {
            auto [tvs, wanteds, monotype] = instantiate(sig_type);
            if (wanteds.size())
                throw myexception()<<"variable '"<<name<<"' cannot have constrained type '"<<sig_type<<"' due to monomorphism restriction";
            type = monotype;
        }
        else
        {
            // Add flat to check that the expected type doesn't have any non-entailed wanteds!
            V.wrap = subsumptionCheck( exp_type.check_type(), sig_type );
        }
    }
    else
    {
        if (exp_type.infer())
            type = fresh_meta_type_var( kind_star() );
        else
            type = exp_type.check_type();
    }

    if (exp_type.infer())
        exp_type.infer_type( type );
    
    V.type = type;
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
    auto [type, env] = inferPat(pat, sigs);
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

        assert(field_types.size() == Con.args.size());

        local_value_env lve;
        for(int i=0; i < field_types.size(); i++)
            lve += checkPat(Con.args[i], field_types[i]);
        pat = Con;

        exp_type.infer_type( type );
        return lve;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto Ap = *ap;

        auto type = expTypeToType(exp_type);
        auto lve1 = checkPat(Ap.pattern, type, sigs);
        auto lve2 = checkVarPat(Ap.var, type, sigs);
        pat = Ap;

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
        expTypeToType(exp_type);
        return {};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::ListPattern>())
    {
        auto L = *l;

        local_value_env lve;
        // We need some way of getting the elemement type that is able to extract
        // forall a.a->a from [forall a. a-> a]
        Hs::Type element_type = fresh_meta_type_var( kind_star() );
        for(auto& element: L.elements)
            lve += checkPat(element, element_type, sigs);

        pat = L;
        exp_type.infer_type( Hs::ListType(element_type) );
        return lve;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::TuplePattern>())
    {
        auto T = *t;
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: T.elements)
        {
            auto [t1, lve1] = inferPat(element, sigs);
            types.push_back(t1);
            lve += lve1;
        }
        pat = T;
        exp_type.infer_type( Hs::TupleType(types) );
        return lve;
    }
    else if (auto l = pat.to<Hs::Literal>())
    {
        auto L = *l;

        if (L.is_BoxedInteger())
        {
            exp_type.infer_type( int_type() );
            return {};
        }

        // 1. Typecheck (==)
//        auto [equals, equals_type] = inferRho(gve, Hs::Var({noloc,"Data.Eq.=="}));
//        L.equalsOp = equals;

        if (L.is_Char())
        {
            exp_type.infer_type( char_type() );
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
            exp_type.infer_type( result_type );
            return {};
        }
        else if (L.is_String())
        {
            exp_type.infer_type( Hs::ListType(char_type()) );
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
            exp_type.infer_type( result_type );
            return {};
        }
        else
            std::abort();
    }
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

tuple<Hs::Type, local_value_env>
typechecker_state::inferPat(Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
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
    else if (auto l = pat.to<Hs::ListPattern>())
    {
        auto L = *l;

        for(auto& element: L.elements)
            element = rename_pattern_from_bindinfo(element, bind_info);

        return L;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::TuplePattern>())
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


