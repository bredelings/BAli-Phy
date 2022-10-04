#include "typecheck.H"
#include "kindcheck.H"

using std::tuple;
using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

tuple<vector<Hs::TypeVar>, vector<Hs::TypeVar>, LIE, vector<Hs::Type>, Hs::Type> typechecker_state::constructor_pattern_types(const Hs::Con& con)
{
    // Should we be instantiating here?
    auto full_con_type = constructor_type(con);
    auto [mtvs, predicates, con_type] = instantiate( full_con_type );

    vector<Hs::Type> field_types;

    while(auto f = Hs::is_function_type(con_type))
    {
        auto [t1,t2] = *f;
        field_types.push_back(t1);
        con_type = t2;
    }
    auto result_type = con_type;

    vector<Hs::TypeVar> u_tvs;
    vector<Hs::TypeVar> ex_tvs;

    return {u_tvs, ex_tvs, predicates, field_types, result_type};
}

// Ensure that we can convert exp_type to pat_type, and get a wrapper proving it.
Core::wrapper typechecker_state::instPatSigma(const Hs::SigmaType& pat_type, const Expected& exp_type)
{
    if (exp_type.infer())
    {
        exp_type.infer_type( pat_type );
        return Core::wrapper_id;
    }
    else
        return subsumptionCheck( exp_type.check_type(), pat_type);
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

void typechecker_state::tcPat(local_value_env& penv, Hs::Var& V, const Expected& exp_type, const signature_env& sigs, const tc_action<local_value_env&>& a)
{
    auto& name = unloc(V.name);
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
            exp_type.infer_type( type );
        }
        else
        {
            // Add flag to check that the expected type doesn't have any non-entailed wanteds!
            V.wrap = subsumptionCheck( exp_type.check_type(), sig_type );
        }
    }
    else
    {
        if (exp_type.infer())
            type = inferResultToType(exp_type);
        else
            type = exp_type.check_type();
    }

    V.type = type;
    local_value_env lve;
    lve = lve.insert({name,type});
    penv += lve;

    a(penv, *this);
}

void typechecker_state::checkPat(local_value_env& penv, Hs::Var& v, const Hs::SigmaType& exp_type, const signature_env& sigs)
{
    return tcPat(penv, v, Check(exp_type), sigs, [](local_value_env&, typechecker_state&){});
}

Hs::Type typechecker_state::inferPat(local_value_env& penv, Hs::Var& V, const map<string, Hs::Type>& sigs)
{
    Expected exp_type = Infer();
    tcPat(penv, V, exp_type, sigs, [](local_value_env&, typechecker_state&){});
    return exp_type.read_type();
}

void typechecker_state::tcPats(local_value_env& penv,
                               vector<Hs::Pattern>& pats, const vector<Expected>& pat_types,
                               const signature_env& sigs,
                               const std::function<void(local_value_env&,typechecker_state&)>& a,
                               int i)
{

    if (i < pats.size())
    {
        tcPat(penv, pats[i], pat_types[i], sigs, [&,i=i+1](local_value_env& penv, typechecker_state& tc){
            auto tc2 = tc.copy_clear_wanteds();
            tc2.add_binders(penv);
            tc2.tcPats(penv, pats, pat_types, sigs, a, i);
            tc.current_wanteds() += tc2.current_wanteds();
        });
    }
    else
    {
        a(penv, *this);
    }
}

// Figure 24. Rules for patterns
void typechecker_state::tcPat(local_value_env& penv, Hs::Pattern& pat, const Expected& exp_type, const map<string, Hs::Type>& sigs, const tc_action<local_value_env&>& a)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::VarPattern>())
    {
        auto V = *v;
        tcPat(penv, V.var, exp_type, sigs, a);
        pat = V;
    }
    // CONSTR-PAT
    else if (auto con = pat.to<Hs::ConPattern>())
    {
        // See GHC/Core/DataCon.hs
        // See GHC/Tc/Gen/Pat.hs > tcDataConPat
        // See GHC/Core/DataCon.hs > mkData
        auto Con = *con;

        // maybe also get type tvs, constructor tvs
        auto [givens, field_types, type] = constructor_pattern_types(Con.head);

        vector<Expected> arg_types;
        for(auto& field_type: field_types)
            arg_types.push_back(Check(field_type));

        assert(field_types.size() == Con.args.size());
        Con.givens = givens;

        auto tc2 = copy_clear_wanteds();
        tc2.tcPats(penv, Con.args, arg_types, sigs, a);
        if (givens.empty())
            current_wanteds() += tc2.current_wanteds();
        else
        {
            vector<Hs::TypeVar> tvs;
            current_wanteds().implications.push_back( std::make_shared<Implication>(tvs, givens, tc2.current_wanteds()) );
        }

        unify( expTypeToType(exp_type), type );

        pat = Con;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto Ap = *ap;
        tcPat(penv, Ap.var, exp_type, sigs, [&](local_value_env& penv, typechecker_state& tc) {
            tc.tcPat(penv, Ap.pattern, exp_type, sigs, a);});
        pat = Ap;
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto Lp = *lp;
        tcPat(penv, Lp.pattern, exp_type, sigs, a);
        pat = Lp;
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto Sp = *sp;
        tcPat(penv, Sp.pattern, exp_type, sigs, a);
        pat = Sp;
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        expTypeToType(exp_type);

        a(penv, *this);
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::ListPattern>())
    {
        auto L = *l;

        auto pat_type = expTypeToType(exp_type);

        // This should be able to extract forall a.a->a from [forall a. a-> a]
        Hs::Type element_type;
        if (auto elem_type = is_list_type( pat_type ))
            element_type = *elem_type;
        else
        {
            element_type = fresh_meta_type_var( kind_star() );
            unify( pat_type, Hs::ListType(element_type) );
        }

        tcPats(penv, L.elements, vector<Expected>(L.elements.size(), Check(element_type)), sigs, a);

        pat = L;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::TuplePattern>())
    {
        auto T = *t;

        auto pat_type = expTypeToType(exp_type);

        vector<Hs::Type> element_types;
        if (auto elem_types = is_tuple_type( pat_type ))
            element_types = *elem_types;
        else
        {
            for(int i=0;i<T.elements.size();i++)
                element_types.push_back( fresh_meta_type_var( kind_star() ) );
            unify( pat_type, Hs::TupleType(element_types) );
        }

        tcPats(penv, T.elements, check_types(element_types), sigs, a);

        pat = T;
    }
    // case (x :: exp_type) of (pat :: type) -> E
    else if (auto tp = pat.to<Hs::TypedPattern>())
    {
        auto TP = *tp;
        TP.type = check_type(TP.type);
        tcPat(penv, TP.pat, Check(TP.type), sigs, a);
        TP.wrap = instPatSigma(TP.type, exp_type);
    }
    else if (auto l = pat.to<Hs::LiteralPattern>())
    {
        auto L = *l;

        if (L.lit.is_BoxedInteger())
        {
            unify( expTypeToType(exp_type), int_type() );
            a(penv, *this);
            return;
        }

        // 1. Typecheck (==)
//        auto [equals, equals_type] = inferRho(gve, Hs::Var({noloc,"Data.Eq.=="}));
//        L.equalsOp = equals;

        if (L.lit.is_Char())
        {
            unify( expTypeToType(exp_type), char_type() );
            a(penv, *this);
            return;
        }
        else if (auto i = L.lit.is_Integer())
        {
            expression_ref fromInteger = Hs::Var({noloc,"Compiler.Num.fromInteger"});
            auto [arg_type, result_type] = unify_function( inferRho(fromInteger) );
            unify(arg_type, int_type());
            unify(result_type, expTypeToType(exp_type));

            L.lit.literal = Hs::Integer(*i, fromInteger);
            pat = L;

            a(penv, *this);
            return;
        }
        else if (L.lit.is_String())
        {
            unify(expTypeToType(exp_type), Hs::ListType(char_type()) );
            return;
        }
        else if (auto d = L.lit.is_Double())
        {
            expression_ref fromRational = Hs::Var({noloc,"Compiler.Real.fromRational"});
            auto [arg_type, result_type] = unify_function( inferRho(fromRational) );
            unify(arg_type, double_type());
            unify(result_type, expTypeToType(exp_type));

            L.lit.literal = Hs::Double(*d, fromRational);
            pat = L;

            a(penv, *this);
            return;
        }
        else
            std::abort();
    }
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

Hs::Type typechecker_state::inferPat(local_value_env& penv, Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
{
    Expected exp_type = Infer();
    tcPat(penv, pat, exp_type, sigs, [](local_value_env&, typechecker_state&) {});
    return exp_type.read_type();
}

void typechecker_state::checkPat(local_value_env& penv, Hs::Pattern& pat, const Hs::SigmaType& exp_type, const signature_env& sigs)
{
    tcPat(penv, pat, Check(exp_type), sigs, [](local_value_env&, typechecker_state&) {});
}


Hs::Var
rename_var_from_bindinfo(const Hs::Var& v, const map<string, Hs::BindInfo>& bind_info_for_ids)
{
    auto& name = unloc(v.name);

    // QUESTION: if there is a wrapper on the outer id, is it already on the inner id?
    // Perhaps any such wrapper should be part of the bindinfo and go from the inner id to the outer id?
    // Should there be a separate wrapper for the inner id?

    return bind_info_for_ids.at(name).inner_id;
}

// Figure 24. Rules for patterns
Hs::Pattern
rename_pattern_from_bindinfo(const Hs::Pattern& pat, const map<string, Hs::BindInfo>& bind_info)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::VarPattern>())
    {
        auto VP = *v;
        VP.var = rename_var_from_bindinfo(VP.var, bind_info);
        return VP;
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

        auto v2 = rename_var_from_bindinfo(ap->var, bind_info);

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
    else if (auto tpat = pat.to<Hs::TypedPattern>())
    {
        auto TPat = *tpat;

        TPat.pat = rename_pattern_from_bindinfo(TPat.pat, bind_info);

        return TPat;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::TuplePattern>())
    {
        auto T = *t;
        for(auto& element: T.elements)
            element = rename_pattern_from_bindinfo(element, bind_info);

        return T;
    }
    else if (pat.is_a<Hs::LiteralPattern>())
        return pat;
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}


