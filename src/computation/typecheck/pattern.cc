#include "typecheck.H"
#include "kindcheck.H"

using std::tuple;
using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

// Ensure that we can convert exp_type to pat_type, and get a wrapper proving it.
Core::wrapper TypeChecker::instPatSigma(const SigmaType& pat_type, const Expected& exp_type)
{
    if (auto I = exp_type.infer())
    {
        fillInfer( pat_type, *I );
        return Core::WrapId;
    }
    else
        return subsumptionCheck( PatOrigin(), exp_type.check_type(), pat_type);
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

void TypeChecker::tcPat(local_value_env& penv, Hs::LVar& LV, const Expected& exp_type, const signature_env& sigs, const tc_action<local_value_env&>& a)
{
    auto& [loc,V] = LV;
    auto& name = V.name;
    Type type;

    if (sigs.count(V))
    {
        // We can only have signatures for pattern binders in a let-context, not a lambda context.
        auto sig_type = sigs.at(V);
        if (auto I = exp_type.infer())
        {
            auto [tvs, wanteds, monotype] = instantiate( PatOrigin(), sig_type);
            if (wanteds.size())
                throw note_exception()<<"variable '"<<name<<"' cannot have constrained type '"<<sig_type<<"' due to monomorphism restriction";
            type = monotype;
            fillInfer(type, *I);
        }
        else
        {
            // Add flag to check that the expected type doesn't have any non-entailed wanteds!
            V.wrap = subsumptionCheck( PatOrigin(), exp_type.check_type(), sig_type );
        }
    }
    else
    {
        if (auto I = exp_type.infer())
            type = inferResultToType(*I);
        else
            type = exp_type.check_type();
    }

    V.type = type;
    local_value_env lve;
    lve = lve.insert({V,type});
    penv += lve;

    push_binder(IDType{V,type});
    a(penv, *this);
    pop_binder();
}

void TypeChecker::checkPat(local_value_env& penv, Hs::LVar& v, const SigmaType& exp_type, const signature_env& sigs)
{
    return tcPat(penv, v, Check(exp_type), sigs, [](local_value_env&, TypeChecker&){});
}

Type TypeChecker::inferPat(local_value_env& penv, Hs::LVar& V, const signature_env& sigs)
{
    Expected exp_type = newInfer();
    tcPat(penv, V, exp_type, sigs, [](local_value_env&, TypeChecker&){});
    return exp_type.read_type();
}

void TypeChecker::tcPats(local_value_env& penv,
                         Hs::LPats& pats, const vector<Expected>& pat_types,
                         const signature_env& sigs,
                         const std::function<void(local_value_env&,TypeChecker&)>& a,
                         int i)
{

    if (i < pats.size())
    {
        tcPat(penv, pats[i], pat_types[i], sigs, [&,i=i+1](local_value_env& penv, TypeChecker& tc){
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
void TypeChecker::tcPat(local_value_env& penv, Hs::LPat& lpat, const Expected& exp_type, const signature_env& sigs, const tc_action<local_value_env&>& a)
{
    if (lpat.loc) push_source_span(*lpat.loc);

    // TAUT-PAT
    auto& pat = unloc(lpat);
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

        auto info = constructor_info(unloc(Con.head));
        auto constraints = info.written_constraints;
        for(auto& constraint: info.gadt_eq_constraints)
            constraints.push_back(constraint);
        auto field_types = info.field_types;
        auto result_type = info.result_type();

        assert(field_types.size() == Con.args.size());

        substitution_t s;
        for(auto& tv: info.uni_tvs)
            s = s.insert({tv,fresh_meta_type_var(unloc(tv.name), *tv.kind)});

        // These are supposed to be "super" skolems.
        vector<TypeVar> ex_tvs2;
        for(auto& tv: info.exi_tvs)
        {
            auto super_skol_tv = FreshVarSource::fresh_rigid_type_var(level()+1, unloc(tv.name), *tv.kind);
            s = s.insert({tv, super_skol_tv});
            ex_tvs2.push_back(super_skol_tv);
        }

        result_type = apply_subst(s, result_type);
        field_types = apply_subst(s, field_types);
        constraints = apply_subst(s, constraints);

        auto givens = preds_to_constraints(PatOrigin(), Given, constraints);
        Con.universal_tyvars = info.uni_tvs;
        Con.existential_tyvars = ex_tvs2;
        for(auto& constraint: dictionary_constraints(givens))
            Con.given_dict_vars.push_back(constraint.ev_var);
        Con.ev_binds = maybe_implication(ex_tvs2, givens,
                                         [&](auto& tc) {
                                             tc.tcPats(penv, Con.args, check_types(field_types), sigs, a);
                                         });

        unify( expTypeToType(exp_type), result_type );

        pat = Con;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto Ap = *ap;
        tcPat(penv, Ap.var, exp_type, sigs, [&](local_value_env& penv, TypeChecker& tc) {
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

        auto exp_pat_type = expTypeToType(exp_type);

        // This should be able to extract forall a.a->a from [forall a. a-> a]
        Type element_type;
        if (auto elem_type = is_list_type( exp_pat_type ))
            element_type = *elem_type;
        else
        {
            element_type = fresh_meta_type_var( kind_type() );
            unify( exp_pat_type, list_type(element_type) );
        }

        tcPats(penv, L.elements, vector<Expected>(L.elements.size(), Check(element_type)), sigs, a);

        pat = L;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::TuplePattern>())
    {
        auto T = *t;

        auto exp_pat_type = expTypeToType(exp_type);

        vector<Type> element_types;
        // We want to extract forall types from fields of the expected type if its the right sized tuple.
        if (auto elem_types = is_tuple_type( exp_pat_type ); elem_types and elem_types->size() == T.elements.size())
            element_types = *elem_types;
        else
        {
            for(int i=0;i<T.elements.size();i++)
                element_types.push_back( fresh_meta_type_var( kind_type() ) );
            unify( exp_pat_type, tuple_type(element_types) );
        }

        tcPats(penv, T.elements, check_types(element_types), sigs, a);

        pat = T;
    }
    // case (x :: exp_type) of (pat :: type) -> E
    else if (auto tp = pat.to<Hs::TypedPattern>())
    {
        auto TP = *tp;
        auto type = check_type(desugar(TP.type));
        tcPat(penv, TP.pat, Check(type), sigs, a);
        TP.wrap = instPatSigma(type, exp_type);
        // I think we should translate this to case (wrap(x)) of pat -> E
        // Does this undermine the grouping of constructors in desugaring?
        // Does it only happen when pat is a var, so that we have
        //    let pat = wrap(x) in E ?
    }
    else if (auto l = pat.to<Hs::LiteralPattern>())
    {
        auto L = *l;

        // 1. (==) :: Eq a => a -> a -> Bool
        auto [ obj_type, pattern_type, result_type ] = unify_two_arg_function( inferRho(L.equalsOp) );
        unify( obj_type, pattern_type );
        unify( result_type, bool_type() );

        // 2. exp_type ~ a
        unify(pattern_type, expTypeToType(exp_type));

        // 3. a ~ type_of(L.lit)
        tcRho(L.lit, Check(pattern_type) );

        pat = L;

        a(penv, *this);
    }
    else
        throw note_exception()<<"Unrecognized pattern '"<<pat<<"'!";

    if (lpat.loc) pop_source_span();
}

Type TypeChecker::inferPat(local_value_env& penv, Hs::LPat& pat, const signature_env& sigs)
{
    Expected exp_type = newInfer();
    tcPat(penv, pat, exp_type, sigs, [](local_value_env&, TypeChecker&) {});
    return exp_type.read_type();
}

void TypeChecker::checkPat(local_value_env& penv, Hs::LPat& pat, const SigmaType& exp_type, const signature_env& sigs)
{
    tcPat(penv, pat, Check(exp_type), sigs, [](local_value_env&, TypeChecker&) {});
}


Hs::LVar
rename_var_from_bindinfo(Hs::LVar lv, const map<Hs::Var, Hs::BindInfo>& bind_info_for_ids)
{
    auto& [loc,v] = lv;

    // QUESTION: if there is a wrapper on the outer id, is it already on the inner id?
    // Perhaps any such wrapper should be part of the bindinfo and go from the inner id to the outer id?
    // Should there be a separate wrapper for the inner id?

    v = bind_info_for_ids.at(v).inner_id;
    return lv;
}

// Figure 24. Rules for patterns
Hs::LPat
rename_pattern_from_bindinfo(Hs::LPat lpat, const map<Hs::Var, Hs::BindInfo>& bind_info)
{
    auto& pat = unloc(lpat);

    // TAUT-PAT
    if (auto v = pat.to<Hs::VarPattern>())
    {
        auto VP = *v;
        VP.var = rename_var_from_bindinfo(VP.var, bind_info);
        pat = VP;
    }
    // CONSTR-PAT
    else if (auto con = pat.to<Hs::ConPattern>())
    {
        auto Con = *con;

        for(auto& sub_pat: Con.args)
            sub_pat = rename_pattern_from_bindinfo(sub_pat, bind_info);

        pat = Con;
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto p1 = rename_pattern_from_bindinfo(ap->pattern, bind_info);

        auto v2 = rename_var_from_bindinfo(ap->var, bind_info);

        pat = Hs::AsPattern(v2, p1);
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        pat = Hs::LazyPattern( rename_pattern_from_bindinfo(lp->pattern, bind_info) );
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        pat = Hs::StrictPattern( rename_pattern_from_bindinfo(sp->pattern, bind_info) );
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::ListPattern>())
    {
        auto L = *l;

        for(auto& element: L.elements)
            element = rename_pattern_from_bindinfo(element, bind_info);

        pat = L;
    }
    else if (auto tpat = pat.to<Hs::TypedPattern>())
    {
        auto TPat = *tpat;

        TPat.pat = rename_pattern_from_bindinfo(TPat.pat, bind_info);

        pat = TPat;
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::TuplePattern>())
    {
        auto T = *t;
        for(auto& element: T.elements)
            element = rename_pattern_from_bindinfo(element, bind_info);

        pat = T;
    }
    else if (pat.is_a<Hs::LiteralPattern>())
    { }
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";

    return lpat;
}


