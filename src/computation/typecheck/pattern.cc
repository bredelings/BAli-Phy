#include "typecheck.H"
#include "kindcheck.H"

using std::tuple;
using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

// Figure 24. Rules for patterns
tuple<Hs::Pattern, Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        auto V = *v;
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
        lve = lve.insert({name, type});
	return { V, type , lve };
    }
    // CONSTR-PAT
    else if (auto con = pat.head().to<Hs::Con>())
    {
        local_value_env lve;
        local_instance_env lie;
        vector<Hs::Type> types;
        auto pats = pat.copy_sub();

        for(auto& pat: pats)
        {
            auto [p, t1, lve1] = infer_pattern_type(pat, sigs);
            pat = p;
            types.push_back(t1);
            lve += lve1;
        }
        substitution_t s;
        auto [type,field_types] = constr_types(*con);

        assert(field_types.size() == pat.size());

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
            unify(types[i], field_types[i]);

        return { pat, type, lve };
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto [pat, t, lve] = infer_pattern_type(ap->pattern, sigs);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {pat, t, lve};
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
    // ???
    else if (pat.is_int())
    {
        auto [dvar, type] = fresh_num_type();
        return {pat, type, {}};
    }
    else if (pat.is_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        return {pat, type, {}};
    }
    else if (pat.is_char())
    {
        return {pat, char_type(), {}};
    }
    else if (false) // Literal string
    {
        return {pat, Hs::ListType(char_type()), {}};
    }
    else if (pat.is_log_double())
        throw myexception()<<"log_double literatal should be impossible: '"<<pat<<"'!";
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}


