#include <vector>
#include <set>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::vector;
using std::set;

bound_var_info renamer_state::rename_patterns(Hs::LPats& patterns, bool top)
{
    bound_var_info bound;

    // Rename the arguments
    for(auto& e: patterns)
    {
	auto bound_here =  rename_pattern(e, top);
        auto overlap = intersection(bound, bound_here);
        if (not overlap.empty())
        {
            auto name = *overlap.begin();
            error(Note()<<"Pattern uses variable '"<<name<<"' twice!");
        }
	add(bound, bound_here);
    }

    return bound;
}

// FIXME - can we just call rename_pattern on this directly???
// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::find_vars_in_pattern(const Hs::LPat& lpat, bool top)
{
    auto& [loc,pat] = lpat;

    assert(not is_apply_exp(pat));
    assert(not pat.is_a<Hs::ApplyExp>());

    // 1. Handle _
    if (pat.is_a<Hs::WildcardPattern>())
	return {};

    // 2. Handle ~pat or !pat
    else if (pat.is_a<Hs::LazyPattern>())
    {
        auto LP = pat.as_<Hs::LazyPattern>();
        return find_vars_in_pattern(LP.pattern, top);
    }
    else if (pat.is_a<Hs::StrictPattern>())
    {
        auto SP = pat.as_<Hs::StrictPattern>();
        return find_vars_in_pattern(SP.pattern, top);
    }
    else if (auto tpat = pat.to<Hs::TypedPattern>())
    {
        return find_vars_in_pattern(tpat->pat, top);
    }
    // 3. Handle x@pat
    else if (pat.is_a<Hs::AsPattern>())
    {
        auto& AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = find_vars_in_pattern({noloc,Hs::VarPattern(AP.var)}, top);
	bool overlap = not disjoint_add(bound, find_vars_in_pattern(AP.pattern, top));

	if (overlap)
	    error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");
	return bound;
    }

    else if (pat.is_a<Hs::ListPattern>())
    {
        auto& L = pat.as_<Hs::ListPattern>();
        return find_vars_in_patterns(L.elements, top);
    }
    else if (pat.is_a<Hs::TuplePattern>())
    {
        auto& T = pat.as_<Hs::TuplePattern>();
        return find_vars_in_patterns(T.elements, top);
    }
    else if (auto v = pat.to<Hs::VarPattern>())
    {
        auto id = unloc(v->var).name;

	if (is_qualified_symbol(id))
        {
            error(lpat.loc, Note()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!");
            return {};
        }

	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	return {id};
    }
    // If its a constructor pattern!
    else if (auto c = pat.to<Hs::ConPattern>())
    {
        auto id = unloc(c->head).name;

        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            try
            {
                auto S = m.lookup_symbol(id);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

                // FIXME -- we really want the location of the whole pattern here
                if (*S->arity != c->args.size())
                    error(loc, Note()<<"Pattern has "<<c->args.size()<<" fields, but should have "<<*S->arity<<"!");
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }

        // 11. Return the variables bound
        return find_vars_in_patterns(c->args, top);
    }
    else if (auto i = pat.to<Hs::InfixPat>())
    {
        Hs::LPats operands;
        for(auto& term: i->terms)
            if (not unloc(term).is_a<Hs::Var>() and not unloc(term).is_a<Hs::Con>() and not unloc(term).is_a<Hs::Neg>())
                operands.push_back(term);
        return find_vars_in_patterns(operands, top);
    }
    else if (auto r = pat.to<Hs::RecordPattern>())
    {
        if (unloc(r->fbinds).dotdot)
            error(lpat.loc, Note()<<"Record wildcards in patterns are not implemented yet.");

        Hs::LPats field_patterns;
        for(const auto& field: unloc(r->fbinds))
            field_patterns.push_back(unloc(field).pattern);

        return find_vars_in_patterns(field_patterns, top);
    }
    else if (pat.is_a<Hs::LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

bound_var_info renamer_state::rename_var_pattern(Hs::LVar& LV, bool top)
{
    auto& [loc,V] = LV;
    auto id = V.name;

    if (is_qualified_symbol(id))
        error(loc, Note() << "Binder variable '"<<id<<"' is qualified in pattern '"<<V.print()<<"'!");
    // Qualify the id if this is part of a top-level decl
    if (top)
        id = m.name + "." + id;
    else
    {
        // FIXME - since we are creating an ID here, we should give it a unique id!
    }
    V.name = id;

    return {id};
}

// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::rename_pattern(Hs::LPat& lpat, bool top)
{
    auto& [loc,pat] = lpat;

    assert(not is_apply_exp(pat));
    assert(not pat.is_a<Hs::ApplyExp>());

    // 1. Handle _
    if (pat.is_a<Hs::WildcardPattern>())
	return {};

    // 2. Handle ~pat
    if (pat.is_a<Hs::LazyPattern>())
    {
        auto LP = pat.as_<Hs::LazyPattern>();
	auto bound = rename_pattern(LP.pattern, top);
	pat = LP;
	return bound;
    }
    else if (pat.is_a<Hs::StrictPattern>())
    {
        auto SP = pat.as_<Hs::StrictPattern>();
	auto bound = rename_pattern(SP.pattern, top);
	pat = SP;
	return bound;
    }
    else if (auto tpat = pat.to<Hs::TypedPattern>())
    {
        auto TPat = *tpat;
        auto bound = rename_pattern(TPat.pat, top);
        TPat.type = rename_and_quantify_type(TPat.type);
        pat = TPat;
        return bound;
    }

    // 3. Handle x@pat
    if (pat.is_a<Hs::AsPattern>())
    {
        auto AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = rename_var_pattern(AP.var, false);
	bool overlap = not disjoint_add(bound, rename_pattern(AP.pattern, false));
        if (overlap)
            error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");

	pat = AP;

	return bound;
    }
    
    //4. Handle List pattern.
    if (pat.is_a<Hs::ListPattern>())
    {
        auto L = pat.as_<Hs::ListPattern>();
        auto bound = rename_patterns(L.elements,top);
        pat = L;
        return bound;
    }
    //5. Handle List pattern.
    else if (pat.is_a<Hs::TuplePattern>())
    {
        auto T = pat.as_<Hs::TuplePattern>();
        auto bound = rename_patterns(T.elements,top);
        pat = T;
        return bound;
    }
    else if (auto v = pat.to<Hs::VarPattern>())
    {
        auto V = *v;
        auto bound = rename_var_pattern(V.var, top);
        pat = V;
	return bound;
    }
    else if (auto c = pat.to<Hs::ConPattern>())
    {
        auto C = *c;

        // 8. Rename arguments and accumulate bound variables
        vector<expression_ref> args = pat.copy_sub();

        bound_var_info bound;
        // Rename the arguments
        bool overlap = false;
        for(auto& e: C.args)
        {
            auto bound_here =  rename_pattern(e, top);
            overlap = overlap or not disjoint_add(bound, bound_here);
        }

        if (overlap)
            error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");

        auto id = unloc(C.head).name;

        // 7. Resolve constructor name if identifier is a constructor
        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            try
            {
                auto S = m.lookup_symbol(id);
                assert(S);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

                // FIXME -- we really want the location of the whole pattern here
                if (*S->arity != c->args.size())
                    error(loc, Note()<<"Pattern has "<<c->args.size()<<" fields, but should have "<<*S->arity<<"!");

                unloc(C.head).name = S->name;
                unloc(C.head).arity = *S->arity;

                // 10. Construct the renamed pattern
                pat = C;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }

        // 11. Return the variables bound
        return bound;
    }
    else if (auto i = pat.to<Hs::InfixPat>())
    {
        auto I = *i;
        bool has_bad_operator = false;
        for(int n=1; n<I.terms.size(); n += 2)
        {
            const auto& op = I.terms[n];
            if (auto v = unloc(op).to<Hs::Var>())
            {
                error(op.loc, Note()<<"Variable operator '"<<v->name<<"' is not valid in a pattern.");
                has_bad_operator = true;
            }
        }

        if (has_bad_operator)
        {
            bound_var_info bound;
            bool overlap = false;
            for(auto& term: I.terms)
            {
                if (unloc(term).is_a<Hs::Var>() or unloc(term).is_a<Hs::Con>() or unloc(term).is_a<Hs::Neg>())
                    continue;

                auto bound_here = rename_pattern(term, top);
                overlap = overlap or not disjoint_add(bound, bound_here);
            }
            if (overlap)
                error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");
            pat = Hs::WildcardPattern();
            return bound;
        }

        try
        {
            lpat = desugar_pattern_infix(*this, I.terms);
            return rename_pattern(lpat, top);
        }
        catch (myexception& e)
        {
            error(loc, Note()<<e.what());
            pat = Hs::WildcardPattern();
            return {};
        }
    }
    else if (auto r = pat.to<Hs::RecordPattern>())
    {
        auto R = *r;

        if (unloc(R.fbinds).dotdot)
            error(lpat.loc, Note()<<"Record wildcards in patterns are not implemented yet.");

        bound_var_info bound;
        bool overlap = false;
        for(auto& field: unloc(R.fbinds))
        {
            auto bound_here = rename_pattern(unloc(field).pattern, top);
            overlap = overlap or not disjoint_add(bound, bound_here);
        }

        if (overlap)
            error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");

        auto id = unloc(R.head).name;

        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            try
            {
                auto S = m.lookup_symbol(id);
                assert(S);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

                unloc(R.head).name = S->name;
                unloc(R.head).arity = *S->arity;
                pat = R;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }

        return bound;
    }
    // 4. Handle literal values
    else if (pat.is_a<Hs::LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}
