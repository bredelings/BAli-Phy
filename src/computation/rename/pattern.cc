#include <string>
#include <vector>
#include <set>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::optional;
using std::pair;
using std::set;


// The issue here is to rewrite @ f x y -> f x y
// so that f is actually the head.
Hs::LPat unapply(Hs::LExp LE)
{
    auto& E = unloc(LE);

    Hs::LPat LP;
    LP.loc = LE.loc;
    auto& P = unloc(LP);

    if (auto l = E.to<Hs::List>())
    {
        Hs::ListPattern LP;
        for(auto& pattern: l->elements)
            LP.elements.push_back( unapply(pattern) );
        P = LP;
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        Hs::TuplePattern TP;
        for(auto& pattern: t->elements)
            TP.elements.push_back( unapply(pattern) );
        P = TP;
    }
    else if (auto ap = E.to<Hs::AsPattern>())
    {
        P = Hs::AsPattern(ap->var, unapply(ap->pattern));
    }
    else if (auto lp = E.to<Hs::LazyPattern>())
    {
        P = Hs::LazyPattern(unapply(lp->pattern));
    }
    else if (E.is_a<Hs::StrictPattern>())
    {
        auto SP = E.as_<Hs::StrictPattern>();
        SP.pattern = unapply(SP.pattern);
        P = SP;
    }
    else if (E.is_a<Hs::ApplyExp>())
    {
        auto [head, args] = Hs::decompose_apps(LE);

        // We shouldn't have e.g. (@ (@ f x) y) -- this should already be dealt with by rename_infix
        auto con = unloc(head).to<Hs::Con>();
        if (not con)
            throw myexception()<<"In pattern `"<<E<<"`:\n    `"<<head<<"` is not a data constructor.";

        Hs::LPats pat_args;
        for(auto& arg: args)
            pat_args.push_back(unapply(arg));

        P = Hs::ConPattern(*con, pat_args);
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        Hs::TypedPattern TP;
        TP.pat = unapply(texp->exp);
        TP.type = texp->type;
        P = TP;
    }
    else if (auto l = E.to<Hs::Literal>())
        P = Hs::LiteralPattern(*l);
    else if (auto c = E.to<Hs::Con>())
        P = Hs::ConPattern(*c, {});
    else if (auto v = E.to<Hs::Var>())
        P = Hs::VarPattern(*v);
    else if (E.is_a<Hs::WildcardPattern>())
        P = Hs::WildcardPattern();
    else
        std::abort();
    return LP;
}

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
        return find_vars_in_pattern(tpat->pat);
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
        auto id = v->var.name;

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
        auto id = c->head.name;

        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            auto S = m.lookup_symbol(id);
            if (S->symbol_type != constructor_symbol)
                error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

            // FIXME -- we really want the location of the whole pattern here
            if (*S->arity != c->args.size())
                error(loc, Note()<<"Constructor '"<<id<<"' arity "<<*S->arity<<" doesn't match pattern '"<<pat<<"'!");
        }

        // 11. Return the variables bound
        return find_vars_in_patterns(c->args, top);
    }
    else if (pat.is_a<Hs::LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

bound_var_info renamer_state::rename_var_pattern(const optional<yy::location>& loc, Hs::Var& V, bool top)
{
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
        TPat.type = rename_type(TPat.type);
        pat = TPat;
        return bound;
    }

    // 3. Handle x@pat
    if (pat.is_a<Hs::AsPattern>())
    {
        auto AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = rename_var_pattern(loc, AP.var, false);
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
        auto bound = rename_var_pattern(loc, V.var, top);
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

        auto id = C.head.name;

        // 7. Resolve constructor name if identifier is a constructor
        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            auto S = m.lookup_symbol(id);
            assert(S);
            if (S->symbol_type != constructor_symbol)
                error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

            // FIXME -- we really want the location of the whole pattern here
            if (*S->arity != c->args.size())
                error(loc, Note()<<"Constructor '"<<id<<"' arity "<<*S->arity<<" doesn't match pattern '"<<pat<<"'!");

            C.head.name = S->name;
            C.head.arity = *S->arity;

            // 10. Construct the renamed pattern
            pat = C;
        }

        // 11. Return the variables bound
        return bound;
    }
    // 4. Handle literal values
    else if (pat.is_a<Hs::LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

