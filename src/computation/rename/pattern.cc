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
using std::pair;
using std::set;


// The issue here is to rewrite @ f x y -> f x y
// so that f is actually the head.
expression_ref unapply(expression_ref E)
{
    if (auto l = E.to<Hs::List>())
    {
        Hs::ListPattern LP;
        for(auto& pattern: l->elements)
            LP.elements.push_back( unapply(pattern) );
        return LP;
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        Hs::TuplePattern TP;
        for(auto& pattern: t->elements)
            TP.elements.push_back( unapply(pattern) );
        return TP;
    }
    else if (E.is_a<Hs::AsPattern>())
    {
        auto& AP = E.as_<Hs::AsPattern>();
        return Hs::AsPattern(AP.var, unapply(AP.pattern));
    }
    else if (E.is_a<Hs::LazyPattern>())
    {
        auto LP = E.as_<Hs::LazyPattern>();
        return Hs::LazyPattern(unapply(LP.pattern));
    }
    else if (E.is_a<Hs::StrictPattern>())
    {
        auto SP = E.as_<Hs::StrictPattern>();
        SP.pattern = unapply(SP.pattern);
        return SP;
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;

        flatten(App);

        // We shouldn't have e.g. (@ (@ f x) y) -- this should already be dealt with by rename_infix
        assert(App.head.is_a<Hs::Con>());

        for(auto& arg: App.args)
            arg = unapply(arg);

        return Hs::ConPattern(App.head.as_<Hs::Con>(), std::move(App.args));
    }
    else if (E.is_a<Hs::Literal>())
        return E;
    else if (E.is_a<Hs::Var>())
        return E;
    else if (E.is_a<Hs::WildcardPattern>())
        return E;
    else if (E.is_a<Hs::Con>())
        return Hs::ConPattern(E.as_<Hs::Con>(),{});
    else
        std::abort();
}

bound_var_info renamer_state::rename_patterns(vector<expression_ref>& patterns, bool top)
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
            throw myexception()<<"Pattern uses a variable '"<<name<<"' twice!";
        }
	add(bound, bound_here);
    }

    return bound;
}

// FIXME - can we just call rename_pattern on this directly???
// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::find_vars_in_pattern(const expression_ref& pat, bool top)
{
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

    // 3. Handle x@pat
    else if (pat.is_a<Hs::AsPattern>())
    {
        auto& AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = find_vars_in_pattern(AP.var, top);
	bool overlap = not disjoint_add(bound, find_vars_in_pattern(AP.pattern, top));

	if (overlap)
	    throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";
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
    else if (auto v = pat.to<Hs::Var>())
    {
        auto id = unloc(v->name);

	if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!";

	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	return {id};
    }
    // If its a constructor pattern!
    else if (auto c = pat.to<Hs::ConPattern>())
    {
        auto id = unloc(c->head.name);

        if (not m.is_declared(id))
            throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

        const symbol_info& S = m.lookup_symbol(id);
        if (S.symbol_type != constructor_symbol)
            throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

        if (S.arity != c->args.size())
            throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

        // 11. Return the variables bound
        return find_vars_in_patterns(c->args, top);
    }
    else if (pat.is_a<Hs::Literal>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

bound_var_info renamer_state::rename_var_pattern(Hs::Var& V, bool top)
{
    auto id = unloc(V.name);

    if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<V.print()<<"'!";
    // Qualify the id if this is part of a top-level decl
    if (top)
        id = m.name + "." + id;
    else
    {
        // FIXME - since we are creating an ID here, we should give it a unique id!
    }
    unloc(V.name) = id;

    return {id};
}

// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::rename_pattern(expression_ref& pat, bool top)
{
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

    // 3. Handle x@pat
    if (pat.is_a<Hs::AsPattern>())
    {
        auto AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = rename_var_pattern(AP.var, false);
	bool overlap = not disjoint_add(bound, rename_pattern(AP.pattern, false));
	pat = AP;

	if (overlap)
	    throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";
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
    else if (auto v = pat.to<Hs::Var>())
    {
        auto V = *v;
        auto bound = rename_var_pattern(V);
        pat = V;
	return bound;
    }
    else if (auto c = pat.to<Hs::ConPattern>())
    {
        auto C = *c;

        auto id = unloc(C.head.name);

        // 7. Resolve constructor name if identifier is a constructor
        if (not m.is_declared(id))
            throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

        const symbol_info& S = m.lookup_symbol(id);
        if (S.symbol_type != constructor_symbol)
            throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

        if (S.arity != c->args.size())
            throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

        unloc(C.head.name) = S.name;
        C.head.arity = S.arity;

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
            throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";

        // 10. Construct the renamed pattern
        pat = C;

        // 11. Return the variables bound
        return bound;
    }
    // 4. Handle literal values
    else if (pat.is_a<Hs::Literal>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

