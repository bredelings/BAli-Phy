#include <string>
#include <vector>
#include <set>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;


expression_ref shift_list(vector<expression_ref>& v)
{
    if (not v.size()) return {};

    auto head = v[0];
    for(int i=0;i<v.size()-1;i++)
	v[i] = v[i+1];
    v.pop_back();
    return head;
}

// The issue here is to rewrite @ f x y -> f x y
// so that f is actually the head.
expression_ref unapply(expression_ref E)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& pattern: L.elements)
            pattern = unapply(pattern);
        return L;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& pattern: T.elements)
            pattern = unapply(pattern);
        return T;
    }
    else if (E.is_a<Haskell::AsPattern>())
    {
        auto& AP = E.as_<Haskell::AsPattern>();
        return Haskell::AsPattern(AP.var, unapply(AP.pattern));
    }
    else if (E.is_a<Haskell::LazyPattern>())
    {
        auto LP = E.as_<Haskell::LazyPattern>();
        return Haskell::LazyPattern(unapply(LP.pattern));
    }
    else if (E.is_a<Haskell::StrictPattern>())
    {
        auto SP = E.as_<Haskell::StrictPattern>();
        SP.pattern = unapply(SP.pattern);
        return SP;
    }

    if (not E.size()) return E;

    auto head = E.head();
    auto args = E.sub();
    if (is_apply_exp(E))
	head = shift_list(args);

    // We shouldn't have e.g. (@ (@ f x) y) -- this should already be dealt with by rename_infix
    assert(not is_apply_exp(head));
    assert(not head.size());

    for(auto& arg: args)
	arg = unapply(arg);

    return expression_ref{head,std::move(args)};
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

    // 1. Handle _
    if (pat.is_a<Haskell::WildcardPattern>())
	return {};

    // 2. Handle ~pat or !pat
    if (pat.is_a<Haskell::LazyPattern>())
    {
        auto LP = pat.as_<Haskell::LazyPattern>();
        return find_vars_in_pattern(LP.pattern, top);
    }
    if (pat.is_a<Haskell::StrictPattern>())
    {
        auto SP = pat.as_<Haskell::StrictPattern>();
        return find_vars_in_pattern(SP.pattern, top);
    }

    // 3. Handle x@pat
    if (pat.is_a<Haskell::AsPattern>())
    {
        auto& AP = pat.as_<Haskell::AsPattern>();
	assert(not top);

	auto bound = find_vars_in_pattern(AP.var, top);
	bool overlap = not disjoint_add(bound, find_vars_in_pattern(AP.pattern, top));

	if (overlap)
	    throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";
	return bound;
    }

    if (pat.is_a<Haskell::List>())
    {
        auto& L = pat.as_<Haskell::List>();
        return find_vars_in_patterns(L.elements, top);
    }
    else if (pat.is_a<Haskell::Tuple>())
    {
        auto& T = pat.as_<Haskell::Tuple>();
        return find_vars_in_patterns(T.elements, top);
    }
    else if (auto v = pat.to<Haskell::Var>())
    {
        auto id = unloc(v->name);

	if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!";

	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	return {id};
    }
    // If its a constructor pattern!
    else if (auto c = pat.head().to<Haskell::Con>())
    {
        auto id = unloc(c->name);

        if (not m.is_declared(id))
            throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

        const symbol_info& S = m.lookup_symbol(id);
        if (S.symbol_type != constructor_symbol)
            throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

        if (S.arity != pat.size())
            throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

        // 11. Return the variables bound
        return find_vars_in_patterns(pat.copy_sub(), top);
    }
    else if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double())
        return {};
    else if (pat.is_a<Hs::Literal>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::rename_pattern(expression_ref& pat, bool top)
{
    assert(not is_apply_exp(pat));

    // 1. Handle _
    if (pat.is_a<Haskell::WildcardPattern>())
	return {};

    // 2. Handle ~pat
    if (pat.is_a<Haskell::LazyPattern>())
    {
        auto LP = pat.as_<Haskell::LazyPattern>();
	auto bound = rename_pattern(LP.pattern, top);
	pat = LP;
	return bound;
    }
    else if (pat.is_a<Haskell::StrictPattern>())
    {
        auto SP = pat.as_<Haskell::StrictPattern>();
	auto bound = rename_pattern(SP.pattern, top);
	pat = SP;
	return bound;
    }

    // 3. Handle x@pat
    if (pat.is_a<Haskell::AsPattern>())
    {
        auto AP = pat.as_<Haskell::AsPattern>();
	assert(not top);

	auto bound = rename_pattern(AP.var, false);
	bool overlap = not disjoint_add(bound, rename_pattern(AP.pattern, false));
	pat = AP;

	if (overlap)
	    throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";
	return bound;
    }
    
    //4. Handle List pattern.
    if (pat.is_a<Haskell::List>())
    {
        auto L = pat.as_<Haskell::List>();
        auto bound = rename_patterns(L.elements,top);
        pat = L;
        return bound;
    }
    //5. Handle List pattern.
    else if (pat.is_a<Haskell::Tuple>())
    {
        auto T = pat.as_<Haskell::Tuple>();
        auto bound = rename_patterns(T.elements,top);
        pat = T;
        return bound;
    }
    else if (pat.is_a<Haskell::Var>())
    {
        auto V = pat.as_<Haskell::Var>();
        auto id = unloc(V.name);

	if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!";
	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
        else
        {
            // FIXME - since we are creating an ID here, we should give it a unique id!
        }
	unloc(V.name) = id;

        // We translate to a var( ) here!
        // Maybe we should do this during desugaring instead?
        pat = V;

	return {id};
    }
    else if (pat.head().is_a<Haskell::Con>())
    {
        auto C = pat.head().as_<Haskell::Con>();
        auto id = unloc(C.name);

        // 7. Resolve constructor name if identifier is a constructor
        if (not m.is_declared(id))
            throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

        const symbol_info& S = m.lookup_symbol(id);
        if (S.symbol_type != constructor_symbol)
            throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

        if (S.arity != pat.size())
            throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

        unloc(C.name) = S.name;
        C.arity = S.arity;

        // 8. Rename arguments and accumulate bound variables
        vector<expression_ref> args = pat.copy_sub();

        bound_var_info bound;
        // Rename the arguments
        bool overlap = false;
        for(auto& e: args)
        {
            auto bound_here =  rename_pattern(e, top);
            overlap = overlap or not disjoint_add(bound, bound_here);
        }

        if (overlap)
            throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";

        // 10. Construct the renamed pattern
        if (args.size())
            pat = expression_ref{C,args};
        else
            pat = C;

        // 11. Return the variables bound
        return bound;
    }
    // 4. Handle literal values
    else if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double())
        return {};
    else if (pat.is_a<Hs::Literal>())
    {
        return {};
    }
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

