#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>
#include <deque>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "computation/expression/tuple.H"
#include "computation/expression/constructor.H"
#include "util/graph.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::map;
using std::deque;

/*
 * We probably want to move away from using dummies to represent patterns.
 * - Dummies can't represent e.g. irrefutable patterns.
 */

// What would be involved in moving the renamer to a kind of phase 2?
// How do we get the exported symbols before we do the desugaring that depends on imports?

// rename_infix does:
// (i) precedence handling for infix expressions
// (ii) rewrites @ f x y -> f x y for decls
// (iii) rewrites @ C x y -> C x y for patterns

// Consider h:t !! y.  This can be h:(t!!y) or (h:t)!!y

// We might have @ (infix x op y) z.  Infix handling will rewrite this to
// @ (@ op x y) z.  We need to change this to (@ op x y z).
// However, if we have @ (: x y) z, then we don't want to rewrite this to (: x y z).
// What are the rules for well-formed patterns?
// Only one op can be a non-constructor (in decl patterns), and that op needs to end up at the top level.

pair<map<string,Hs::Type>, Hs::Decls> group_decls(const Haskell::Decls& decls);


expression_ref rename_infix_decl(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::ValueDecl>())
    {
        auto D = E.as_<Haskell::ValueDecl>();

        unloc(D.lhs) = rename_infix(m, unloc(D.lhs));
	unloc(D.lhs) = unapply( unloc(D.lhs) );
        D.rhs = rename_infix(m, D.rhs);

	assert( unloc(D.lhs).head().is_a<Hs::Var>() or is_pattern_binding(D));

        return D;
    }
    else if (E.is_a<Hs::SignatureDecl>())
        return E;
    else if (E.is_a<Hs::FixityDecl>())
        return E;
    else
        std::abort();
}

optional<Hs::Var> fundecl_head(const expression_ref& decl)
{
    assert(decl.is_a<Hs::SignatureDecl>() or decl.is_a<Hs::FixityDecl>() or decl.is_a<Hs::ValueDecl>());

    if (auto d = decl.to<Hs::ValueDecl>(); d and is_function_binding(*d))
    {
        auto fvar = unloc(d->lhs).head();
        assert(fvar.is_a<Hs::Var>());
        return fvar.as_<Hs::Var>();
    }
    return {};
}

// Probably we should first partition by (same x y = x and y are both function decls for the same variable)
pair<map<string,Hs::Type>, Hs::Decls> group_decls(const Haskell::Decls& decls)
{
    map<string, Hs::Type> signatures;

    Haskell::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        // Remove signature and fixity decls after recording signatures.
        if (auto sd = decl.to<Haskell::SignatureDecl>())
        {
            for(auto& var: sd->vars)
            {
                auto& name = unloc(var.name);
                if (signatures.count(name))
                    throw myexception()<<"Second signature for var '"<<name<<"' at location "<<*var.name.loc;
                signatures.insert({name, sd->type});
            }
        }
        else if (decl.is_a<Haskell::FixityDecl>())
        {
            // FixityDecls should survive up to this point so that we can properly segment decls.
            // But remove them here -> the type-checker shouldn't see them.
        }
        else if (auto d = decl.to<Haskell::ValueDecl>(); d and is_pattern_binding(*d))
        {
            decls2.push_back(Haskell::PatDecl{ d->lhs, d->rhs});
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Match m;
            for(int j=i;j<decls.size();j++)
            {
                if (fundecl_head(decls[j]) != fvar) break;

                auto& D = decls[j].as_<Haskell::ValueDecl>();

                m.rules.push_back( Hs::MRule{ unloc(D.lhs).copy_sub(), D.rhs } );

                if (m.rules.back().patterns.size() != m.rules.front().patterns.size())
                    throw myexception()<<"Function '"<<*fvar<<"' has different numbers of arguments!";
            }

            assert(not m.rules[0].patterns.empty() or m.rules.size() == 1);

            decls2.push_back( Hs::FunDecl( *fvar, m ) );

            // skip the other bindings for this function
            i += (m.rules.size()-1);
        }
        else
            std::abort();
    }

    return {signatures, decls2};
}

Haskell::Binds rename_infix(const Module& m, Haskell::Binds binds)
{
    assert(binds.size() == 1);
    for(auto& e: binds[0])
        e = rename_infix_decl(m, e);

    auto [sigs,bind0] = group_decls(binds[0]);

    binds.signatures = sigs;
    binds[0] = bind0;

    return binds;
}

bound_var_info renamer_state::rename_decls(Haskell::Binds& binds, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars, bool top)
{
    set<string> decls_free_vars;
    auto new_binders = rename_decls(binds, plus(bound, binders), decls_free_vars, top);
    add(free_vars, minus(decls_free_vars, binders));
    return new_binders;
}

bound_var_info renamer_state::rename_signatures(map<string, Hs::Type>& signatures, bool top)
{
    bound_var_info bound;
    map<string, Hs::Type> signatures2;
    for(auto& [name, type]: signatures)
    {
        assert(not is_qualified_symbol(name));
        type = rename_type(type);

        auto name2 = name;
        if (top)
            name2 = m.name + "." + name;
        signatures2.insert( {name2, type} );

        bound.insert(name2);
    }

    signatures = std::move(signatures2);
    return bound;
}

vector<Hs::Decls> split_decls(const Hs::Decls& decls, const vector< vector<int> >& referenced_decls)
{
    // 1. Compute strongly-connected components
    auto components = get_ordered_strong_components( make_graph(referenced_decls) );

    // 2. Divide the decls into groups
    vector<Hs::Decls> bind_groups;
    for(auto& component: components)
    {
        Hs::Decls bdecls;
        for(int i : component)
        {
            auto& decl = decls[i];

            // Collect the value decl
            bdecls.push_back(decl);
        }

        // Check if the decls group is recursive
        if (bdecls.size() >1)
            bdecls.recursive = true;
        else
        {
            int i = component[0];
            bdecls.recursive = includes(referenced_decls[i], i);
        }

        bind_groups.push_back(bdecls);
    }
    return bind_groups;
}

void group_binds(Hs::Binds& binds, const vector< vector<int> >& referenced_decls)
{
    auto& decls = binds[0];
    assert(referenced_decls.size() == decls.size());

    vector<Hs::Decls> new_binds = split_decls(decls, referenced_decls);

    // Split the bindings, but keep the signatures
    (vector<Hs::Decls>&)binds = new_binds;
}

bound_var_info renamer_state::rename_decls(Haskell::Binds& binds, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    assert(binds.size() == 1);
    auto& decls = binds[0];

    auto binders = find_bound_vars_in_decls(decls, top);

    auto sig_binders = rename_signatures(binds.signatures, top);

    for(auto& sig_binder: sig_binders)
        if (not binders.count(sig_binder))
            throw myexception()<<"Signature but no definition for '"<<sig_binder<<"'";

    set<string> decls_free_vars;
    auto refs = rename_grouped_decls(decls, plus(bound, binders), decls_free_vars, top);
    group_binds(binds, refs);

    add(free_vars, minus(decls_free_vars,binders));

    return binders;
}


