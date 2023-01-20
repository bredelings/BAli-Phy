#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>
#include <deque>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
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

pair<map<Hs::Var,Hs::Type>, Hs::Decls> group_decls(const Haskell::Decls& decls); // value decls, signature decls, and fixity decls


bool is_definitely_pattern(const Haskell::Expression& lhs)
{
    if (lhs.is_a<Haskell::List>())
        return true;
    else if (lhs.is_a<Haskell::Tuple>())
        return true;
    else if (lhs.is_a<Haskell::AsPattern>())
        return true;
    else if (lhs.is_a<Haskell::LazyPattern>())
        return true;
    else if (lhs.is_a<Haskell::StrictPattern>())
        return true;
    else if (lhs.is_a<Haskell::ConPattern>())
        return true;
    else if (lhs.is_a<Haskell::VarPattern>())
        return true;
    else if (lhs.is_a<Haskell::LiteralPattern>())
        return true;

    return false;
}

expression_ref rename_infix_decl(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::ValueDecl>())
    {
        auto D = E.as_<Haskell::ValueDecl>();

        auto lhs = rename_infix(m, D.lhs);
        auto rhs = rename_infix(m, D.rhs);

        if (auto v = unloc(lhs).to<Hs::Var>())
            return Hs::simple_decl(*v, rhs);
        else if (is_definitely_pattern(unloc(lhs)))
            return Hs::PatDecl( unapply(lhs), rhs );
        else if (unloc(lhs).is_a<Hs::ApplyExp>())
        {
            auto [head,args] = Hs::decompose_apps(lhs);

            if (unloc(head).is_a<Hs::Con>())
                return Hs::PatDecl( unapply(lhs), rhs);

            else if (auto v = unloc(head).to<Hs::Var>())
            {
                Hs::LPats pats;
                for(auto& arg: args)
                    pats.push_back( unapply(arg) );

                return Hs::simple_fun_decl(*v, pats, rhs);
            }
        }
        throw myexception()<<"I don't recognize this declaration:\n    "<<E.print();
    }
    else if (E.is_a<Hs::SignatureDecl>())
        return E;
    else if (E.is_a<Hs::FixityDecl>())
        return E;
    else if (E.is_a<Hs::TypeFamilyDecl>() or E.is_a<Hs::TypeFamilyInstanceDecl>())
    {
        // We get here for type family stuff inside of class declarations.
        // Ignoring infix type names for now?
        return E;
    }
    else
        std::abort();
}

optional<Hs::Var> fundecl_head(const expression_ref& decl)
{
    if (auto fd = decl.to<Hs::FunDecl>())
        return fd->v;
    else
        return {};
}

// Probably we should first partition by (same x y = x and y are both function decls for the same variable)
pair<map<Hs::Var,Hs::Type>, Hs::Decls> group_decls(const Haskell::Decls& decls)
{
    map<Hs::Var, Hs::Type> signatures;

    Haskell::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        // Remove signature and fixity decls after recording signatures.
        if (auto sd = decl.to<Haskell::SignatureDecl>())
        {
            for(auto& var: sd->vars)
            {
                if (signatures.count(var))
                    throw myexception()<<"Second signature for var '"<<var.name<<"'"; // at location "<<*var.name.loc;
                signatures.insert({var, sd->type});
            }
        }
        else if (decl.is_a<Haskell::FixityDecl>())
        {
            // FixityDecls should survive up to this point so that we can properly segment decls.
            // But remove them here -> the type-checker shouldn't see them.
        }
        else if (auto d = decl.to<Haskell::PatDecl>())
        {
            decls2.push_back(*d);
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Matches m;
            for(int j=i;j<decls.size();j++)
            {
                if (fundecl_head(decls[j]) != fvar) break;

                auto& FD = decls[j].as_<Hs::FunDecl>();

                assert(FD.matches.size() == 1);
                m.push_back( FD.matches[0] );

                if (m.back().patterns.size() != m.front().patterns.size())
                    throw myexception()<<"Function '"<<*fvar<<"' has different numbers of arguments!";
            }

            if (m[0].patterns.empty() and m.size() != 1)
                throw myexception()<<"Multiple definitions for variable "<<fvar->print()<<"!";

            decls2.push_back( Hs::FunDecl( *fvar, m ) );

            // skip the other bindings for this function
            i += (m.size()-1);
        }
        else
            std::abort();
    }

    return {signatures, decls2};
}

Hs::Decls group_fundecls(const Haskell::Decls& decls)
{
    Haskell::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto d = decl.to<Haskell::PatDecl>())
        {
            decls2.push_back(*d);
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Matches m;
            for(int j=i;j<decls.size();j++)
            {
                if (fundecl_head(decls[j]) != fvar) break;

                auto& FD = decls[j].as_<Hs::FunDecl>();

                assert(FD.matches.size() == 1);
                m.push_back( FD.matches[0] );

                if (m.back().patterns.size() != m.front().patterns.size())
                    throw myexception()<<"Function '"<<*fvar<<"' has different numbers of arguments!";
            }

            if (m[0].patterns.empty() and m.size() != 1)
                throw myexception()<<"Multiple definitions for variable "<<fvar->print()<<"!";

            decls2.push_back( Hs::FunDecl( *fvar, m ) );

            // skip the other bindings for this function
            i += (m.size()-1);
        }
        else
            std::abort();
    }

    return decls2;
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

bound_var_info renamer_state::rename_signatures(map<Hs::Var, Hs::Type>& signatures, bool top)
{
    bound_var_info bound;
    map<Hs::Var, Hs::Type> signatures2;
    for(auto& [var, type]: signatures)
    {
        assert(not is_qualified_symbol(var.name));
        type = rename_type(type);

        auto var2 = var;
        if (top)
            qualify_name(var2.name);
        signatures2.insert( {var2, type} );

        bound.insert(var2.name);
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


// So... factor out rename_grouped_decl( ), and then make a version that splits into components, and a version that does not?
// Splitting the decls for classes and instances into  components really doesn't make sense...

// maps names in a declaration group to a declaration in the group.
std::tuple<map<string,int>, map<Hs::Var,vector<Hs::Var>>> get_indices_for_names(const Hs::Decls& decls)
{
    map<string,int> index_for_name;
    map<Hs::Var,std::vector<Hs::Var>> duplicate_defs;

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];

        // Get the binder vars introduced by this declaration
        set<Hs::Var> vars;
        if (auto fd = decl.to<Hs::FunDecl>())
            vars.insert({fd->v});
        else if (auto pd = decl.to<Hs::PatDecl>())
            vars = Hs::vars_in_pattern( pd->lhs );
        else
            std::abort();

        // Record the index for each of those vars
        for(auto& var: vars)
        {
            auto iter = duplicate_defs.find(var);

            if (iter == duplicate_defs.end())
            {
                // Record the index for each of those vars
                index_for_name.insert({var.name, i});
                // Create an empty list of duplicates.
                duplicate_defs.insert({var,{}});
            }
            else
            {
                // Record a duplicate
                iter->second.push_back(var);
            }
        }
    }

    return {index_for_name, duplicate_defs};
}

vector<vector<int>> renamer_state::rename_grouped_decls(Haskell::Decls& decls, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    // NOTE: bound already includes the binder names.

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];

        if (decl.is_a<Hs::PatDecl>())
        {
            auto PD = decl.as_<Hs::PatDecl>();

            rename_pattern( PD.lhs, top);
            PD.rhs = rename(PD.rhs, bound, PD.rhs_free_vars);
            decl = PD;
        }
        else if (decl.is_a<Hs::FunDecl>())
        {
            auto FD = decl.as_<Hs::FunDecl>();
            auto& name = FD.v.name;
            assert(not is_qualified_symbol(name));
            if (top)
                name = m.name + "." + name;

            FD.matches = rename(FD.matches, bound, FD.rhs_free_vars);

            decl = FD;
        }
        else
            std::abort();
    }

    // Map the names to indices
    auto [index_for_name, duplicate_defs] = get_indices_for_names(decls);
    for(auto& [first_def, second_defs]: duplicate_defs)
    {
        for(auto& second_def: second_defs)
        {
            Note note;
            note<<"Name `"<<second_def.name<<"` redefined.";
//            if (first_def.name.loc)
//                note<<"\nFirst definition at "<<*first_def.name.loc;
            error( /*second_def.name.loc,*/ note);
        }
    }

    // Construct referenced decls
    vector<vector<int>> referenced_decls;
    for(int i=0;i<decls.size();i++)
    {
        vector<int> refs;
        auto& rhs_free_vars = get_rhs_free_vars(decls[i]);
        for(auto& name: rhs_free_vars)
        {
            auto it = index_for_name.find(name);

            // Skip if this name isn't one of the ids being defined.
            if (it == index_for_name.end()) continue;

            refs.push_back(it->second);
        }
        referenced_decls.push_back( std::move(refs) );

        add(free_vars, rhs_free_vars);
    }

    // NOTE: binder names are removed in the called - rename_decls( ).

    return referenced_decls;
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


