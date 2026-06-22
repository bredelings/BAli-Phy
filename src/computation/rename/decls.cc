#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>
#include <deque>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "util/graph.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::tuple;
using std::set;
using std::optional;
using std::map;
using std::deque;

optional<Hs::LVar> fundecl_head(const expression_ref& decl)
{
    if (auto fd = decl.to<Hs::FunDecl>())
        return fd->v;
    else
        return {};
}

// Probably we should first partition by (same x y = x and y are both function decls for the same variable)
tuple<map<Hs::LVar,Hs::LType>, map<Hs::LVar, Hs::inline_pragma_t>, Hs::Decls> group_decls(const Haskell::Decls& decls)
{
    map<Hs::LVar, Hs::LType> type_sigs;

    map<Hs::LVar, Hs::inline_pragma_t> inline_sigs;

    Hs::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto [loc,decl] = decls[i];
        // Remove signature and fixity decls after recording type_sigs.
        if (auto sd = decl.to<Hs::TypeSigDecl>())
        {
            for(auto& lvar: sd->vars)
            {
                if (type_sigs.count(lvar))
                    throw myexception()<<"Second signature for var '"<<unloc(lvar).name<<"' at location "<<*lvar.loc;
                else
                    type_sigs.insert({lvar, sd->type});
            }
        }
        else if (decl.is_a<Hs::FixityDecl>())
        {
            // Preserve local fixity declarations until the caller has extended the scoped fixity environment.
            decls2.push_back({loc, decl});
        }
        else if (auto d = decl.to<Hs::PatDecl>())
        {
            decls2.push_back({loc,*d});
        }
        else if (auto ip = decl.to<Hs::InlinePragma>())
        {
            Hs::Var x(unloc(ip->var));
            inline_sigs.insert({{ip->var.loc, x}, ip->command});
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Matches m;
            int consumed_decls = 0;
            for(int j=i;j<decls.size();j++)
            {
                auto [loc2,decl2] = decls[j];
                if (fundecl_head(decl2) != fvar) break;
                consumed_decls++;

                loc = loc * loc2;
                auto& FD = decl2.as_<Hs::FunDecl>();

                for(const auto& match: FD.matches)
                {
                    m.push_back(match);

                    if (m.back().patterns.size() != m.front().patterns.size())
                        error(loc2, Note()<<"Function '"<<*fvar<<"' has different numbers of arguments!");
                }
            }

            if (m[0].patterns.empty() and m.size() != 1)
                error(loc, Note()<<"Multiple definitions for variable "<<fvar->print()<<"!");

            decls2.push_back( {loc,Hs::FunDecl( *fvar, m )} );

            // skip the other bindings for this function
            i += (consumed_decls-1);
        }
        else
            std::abort();
    }

    return {type_sigs, inline_sigs, decls2};
}

Hs::Decls group_fundecls(const Haskell::Decls& decls)
{
    Haskell::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto [loc,decl] = decls[i];
        if (auto d = decl.to<Haskell::PatDecl>())
        {
            decls2.push_back({loc,*d});
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Matches m;
            int consumed_decls = 0;
            for(int j=i;j<decls.size();j++)
            {
                auto [loc2,decl2] = decls[j];
                if (fundecl_head(decl2) != fvar) break;
                consumed_decls++;

                auto& FD = decl2.as_<Hs::FunDecl>();
                loc = loc * loc2;

                for(const auto& match: FD.matches)
                {
                    m.push_back(match);

                    if (m.back().patterns.size() != m.front().patterns.size())
                        throw myexception()<<"Function '"<<*fvar<<"' has different numbers of arguments!";
                }
            }

            if (m[0].patterns.empty() and m.size() != 1)
                throw myexception()<<"Multiple definitions for variable "<<fvar->print()<<"!";

            decls2.push_back( {loc, Hs::FunDecl( *fvar, m )} );

            // skip the other bindings for this function
            i += (consumed_decls-1);
        }
        else
            std::abort();
    }

    return decls2;
}

bound_var_info renamer_state::rename_decls(Haskell::Binds& binds, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars, bool top)
{
    set<string> decls_free_vars;
    auto new_binders = rename_decls(binds, plus(bound, binders), decls_free_vars, top);
    add(free_vars, minus(decls_free_vars, binders));
    return new_binders;
}

void renamer_state::rename_signatures(map<Hs::LVar, Hs::LType>& type_sigs, map<Hs::LVar, Hs::inline_pragma_t>& inline_sigs, const bound_var_info& binders, bool top)
{
    map<Hs::LVar, Hs::LType> type_sigs2;
    for(auto& [lvar, ltype]: type_sigs)
    {
        ltype = rename_and_quantify_type(ltype);

        if (is_qualified_symbol(unloc(lvar).name))
        {
            error(lvar.loc, Note()<<"Variable name may not be qualified!");
            continue;
        }

        auto lvar2 = lvar;
        auto& var2 = unloc(lvar2);
        if (top)
            qualify_name(var2.name);
        type_sigs2.insert( {lvar2, ltype} );

        if (not binders.count(var2.name))
        {
            error(lvar.loc, Note()<<"Signature but no definition for '"<<unloc(lvar).name<<"'");
        }
    }
    type_sigs = std::move(type_sigs2);

    map<Hs::LVar, Hs::inline_pragma_t> inline_sigs2;
    for(auto& [lvar, ip]: inline_sigs)
    {
        if (is_qualified_symbol(unloc(lvar).name))
        {
            error(lvar.loc, Note()<<"Variable name may not be qualified!");
            continue;
        }

        auto lvar2 = lvar;
        auto& var2 = unloc(lvar2);
        if (top)
            qualify_name(var2.name);
        inline_sigs2.insert( {lvar2, ip} );

        if (not binders.count(var2.name))
        {
            error(lvar.loc, Note()<<"Signature but no definition for '"<<unloc(lvar).name<<"'");
        }
    }
    inline_sigs = std::move(inline_sigs2);
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
std::tuple<map<string,int>, map<Hs::Var,vector<Hs::LVar>>> get_indices_for_names(const Hs::Decls& decls)
{
    map<string,int> index_for_name;
    map<Hs::Var,std::vector<Hs::LVar>> duplicate_defs;

    for(int i=0;i<decls.size();i++)
    {
        auto& [loc,decl] = decls[i];

        // Get the binder vars introduced by this declaration
        set<Hs::LVar> vars;
        if (auto fd = decl.to<Hs::FunDecl>())
            vars.insert({fd->v});
        else if (auto pd = decl.to<Hs::PatDecl>())
            vars = Hs::vars_in_pattern( pd->lhs );
        else
            std::abort();

        // Record the index for each of those vars
        for(auto& lvar: vars)
        {
	    auto& [loc,var] = lvar;
            auto iter = duplicate_defs.find(var);

            if (iter == duplicate_defs.end())
            {
                // Record the index for each of those vars
                index_for_name.insert({var.name, i});
                // Create an empty list of duplicates.
                duplicate_defs.insert({var,{lvar}});
            }
            else
            {
                // Record a duplicate
                iter->second.push_back(lvar);
            }
        }
    }

    return {index_for_name, duplicate_defs};
}

bool is_strict_binding_pattern(const Hs::LPat& lpat)
{
    auto& pat = unloc(lpat);

    if (pat.is_a<Hs::StrictPattern>())
        return true;
    else if (auto tpat = pat.to<Hs::TypedPattern>())
        return is_strict_binding_pattern(tpat->pat);
    else
        return false;
}

vector<vector<int>> renamer_state::rename_grouped_decls(Haskell::Decls& decls, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    // NOTE: bound already includes the binder names.

    for(int i=0;i<decls.size();i++)
    {
        auto& [loc,decl] = decls[i];

        if (decl.is_a<Hs::PatDecl>())
        {
            auto PD = decl.as_<Hs::PatDecl>();

            if (top and is_strict_binding_pattern(PD.lhs))
                error(PD.lhs.loc, Note()<<"Strict pattern bindings are not allowed at top level.");

            rename_pattern( PD.lhs, top);
            PD.rhs = rename(PD.rhs, bound, PD.rhs_free_vars);
            decl = PD;
        }
        else if (decl.is_a<Hs::FunDecl>())
        {
            auto FD = decl.as_<Hs::FunDecl>();
            auto& name = unloc(FD.v).name;
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
        for(int j=1;j<second_defs.size();j++)
        {
//	    auto& [loc1, first_def] = second_defs[0];
	    auto& [loc , extra_def] = second_defs[j];

            Note note;
            note<<"Name `"<<extra_def.name<<"` redefined.";
//            How do we attach a reference to the first def w/o adding another error?	   
//            if (first_def.name.loc)
//                note<<"\nFirst definition at "<<*first_def.name.loc;
            error( loc, note);
        }
    }

    // Construct referenced decls
    vector<vector<int>> referenced_decls;
    for(auto& [loc,decl]: decls)
    {
        vector<int> refs;
        auto& rhs_free_vars = get_rhs_free_vars(decl);
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

    remove_fixity_decls(decls);

    auto binders = find_bound_vars_in_decls(decls, top);

    rename_signatures(binds.signatures, binds.inline_sigs, binders, top);

    set<string> decls_free_vars;
    auto refs = rename_grouped_decls(decls, plus(bound, binders), decls_free_vars, top);
    group_binds(binds, refs);

    add(free_vars, minus(decls_free_vars,binders));

    return binders;
}
