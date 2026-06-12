#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>
#include <algorithm>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::map;

// So... let_exp is like Core, and Let is like STG.

// A data declaration MAY use the same field label in multiple constructors as long as the typing of the field is the same in all cases after type synonym expansion.
// A label CANNOT be shared by more than one type in scope.
// Field names share the top level namespace with ordinary variables and class methods and must not conflict with other top level names in scope.

namespace
{
    // Build the body of a selector from the field metadata for one label.
    expression_ref synthesize_field_accessor(const Module& m, const FieldInfo& field)
    {
        vector<Located<Haskell::Alt>> alts;
        auto selector_name = get_unqualified_name(field.name);

        assert(field.constructors.size() == field.positions.size());

        for(size_t i = 0; i < field.constructors.size(); ++i)
        {
            const auto& constructor_name = field.constructors[i];
            const auto& position = field.positions[i];
            auto constructor = m.lookup_resolved_symbol(constructor_name);
            if (not constructor or not constructor->arity)
                throw myexception() << "Could not synthesize selector for field '" << field.name << "' because constructor '" << constructor_name << "' has no known arity.";

            int arity = *constructor->arity;
            if (position < 0 or position >= arity)
                throw myexception() << "Field '" << field.name << "' is recorded at position " << position << " for constructor '" << constructor_name << "', but the constructor arity is " << arity << ".";

            Hs::LPats pats(arity, {noloc, Hs::WildcardPattern()});
            unloc(pats[position]) = Hs::VarPattern({noloc, Hs::Var(selector_name)});

            Hs::LPat pattern = {noloc, Hs::ConPattern({noloc, Hs::Con(constructor_name, arity)}, pats)};
            auto rhs = Haskell::SimpleRHS({noloc, Hs::Var(selector_name)});
            alts.push_back({noloc, {pattern, rhs}});
        }

        Hs::Var arg("v$0");
        Hs::LPat arg_pat = {noloc, Hs::VarPattern({noloc, arg})};
        expression_ref body = Haskell::CaseExp({noloc, arg}, Haskell::Alts(alts));
        expression_ref lambda = Haskell::LambdaExp({arg_pat}, {noloc, body});
        return lambda;
    }

    // Generate selector declarations after renaming, using already-qualified names.
    Hs::Decls synthesize_field_accessors(const Module& m)
    {
        Hs::Decls decls;
        for(const auto& [_, field]: m.local_synthesizable_record_fields())
        {
            auto selector = m.lookup_resolved_symbol(field.name);
            assert(selector and selector->record_selector);
            if (selector->record_selector->callability == RecordSelectorCallability::Naughty)
                continue;

            Hs::LVar lhs = {noloc, Hs::Var(field.name)};
            auto rhs = Hs::LExp{noloc, synthesize_field_accessor(m, field)};
            decls.push_back({noloc, Haskell::simple_decl(lhs, rhs)});
        }

        return decls;
    }
}

Hs::Decls synthesize_renamed_field_accessors(const Module& m)
{
    return synthesize_field_accessors(m);
}

// 1. The primary purpose of the rename pass is to convert identifiers to (possibly qualified) vars.

// 2. Additionally, we also try and translate rec expressions to mfix expressions here.

// We keep track of locally bound variables only so that we know when to avoid looking for a qualified symbol.
typedef set<string> bound_var_info;

// Currently we interleave discovering bound variables and modifying them.  For example, when we
// analyze a `let decls body` statement, we rename the variables in the decls at the same time that
// we accumulate the bound variables.  We then use the combined list of bound variables to rename the body.

bound_var_info intersection(const bound_var_info& bv1, const bound_var_info& bv2)
{
    bound_var_info I;
    for(auto& v: bv1)
 	if (bv2.count(v))
            I.insert(v);

    return I;
}

bool disjoint_add(bound_var_info& bv1, const bound_var_info& bv2)
{
    for(auto& v: bv2)
 	if (not bv1.insert(v).second)
	    return false;
    bv1.insert(bv2.begin(), bv2.end());
    return true;
}

void add(bound_var_info& bv1, const bound_var_info& bv2)
{
    for(auto& v: bv2)
 	bv1.insert(v);
    bv1.insert(bv2.begin(), bv2.end());
}

Haskell::ModuleDecls rename(const simplifier_options&, const Module& m, Haskell::ModuleDecls M)
{
    renamer_state Rn(m);

    M.type_decls = Rn.rename_type_decls(M.type_decls);

    M.default_decls = Rn.rename_default_decls(M.default_decls);

    // Find all the names bound HERE, versus in individual decls.

    // The idea is that we only add unqualified names here, and they shadow
    // qualified names.

    // 1. Get bound names for top-level value decls
    bound_var_info bound_names;
    add(bound_names, Rn.find_bound_vars_in_decls(M.value_decls[0], true));

    // 2. Add bound names for class methods
    for(auto& [_,decl]: M.type_decls)
    {
        if (auto C = decl.to<Haskell::ClassDecl>())
        {
            for(auto& sig_decl: C->sig_decls)
                for(auto& method_var: sig_decl.vars)
                    bound_names.insert(unloc(method_var).name);
        }
        // Wait.. don't we need to discover constructors, too?
    }

    set<string> free_vars;

    // 3. Extract sigs for foreign imports
    for(auto& foreign_decl: M.foreign_decls)
    {
        Rn.qualify_name(unloc(foreign_decl.function).name );

        foreign_decl.type = Rn.rename_and_quantify_type( foreign_decl.type );

        bound_names.insert( {unloc(foreign_decl.function).name} );
    }

    // 4. Rename value decls.
    auto value_rn = Rn.child();
    value_rn.fixity_env = value_rn.add_fixities_from_decls(value_rn.fixity_env, M.value_decls[0]);
    value_rn.rename_decls(M.value_decls, bound_names, free_vars, true);

    // 5. Replace ids with dummies
    for(auto& [_,decl]: M.type_decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto C = decl.as_<Haskell::ClassDecl>();
            for(auto& [_,method_decl]: C.default_method_decls)
            {
                if (auto pd = method_decl.to<Hs::PatDecl>())
                {
                    Rn.error(pd->lhs.loc, Note()<<"Illegal pattern binding in class "<<C.con);
                }
                else
                {
                    auto FD = method_decl.as_<Hs::FunDecl>();
                    FD.matches = Rn.rename( FD.matches, bound_names, FD.rhs_free_vars);
                    method_decl = FD;
                }
            }
            decl = C;
        }
        else if (decl.is_a<Haskell::InstanceDecl>())
        {
            auto I = decl.as_<Haskell::InstanceDecl>();
            for(auto& [_, method_decl]: I.method_decls)
            {
                if (auto pd = method_decl.to<Hs::PatDecl>())
                {
                    Rn.error(pd->lhs.loc, Note()<<"Illegal pattern binding in instance "<<I.polytype.print());
                }
                else
                {
                    auto FD = method_decl.as_<Hs::FunDecl>();
                    FD.matches = Rn.rename( FD.matches, bound_names, FD.rhs_free_vars);
                    method_decl = FD;
                }
            }
            decl = I;
        }
    }

    // 6. Show warning and error messages.
    show_messages(m.file, std::cerr, Rn.messages());

    // 7. Quit if there were error messages.
    exit_on_error(Rn.messages());

    return M;
}


void renamer_state::error(const Note& note) const
{
    messages().push_back({ErrorMsg, {}, {note}});
}

void renamer_state::error(const optional<yy::location>& loc, const Note& note) const
{
    messages().push_back({ErrorMsg, loc, {note}});
}

void renamer_state::warning(const Note& note) const
{
    messages().push_back({WarningMsg, {}, {note}});
}

void renamer_state::warning(const optional<yy::location>& loc, const Note& note) const
{
    messages().push_back({WarningMsg, loc, {note}});
}

void renamer_state::qualify_name(std::string& name) const
{
    assert(not is_qualified_symbol(name));
    name = m.name + "." + name;
}

void renamer_state::qualify_name(Located<std::string>& name) const
{
    qualify_name(unloc(name));
}

void renamer_state::qualify_name(Hs::Var& v) const
{
    qualify_name(v.name);
}

void renamer_state::qualify_name(Hs::Con& c) const
{
    qualify_name(c.name);
}

void renamer_state::qualify_name(Hs::TypeCon& tc) const
{
    qualify_name(tc.name);
}

bound_var_info renamer_state::find_vars_in_patterns(const Hs::LPats& pats, bool top)
{
    bound_var_info bound;

    for(auto& pat: pats)
    {
        auto bound_here = find_vars_in_pattern(pat, top);
        auto overlap = intersection(bound, bound_here);
        if (not overlap.empty())
        {
            auto name = *overlap.begin();
            error(Note()<<"Pattern uses a variable '"<<name<<"' twice!");
        }
        add(bound, bound_here);
    }

    return bound;
}

bound_var_info renamer_state::find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top)
{
    if (auto d = decl.to<Haskell::PatDecl>())
        return find_vars_in_pattern( d->lhs, top);
    else if (auto d = decl.to<Haskell::FunDecl>())
        return find_vars_in_pattern({d->v.loc,Hs::VarPattern(d->v)}, top);
    else
        std::abort();
}

bound_var_info renamer_state::find_bound_vars_in_decls(const Haskell::Decls& decls, bool top)
{
    // The idea is that we only add unqualified names here, and they shadow
    // qualified names.
    bound_var_info bound_names;
    for(auto& [_,decl]: decls)
    {
        if (decl.is_a<Hs::FixityDecl>())
            continue;
        add(bound_names, find_bound_vars_in_funpatdecl(decl, top));
    }

    return bound_names;
}

// Return a fixity environment extended with local fixity declarations.
fixity_env_t renamer_state::add_fixities_from_decls(fixity_env_t env, const Hs::Decls& decls) const
{
    for(const auto& [loc, decl]: decls)
    {
        auto FD = decl.to<Hs::FixityDecl>();
        if (not FD)
            continue;

        int precedence = FD->precedence ? *FD->precedence : 9;
        if (precedence < 0 or precedence > 9)
        {
            error(loc, Note()<<"Precedence level "<<precedence<<" not allowed.");
            continue;
        }

        for(const auto& name: FD->names)
        {
            auto op = unloc(name);
            if (is_qualified_symbol(op))
            {
                error(name.loc, Note()<<"Trying to declare fixity of qualified symbol '"<<op<<"'.  Use its unqualified name.");
                continue;
            }
            env[op] = fixity_info{FD->fixity, precedence};
        }
    }

    return env;
}

// Remove local fixity declarations after they have been added to the scoped environment.
void remove_fixity_decls(Hs::Decls& decls)
{
    decls.erase(std::remove_if(decls.begin(), decls.end(),
                               [](const Located<expression_ref>& decl) { return unloc(decl).is_a<Hs::FixityDecl>(); }),
                decls.end());
}

bound_var_info renamer_state::find_bound_vars_in_decls(const Haskell::Binds& binds, bool top)
{
    bound_var_info bound_names;
    for(auto& decls: binds)
        add(bound_names, find_bound_vars_in_decls(decls, top));

    for(auto& [var,_]: binds.signatures)
    {
        auto name = unloc(var).name;
        if (top)
            name = m.name + "." + name;
        bound_names.insert(name);
    }

    return bound_names;
}

bound_var_info renamer_state::find_bound_vars_in_decl(const Haskell::TypeSigDecl& decl, bool is_top_level)
{
    bound_var_info bound_names;

    for(auto& var: decl.vars)
    {
        auto name = unloc(var).name;
        if (is_top_level)
            name = m.name + "." + name;
        bound_names.insert(name);
    }

    return bound_names;
}

const set<string>& get_rhs_free_vars(const expression_ref& decl)
{
    if (decl.is_a<Hs::PatDecl>())
        return decl.as_<Hs::PatDecl>().rhs_free_vars;
    else if (decl.is_a<Hs::FunDecl>())
        return decl.as_<Hs::FunDecl>().rhs_free_vars;
    else
        std::abort();
};

bound_var_info renamer_state::find_bound_vars_in_stmt(Located<expression_ref>& lstmt)
{
    auto& stmt = unloc(lstmt);
    if (stmt.is_a<Hs::SimpleQual>())
	return {};
    else if (stmt.is_a<Haskell::PatQual>())
    {
        auto PQ = stmt.as_<Haskell::PatQual>();
        auto bound = find_vars_in_pattern(PQ.bindpat);
        stmt = PQ;
        return bound;
    }
    else if (stmt.is_a<Haskell::LetQual>())
    {
        auto LQ = stmt.as_<Haskell::LetQual>();
        auto bound = find_bound_vars_in_decls(unloc(LQ.binds));
        stmt = LQ;
        return bound;
    }
    else if (stmt.is_a<Haskell::RecStmt>())
        throw myexception()<<"find_bound_vars_in_stmt: should not have a rec stmt inside a rec stmt!";
    else
	std::abort();
}

pair<Hs::LExp,set<string>> renamer_state::rename(const Hs::LExp& E, const bound_var_info& bound)
{
    set<string> free_vars;
    auto E2 = rename(E, bound, free_vars);
    return {E2,free_vars};
}

Hs::LExp
renamer_state::rename(const Hs::LExp& E, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
{
    if (binders.empty())
        return rename(E, bound, free_vars);
    else
    {
        set<string> exp_free_vars;
        auto E2 = rename(E, plus(bound, binders), exp_free_vars);
        add(free_vars, minus(exp_free_vars,binders));
        return E2;
    }
}
