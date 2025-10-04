#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

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

// -1. Can we group decls BEFORE we rename_lhs?
//     * 

// 0. Do we rename signature decls before we group, or do we reame signatures after we group?

// 1. Go through and explicitly handle different expression types
//    instead of using E.size() == 0.

// 2. Ideally convert lambda expressions to lambda_exp and LambdaExp

// 3. Ideally convert case expressions to case_exp and CaseExp

// 4. convert module, imports, exports, etc.

// 5. convert constructors and stuff

// 6. Extract the type dependencies

// 7. Divide into dependency groups

// 8. Infer kinds for type variables.

// A. pop size changes -- how to implement?

// B. try some different data sets from skyline papers -- hcv2, bison, etc.

Hs::MultiGuardedRHS rename_infix(const Module& m, Hs::MultiGuardedRHS R)
{
    for(auto& guarded_rhs: R.guarded_rhss)
    {
        for(auto& guard: guarded_rhs.guards)
            guard = rename_infix(m, guard);

        guarded_rhs.body = rename_infix(m, guarded_rhs.body);
    }

    if (R.decls)
        unloc(*R.decls) = rename_infix(m, unloc(*R.decls));

    return R;
}


Hs::ClassDecl rename_infix(const Module& m, Hs::ClassDecl C)
{
    // 1. Recursively fix-up infix expressions
    for(auto& [_,e]: C.default_method_decls)
        e = rename_infix_decl(m, e);

    // 2. Group different parts of fundecls
    C.default_method_decls = group_fundecls(C.default_method_decls);

    // Don't create sigs map yet.
    return C;
}

Hs::InstanceDecl rename_infix(const Module& m, Hs::InstanceDecl I)
{
    // 1. Recursively fix-up infix expressions
    for(auto& [_,e]: I.method_decls)
        e = rename_infix_decl(m, e);

    // 2. Group different parts of fundecls
    I.method_decls = group_fundecls(I.method_decls);

    return I;
}

Hs::ModuleDecls rename_infix(const Module& m, Hs::ModuleDecls M)
{
    // 1. Handle value decls
    M.value_decls = rename_infix(m, M.value_decls);

    // default_decls aren't infix
    for(auto& [_,type_decl]: M.type_decls)
    {
        // 2. Handle default method decls
        if (auto C = type_decl.to<Haskell::ClassDecl>())
            type_decl = rename_infix(m, *C);
        // 3. Handle method decls
        else if (auto I = type_decl.to<Haskell::InstanceDecl>())
            type_decl = rename_infix(m, *I);
    }
    return M;
}

// A data declaration MAY use the same field label in multiple constructors as long as the typing of the field is the same in all cases after type synonym expansion.
// A label CANNOT be shared by more than one type in scope.
// Field names share the top level namespace with ordinary variables and class methods and must not conflict with other top level names in scope.

Hs::Decls synthesize_field_accessors(const Hs::Decls& decls)
{
    Hs::Decls decls2;

    for(auto& [_,decl]: decls)
    {
        if (not decl.is_a<Haskell::DataOrNewtypeDecl>()) continue;
        auto D = decl.as_<Haskell::DataOrNewtypeDecl>();

        if (not D.is_regular_decl()) continue;

        auto& constrs = D.get_constructors();
        if (constrs.empty()) continue;

        // field -> con -> pos
        map<string,map<string,int>> constructor_fields;
        // con -> arity
        map<string,int> arity;

        for(auto& constr: constrs)
        {
            if (constr.is_record_constructor())
            {
                auto& fields = std::get<1>(constr.fields);

                int i = 0;
                for(auto& field_group: fields.field_decls)
                {
                    for(auto& field_name: field_group.field_names)
                    {
                        constructor_fields[unloc(field_name).name][unloc(*constr.con).name] = i;
                        i++;
                    }
                }
                arity[unloc(*constr.con).name] = i;
            }
        }

        if (not arity.empty())
        {
            for(auto& [field_name, constrs]: constructor_fields)
            {
                expression_ref name = Hs::Var(field_name);
                vector<Located<Haskell::Alt>> alts;

                for(auto& [ConName,pos]: constrs)
                {
                    int a = arity[ConName];
                    vector<Located<expression_ref>> f(a, {noloc,Hs::WildcardPattern()});
                    unloc(f[pos]) = name;

                    Hs::LPat pattern = Hs::apply({noloc,Hs::Con(ConName,a)},f);
                    auto rhs = Haskell::SimpleRHS({noloc, name});
                    alts.push_back({noloc,{pattern,rhs}});
                }
                // I removed the {_ -> error("{name}: no match")} alternative, since error( ) generates a var( ).
                // This could lead to worse error messages.

                Hs::Var x("v$0"); // FIXME??
                expression_ref body = Haskell::CaseExp({noloc,x},Haskell::Alts(alts));
                expression_ref lambda = Haskell::LambdaExp({{noloc,x}},{noloc,body});

                decls2.push_back({noloc,Haskell::ValueDecl({noloc,name}, lambda)});
            }
        }
    }
    return decls2;
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
    Rn.rename_decls(M.value_decls, bound_names, free_vars, true);

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
    show_messages(m.file, std::cerr, Rn.messages);

    // 7. Quit if there were error messages.
    exit_on_error(Rn.messages);

    return M;
}


void renamer_state::error(const Note& note) const
{
    messages.push_back({ErrorMsg, {}, {note}});
}

void renamer_state::error(const optional<yy::location>& loc, const Note& note) const
{
    messages.push_back({ErrorMsg, loc, {note}});
}

void renamer_state::warning(const Note& note) const
{
    messages.push_back({WarningMsg, {}, {note}});
}

void renamer_state::warning(const optional<yy::location>& loc, const Note& note) const
{
    messages.push_back({WarningMsg, loc, {note}});
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
        add(bound_names, find_bound_vars_in_funpatdecl(decl, top));

    return bound_names;
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

bound_var_info renamer_state::find_bound_vars_in_stmt(const Located<expression_ref>& lstmt)
{
    auto& stmt = unloc(lstmt);
    if (stmt.is_a<Hs::SimpleQual>())
	return {};
    else if (stmt.is_a<Haskell::PatQual>())
    {
        auto& PQ = stmt.as_<Haskell::PatQual>();
        return find_vars_in_pattern(PQ.bindpat);
    }
    else if (stmt.is_a<Haskell::LetQual>())
    {
        auto& LQ = stmt.as_<Haskell::LetQual>();
        return find_bound_vars_in_decls(unloc(LQ.binds));
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

