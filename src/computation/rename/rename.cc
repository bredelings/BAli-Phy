#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

#include "rename.H"
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


set<string> find_bound_vars(const expression_ref& E)
{
    if (E.is_expression())
    {
	set<string> bound;
	for(const auto& e:E.sub())
	    add(bound, find_bound_vars(e));
	return bound;

    }
    else if (E.is_a<Haskell::List>())
    {
        auto& L = E.as_<Haskell::List>();
	set<string> bound;
	for(const auto& e: L.elements)
	    add(bound, find_bound_vars(e));
	return bound;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto& T = E.as_<Haskell::Tuple>();
	set<string> bound;
	for(const auto& e: T.elements)
	    add(bound, find_bound_vars(e));
	return bound;
    }
    else if (E.is_a<Hs::Var>())
    {
        auto& value = unloc(E.as_<Hs::Var>().name);
	assert(not is_haskell_con_name(value));
        return {value};
    }
    else
        return {};
}

string get_func_name(const Haskell::ValueDecl& decl)
{
    auto& head = unloc(decl.lhs).head();
    assert(head.is_a<Hs::Var>() or head.is_a<var>());
    if (head.is_a<Hs::Var>())
        return unloc(head.as_<Hs::Var>().name);
    else if (head.is_a<var>())
        return head.as_<var>().name;
    else
        std::abort();
}

string desugar_get_func_name(const Haskell::ValueDecl& decl)
{
    return unloc(decl.lhs).head().as_<var>().name;
}

bool is_pattern_binding(const Haskell::ValueDecl& decl)
{
    auto& lhs = unloc(decl.lhs);
    if (lhs.is_a<Haskell::List>())
        return true;
    if (lhs.is_a<Haskell::Tuple>())
        return true;
    if (lhs.is_a<Haskell::AsPattern>())
        return true;
    if (lhs.is_a<Haskell::LazyPattern>())
        return true;
    if (lhs.is_a<Haskell::StrictPattern>())
        return true;
    // FIXME: we should make a ConPattern
    if (lhs.head().is_a<Haskell::Con>())
        return true;
    return false;
}

bool is_function_binding(const Haskell::ValueDecl& decl)
{
    return not is_pattern_binding(decl);
}

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

Hs::ModuleDecls rename_infix(const Module& m, Hs::ModuleDecls M)
{
    M.value_decls = rename_infix(m, M.value_decls);

    for(auto& type_decl: M.type_decls)
    {
        if (type_decl.is_a<Haskell::ClassDecl>())
        {
            auto C = type_decl.as_<Haskell::ClassDecl>();
            if (C.binds)
                unloc(*C.binds) = rename_infix(m, unloc(*C.binds));
            type_decl = C;
        }
        else if (type_decl.is_a<Haskell::InstanceDecl>())
        {
            auto I = type_decl.as_<Haskell::InstanceDecl>();
            if (I.binds)
                unloc(*I.binds) = rename_infix(m, unloc(*I.binds));
            type_decl = I;
        }
    }
    return M;
}

// A data declaration MAY use the same field label in multiple constructors as long as the typing of the field is the same in all cases after type synonym expansion.
// A label CANNOT be shared by more than one type in scope.
// Field names share the top level namespace with ordinary variables and class methods and must not conflict with other top level names in scope.

Hs::Decls synthesize_field_accessors(const Hs::Decls& decls)
{
    Hs::Decls decls2;

    for(auto& decl: decls)
    {
        if (not decl.is_a<Haskell::DataOrNewtypeDecl>()) continue;
        auto D = decl.as_<Haskell::DataOrNewtypeDecl>();

        auto constrs = D.constructors;
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
                        constructor_fields[unloc(field_name.name)][constr.name] = i;
                        i++;
                    }
                }
                arity[constr.name] = i;
            }
        }

        if (not arity.empty())
        {
            for(auto& [field_name, constrs]: constructor_fields)
            {
                expression_ref name = Hs::Var({noloc,field_name});
                vector<Located<Haskell::Alt>> alts;

                for(auto& [ConName,pos]: constrs)
                {
                    int a = arity[ConName];
                    vector<expression_ref> f(a, Hs::WildcardPattern());
                    f[pos] = name;

                    auto pattern = expression_ref{Hs::Con({noloc,ConName},a),f};
                    auto rhs = Haskell::SimpleRHS({noloc, name});
                    alts.push_back({noloc,{pattern,rhs}});
                }
                // I removed the {_ -> error("{name}: no match")} alternative, since error( ) generates a var( ).
                // This could lead to worse error messages.

                Hs::Var x({noloc,"v$0"}); // FIXME??
                expression_ref body = Haskell::CaseExp(x,Haskell::Alts(alts));
                body = Haskell::LambdaExp({x},body);

                decls2.push_back(Haskell::ValueDecl({noloc,name}, body));
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

Haskell::Type renamer_state::rename_type(const Haskell::Type& type)
{
    if (auto tc = type.to<Haskell::TypeCon>())
    {
        auto& name = unloc(tc->name);
        auto& loc = tc->name.loc;

        if (m.type_is_declared(name))
        {
            auto T = m.lookup_type(name);
            auto& qualified_name = T.name;
            return Haskell::TypeCon({loc, qualified_name});
        }
        else
        {
            if (loc)
                throw myexception()<<"Can't find tycon '"<<name<<"' at "<<*loc;
            else
                throw myexception()<<"Can't find tycon '"<<name<<"'";
        }
    }
    else if (auto tv = type.to<Haskell::TypeVar>())
    {
        auto& name = unloc(tv->name);

        assert(is_haskell_varid(name));

        return type;
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto app = type.as_<Haskell::TypeApp>();
        app.head = rename_type(app.head);
        app.arg  = rename_type(app.arg);
        return app;
    }
    else if (type.is_a<Haskell::TupleType>())
    {
        auto tuple = type.as_<Haskell::TupleType>();
        for(auto& type: tuple.element_types)
            type = rename_type(type);
        return tuple;
    }
    else if (type.is_a<Haskell::ListType>())
    {
        auto list = type.as_<Haskell::ListType>();
        list.element_type = rename_type(list.element_type);
        return list;
    }
    else if (type.is_a<Haskell::ConstrainedType>())
    {
        auto ctype = type.as_<Haskell::ConstrainedType>();
        ctype.context = rename(ctype.context);
        ctype.type = rename_type(ctype.type);
        return ctype;
    }
    else
        throw myexception()<<"rename_type: unrecognized type \""<<type.print()<<"\"";
}

Haskell::Context renamer_state::rename(Haskell::Context context)
{
    for(auto& constraint: context.constraints)
        constraint = rename_type(constraint);
    return context;
}

Haskell::DataOrNewtypeDecl renamer_state::rename(Haskell::DataOrNewtypeDecl decl)
{
    decl.name = m.name + "." + decl.name;

    decl.context = rename(decl.context);

    for(auto& constructor: decl.constructors)
    {
        constructor.name = m.name + "." + constructor.name;

        if (constructor.context)
        {
            for(auto& constraint: constructor.context->constraints)
                constraint = rename_type(constraint);
        }

        if (constructor.is_record_constructor())
        {
            for(auto& field: std::get<1>(constructor.fields).field_decls)
            {
                for(auto& var: field.field_names)
                    unloc(var.name) = m.name + "." + unloc(var.name);
                field.type = rename_type(field.type);
            }
        }
        else
        {
            for(auto& type: std::get<0>(constructor.fields))
                type = rename_type(type);
        }
    }

    return decl;
}

Haskell::InstanceDecl renamer_state::rename(Haskell::InstanceDecl I)
{
    I.context = rename(I.context);
    I.constraint = rename_type(I.constraint);

    // Renaming of the decl group is done in rename_decls

    return I;
}

Haskell::ClassDecl renamer_state::rename(Haskell::ClassDecl decl)
{
    decl.name = m.name + "." + decl.name;
    decl.context = rename(decl.context);
    // Renaming of the decl group is done in rename_decls
    return decl;
}

Haskell::TypeSynonymDecl renamer_state::rename(Haskell::TypeSynonymDecl decl)
{
    decl.name = m.name + "." + decl.name;
    unloc(decl.rhs_type) = rename_type(unloc(decl.rhs_type));
    return decl;
}


Haskell::Decls renamer_state::rename_type_decls(Haskell::Decls decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
            decl = rename(decl.as_<Haskell::ClassDecl>());
        else if (decl.is_a<Haskell::DataOrNewtypeDecl>())
            decl = rename(decl.as_<Haskell::DataOrNewtypeDecl>());
        else if (decl.is_a<Haskell::InstanceDecl>())
            decl = rename(decl.as_<Haskell::InstanceDecl>());
        else if (decl.is_a<Haskell::TypeSynonymDecl>())
            decl = rename(decl.as_<Haskell::TypeSynonymDecl>());
    }

    return decls;
}

Haskell::ModuleDecls rename(const simplifier_options& opts, const Module& m, Haskell::ModuleDecls M)
{
    renamer_state Rn(m);

    M.type_decls = Rn.rename_type_decls(M.type_decls);

    // Find all the names bound HERE, versus in individual decls.

    // The idea is that we only add unqualified names here, and they shadow
    // qualified names.
    bound_var_info bound_names;
    add(bound_names, Rn.find_bound_vars_in_decls(M.value_decls[0], true));
    for(auto& decl: M.type_decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto C = decl.as_<Haskell::ClassDecl>();
            if (C.binds)
            {
                auto& vdecls = unloc(*C.binds)[0];
                add(bound_names, Rn.find_bound_vars_in_decls(vdecls, true));
            }
            decl = C;
        }
        // Wait.. don't we need to discover constructors, too?
    }

    // Replace ids with dummies
    for(auto& decl: M.type_decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto C = decl.as_<Haskell::ClassDecl>();
            if (C.binds)
            {
                auto method_binders = Rn.rename_signatures( unloc(*C.binds).signatures, true);
                add( bound_names, method_binders );
            }
            decl = C;
        }
    }

    set<string> free_vars;

    // Extract sigs for builtins
    for(auto& builtin_decl: M.builtin_decls)
    {
        auto& builtin_name = builtin_decl.function_name;
        auto iter = M.value_decls.signatures.find(builtin_name);
        if (iter == M.value_decls.signatures.end())
        {
            if (opts.typecheck)
                throw myexception()<<"no signature for builtin '"<<builtin_name<<"'";
            continue;
        }
        auto builtin_type = iter->second;

        M.builtin_signatures.insert({builtin_name, builtin_type});

        M.value_decls.signatures.erase(iter);
    }
    add(bound_names, Rn.rename_signatures(M.builtin_signatures, true));
    
    Rn.rename_decls(M.value_decls, bound_names, free_vars, true);

    // Replace ids with dummies
    for(auto& decl: M.type_decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto C = decl.as_<Haskell::ClassDecl>();
            if (C.binds)
            {
                assert(unloc(*C.binds).size() == 1);
                auto& decls = unloc(*C.binds)[0];

                for(auto& mdecl: decls)
                {
                    if (mdecl.is_a<Hs::PatDecl>())
                        throw myexception()<<"Illegal pattern binding in class "<<C.name;
                    auto FD = mdecl.as_<Hs::FunDecl>();
                    FD.match = Rn.rename( FD.match, bound_names, FD.rhs_free_vars);
                    mdecl = FD;
                }
            }
            decl = C;
        }
        else if (decl.is_a<Haskell::InstanceDecl>())
        {
            auto I = decl.as_<Haskell::InstanceDecl>();
            if (I.binds)
            {
                assert(unloc(*I.binds).size() == 1);
                auto& decls = unloc(*I.binds)[0];

                for(auto& mdecl: decls)
                {
                    if (mdecl.is_a<Hs::PatDecl>())
                        throw myexception()<<"Illegal pattern binding in instance "<<I.constraint.print();
                    auto FD = mdecl.as_<Hs::FunDecl>();
                    FD.match = Rn.rename( FD.match, bound_names, FD.rhs_free_vars);
                    mdecl = FD;
                }
            }
            decl = I;
        }
    }

    return M;
}



bound_var_info renamer_state::find_vars_in_patterns(const vector<expression_ref>& pats, bool top)
{
    bound_var_info bound;

    for(auto& pat: pats)
    {
        auto bound_here = find_vars_in_pattern(pat, top);
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

bound_var_info find_vars_in_patterns2(const vector<expression_ref>& pats)
{
    bound_var_info bound;

    for(auto& pat: pats)
        add(bound, find_vars_in_pattern2(pat));

    return bound;
}

bound_var_info find_vars_in_pattern2(const expression_ref& pat)
{
    assert(not is_apply_exp(pat));

    if (pat.is_a<Haskell::WildcardPattern>())
	return {};
    else if (auto lp = pat.to<Haskell::LazyPattern>())
        return find_vars_in_pattern2(lp->pattern);
    else if (auto sp = pat.to<Haskell::StrictPattern>())
        return find_vars_in_pattern2(sp->pattern);
    else if (auto ap = pat.to<Haskell::AsPattern>())
	return plus( find_vars_in_pattern2(ap->var), find_vars_in_pattern2(ap->pattern) );
    else if (auto l = pat.to<Haskell::List>())
        return find_vars_in_patterns2(l->elements);
    else if (auto t = pat.to<Haskell::Tuple>())
        return find_vars_in_patterns2(t->elements);
    else if (auto v = pat.to<Haskell::Var>())
	return { unloc(v->name) };
    else if (pat.head().is_a<Haskell::Con>())
        return find_vars_in_patterns2(pat.copy_sub());
    else if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double())
        return {};
    else if (pat.is_a<Hs::Literal>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

bound_var_info renamer_state::find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top)
{
    if (auto d = decl.to<Haskell::PatDecl>())
        return find_vars_in_pattern( unloc(d->lhs), top);
    else if (auto d = decl.to<Haskell::FunDecl>())
        return find_vars_in_pattern(d->v, top);
    else
        std::abort();
}

bound_var_info renamer_state::find_bound_vars_in_decls(const Haskell::Decls& decls, bool top)
{
    // The idea is that we only add unqualified names here, and they shadow
    // qualified names.
    bound_var_info bound_names;
    for(auto& decl: decls)
        add(bound_names, find_bound_vars_in_funpatdecl(decl, top));

    return bound_names;
}

bound_var_info renamer_state::find_bound_vars_in_decls(const Haskell::Binds& binds, bool top)
{
    bound_var_info bound_names;
    for(auto& decls: binds)
        add(bound_names, find_bound_vars_in_decls(decls, top));

    for(auto& [name,_]: binds.signatures)
        add(bound_names, find_vars_in_pattern(Hs::Var({noloc,name}), top));

    return bound_names;
}

bound_var_info renamer_state::find_bound_vars_in_decl(const Haskell::ValueDecl& decl, bool top)
{
    // For a constructor pattern, rename the whole lhs.
    if (is_pattern_binding(decl))
    {
        return find_vars_in_pattern( unloc(decl.lhs), top);
    }
    // For a function pattern, just rename the variable being defined
    else
    {
        auto head = unloc(decl.lhs).head();
        return find_vars_in_pattern(head, top);
    }
}

bound_var_info renamer_state::find_bound_vars_in_decl(const Haskell::SignatureDecl& decl, bool is_top_level)
{
    bound_var_info bound_names;

    for(auto& var: decl.vars)
    {
        auto name = unloc(var.name);
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

map<string,int> get_indices_for_names(const Hs::Decls& decls)
{
    map<string,int> index_for_name;

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (decl.is_a<Hs::FunDecl>())
        {
            auto& FD = decl.as_<Hs::FunDecl>();
            const string& name = unloc(FD.v.name);

            if (index_for_name.count(name)) throw myexception()<<"name '"<<name<<"' is bound twice: "<<decls.print();

            index_for_name.insert({name, i});
        }
        else if (decl.is_a<Hs::PatDecl>())
        {
            auto& PD = decl.as_<Hs::PatDecl>();
            for(const string& name: find_vars_in_pattern2( unloc(PD.lhs) ))
            {
                if (index_for_name.count(name)) throw myexception()<<"name '"<<name<<"' is bound twice: "<<decls.print();

                index_for_name.insert({name, i});
            }
        }
        else
            std::abort();
    }

    return index_for_name;
}

bound_var_info renamer_state::find_bound_vars_in_stmt(const expression_ref& stmt)
{
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

Hs::MultiGuardedRHS renamer_state::rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
{
    set<string> rhs_free_vars;
    auto R2 = rename(R, plus(bound, binders), rhs_free_vars);
    add(free_vars, minus(rhs_free_vars, binders));
    return R2;
}

pair<expression_ref,set<string>> renamer_state::rename(const expression_ref& E, const bound_var_info& bound)
{
    set<string> free_vars;
    auto E2 = rename(E, bound, free_vars);
    return {E2,free_vars};
}

expression_ref
renamer_state::rename(const expression_ref& E, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
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

