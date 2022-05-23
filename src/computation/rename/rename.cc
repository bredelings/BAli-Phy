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
#include "computation/expression/list.H"
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


expression_ref infix_parse(const Module& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
expression_ref infix_parse_neg(const Module& m, const symbol_info& op1, deque<expression_ref>& T)
{
    assert(not T.empty());

    expression_ref E1 = T.front();
    T.pop_front();

    // We are starting with a Neg
    if (E1.head().is_a<Hs::Neg>())
    {
	if (op1.fixity.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

	E1 = infix_parse_neg(m, symbol_info("-",variable_symbol, {}, 2,{left_fix,6}), T);

	return infix_parse(m, op1, {Hs::Var({noloc,"negate"}),E1}, T);
    }
    // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
    else
	return infix_parse(m, op1, E1, T);
}

// FIXME: just return the fixity
symbol_info get_op_sym(const Module& m, const expression_ref& O)
{
    symbol_info op_sym;
    string name;
    if (auto v = O.to<Haskell::Var>())
        name = unloc(v->name);
    else if (auto c = O.to<Haskell::Con>())
        name = unloc(c->name);
    else
        std::abort();

    if (m.is_declared( name ) )
	op_sym = m.get_operator( name );
    else
    {
	// FIXME: if this name is simply never declared, we should warn here.
	op_sym.name = name;
	op_sym.fixity = {left_fix,9};
    }

    return op_sym;
}

// FIXME: "h:t!!0 = h" gives an error that says that the arity of ":" is wrong.
// We get ":" "h" "!!" "t" 0 as a pattern...
//   This seems to be a result of the hack in unapply

/// Expression is of the form ... op1 E1 [op2 ...]. Get right operand of op1.
expression_ref infix_parse(const Module& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T)
{
    if (T.empty())
	return E1;

    expression_ref op2_E = T.front();
    symbol_info op2 = get_op_sym(m, op2_E);

    // illegal expressions
    if (op1.fixity.precedence == op2.fixity.precedence and (op1.fixity.fixity != op2.fixity.fixity or op1.fixity.fixity == non_fix))
	throw myexception()<<"Must use parenthesis to order operators '"<<op1.name<<"' and '"<<op2.name<<"'";

    // left association: ... op1 E1) op2 ...
    if (op1.fixity.precedence > op2.fixity.precedence or (op1.fixity.precedence == op2.fixity.precedence and op1.fixity.fixity == left_fix))
	return E1;

    // right association: .. op1 (E1 op2 {...E3...}) ...
    else
    {
	T.pop_front();
	expression_ref E3 = infix_parse_neg(m, op2, T);

	expression_ref E1_op2_E3 = {op2_E, E1, E3};

	return infix_parse(m, op1, E1_op2_E3, T);
    }
}

expression_ref desugar_infix(const Module& m, const vector<expression_ref>& T)
{
    deque<expression_ref> T2;
    T2.insert(T2.begin(), T.begin(), T.end());

    return infix_parse_neg(m, {"",variable_symbol,{}, 2,{non_fix,-1}}, T2);
}

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
    auto& head = decl.lhs.head();
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
    return decl.lhs.head().as_<var>().name;
}

bool is_pattern_binding(const Haskell::ValueDecl& decl)
{
    if (decl.lhs.is_a<Haskell::List>())
        return true;
    if (decl.lhs.is_a<Haskell::Tuple>())
        return true;
    if (decl.lhs.is_a<Haskell::AsPattern>())
        return true;
    if (decl.lhs.is_a<Haskell::LazyPattern>())
        return true;
    if (decl.lhs.is_a<Haskell::StrictPattern>())
        return true;
    // FIXME: we should make a ConPattern
    if (decl.lhs.head().is_a<Haskell::Con>())
        return true;
    if (decl.lhs.head().is_a<constructor>()) // this happens when called from desugar.cc
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
expression_ref rename_infix(const Module& m, const expression_ref& E);
Hs::MultiGuardedRHS rename_infix(const Module& m, Hs::MultiGuardedRHS R);


expression_ref rename_infix_decl(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::ValueDecl>())
    {
        auto D = E.as_<Haskell::ValueDecl>();

        D.lhs = rename_infix(m, D.lhs);
	D.lhs = unapply(D.lhs);
        D.rhs = rename_infix(m, D.rhs);

	assert(D.lhs.head().is_a<Hs::Var>() or is_pattern_binding(D));

        return D;
    }
    else if (E.is_a<Hs::SignatureDecl>())
        return E;
    else if (E.is_a<Hs::FixityDecl>())
        return E;
    else
        std::abort();
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

expression_ref rename_infix(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = rename_infix(m, element);
        return L;
    }
    else if (E.is_a<Haskell::ListFrom>())
    {
        auto L = E.as_<Haskell::ListFrom>();
        L.from = rename_infix(m, L.from);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThen>())
    {
        auto L = E.as_<Haskell::ListFromThen>();
        L.from = rename_infix(m, L.from);
        L.then = rename_infix(m, L.then);
        return L;
    }
    else if (E.is_a<Haskell::ListFromTo>())
    {
        auto L = E.as_<Haskell::ListFromTo>();
        L.from = rename_infix(m, L.from);
        L.to   = rename_infix(m, L.to);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThenTo>())
    {
        auto L = E.as_<Haskell::ListFromThenTo>();
        L.from = rename_infix(m, L.from);
        L.then = rename_infix(m, L.then);
        L.to   = rename_infix(m, L.to);
        return L;
    }
    else if (E.is_a<Haskell::ListComprehension>())
    {
        auto L = E.as_<Haskell::ListComprehension>();
        L.body = rename_infix(m, L.body);
        for(auto& qual: L.quals)
            qual = rename_infix(m, qual);
        return L;
    }
    else if (E.is_a<Haskell::LeftSection>())
    {
        auto S = E.as_<Haskell::LeftSection>();
        S.l_arg = rename_infix(m, S.l_arg);
        return S;
    }
    else if (E.is_a<Haskell::RightSection>())
    {
        auto S = E.as_<Haskell::RightSection>();
        S.r_arg = rename_infix(m, S.r_arg);
        return S;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = rename_infix(m, element);
        return T;
    }
    else if (E.is_a<Haskell::PatQual>())
    {
        auto PQ = E.as_<Haskell::PatQual>();

        PQ.bindpat = rename_infix(m, PQ.bindpat);
        PQ.bindpat = unapply(PQ.bindpat);

        PQ.exp = rename_infix(m, PQ.exp);

        return PQ;
    }
    else if (E.is_a<Haskell::SimpleQual>())
    {
        auto SQ = E.as_<Haskell::SimpleQual>();
        SQ.exp = rename_infix(m, SQ.exp);
        return SQ;
    }
    else if (E.is_a<Haskell::LetQual>())
    {
        auto LQ = E.as_<Haskell::LetQual>();
        unloc(LQ.binds) = rename_infix(m, unloc(LQ.binds));
        return LQ;
    }
    else if (E.is_a<Haskell::AsPattern>())
    {
        auto& AP = E.as_<Haskell::AsPattern>();
        return Haskell::AsPattern(AP.var, rename_infix(m,AP.pattern));
    }
    else if (E.is_a<Haskell::LazyPattern>())
    {
        auto LP = E.as_<Haskell::LazyPattern>();
        return Haskell::LazyPattern(rename_infix(m,LP.pattern));
    }
    else if (E.is_a<Haskell::StrictPattern>())
    {
        auto SP = E.as_<Haskell::StrictPattern>();
        SP.pattern = rename_infix(m, SP.pattern);
        return SP;
    }
    else if (E.is_a<Haskell::RecStmt>())
    {
        auto R = E.as_<Haskell::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            stmt = rename_infix(m, stmt);
        return R;
    }
    else if (E.is_a<Haskell::Do>())
    {
        auto D = E.as_<Haskell::Do>();
        for(auto& stmt: D.stmts.stmts)
            stmt = rename_infix(m, stmt);
        return D;
    }
    else if (E.is_a<Haskell::MDo>())
    {
        throw myexception()<<"mdo is not handled yet!";
        auto D = E.as_<Haskell::MDo>();
        for(auto& stmt: D.stmts.stmts)
            stmt = rename_infix(m, stmt);
        return D;
    }
    else if (E.is_a<Haskell::LambdaExp>())
    {
        auto L = E.as_<Haskell::LambdaExp>();
        for(auto& arg: L.args)
            arg = unapply(rename_infix(m, arg));
        L.body = rename_infix(m, L.body);

        return L;
    }
    else if (E.is_a<Haskell::LetExp>())
    {
        auto L = E.as_<Haskell::LetExp>();

        unloc(L.binds) = rename_infix(m, unloc(L.binds));
        unloc(L.body)  = rename_infix(m, unloc(L.body));

        return L;
    }
    else if (E.is_a<Haskell::IfExp>())
    {
        auto I = E.as_<Haskell::IfExp>();
        unloc(I.condition) = rename_infix(m, unloc(I.condition));
        unloc(I.true_branch) = rename_infix(m, unloc(I.true_branch));
        unloc(I.false_branch) = rename_infix(m, unloc(I.false_branch));
        return I;
    }
    else if (E.is_a<Haskell::CaseExp>())
    {
        auto C = E.as_<Haskell::CaseExp>();

        C.object = rename_infix(m, C.object);

        for(auto& alt: C.alts)
        {
            unloc(alt).pattern = rename_infix(m, unloc(alt).pattern);
            unloc(alt).pattern = unapply(unloc(alt).pattern);
            unloc(alt).rhs = rename_infix(m, unloc(alt).rhs);
        }

        return C;
    }
    else if (E.is_int() or E.is_log_double() or E.is_double() or E.is_char())
    {
        return E;
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename_infix(m, TE.exp);
        // Nothing to do for TE.type, since there are no type operators unless extensions are enabled.
        return TE;
    }
    else if (auto I = E.to<Hs::InfixExp>())
    {
        auto terms = I->terms;
        for(auto& term: terms)
            term = rename_infix(m, term);
	return desugar_infix(m, terms);
    }
    else if (is_apply(E.head()))
    {
        auto v = E.sub();

        for(auto& e: v)
            e = rename_infix(m, e);

	expression_ref E2;
	if (is_apply(v[0].head()))
	{
	    E2 = v[0];
	    for(int i=1;i<v.size();i++)
		E2 = E2 + v[i];
	}
	else
	{
	    E2 = expression_ref{E.head(),v};
	}
	assert(is_apply(E2.head()));
	assert(not is_apply(E2.sub()[0].head()));
	return E2;
    }
    else if (E.is_a<Hs::WildcardPattern>())
        return E;
    else if (E.is_a<Hs::Var>())
        return E;
    else if (E.is_a<Hs::Con>())
        return E;
    else if (E.head().is_a<Hs::Con>())
    {
        auto v = E.sub();

        for(auto& e: v)
            e = rename_infix(m, e);

        return expression_ref{E.head(),v};
    }
    else if (E.head().is_a<Hs::Neg>())
        return E;
    else
        std::abort();
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

                decls2.push_back(Haskell::ValueDecl(name, body));
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

typedef set<string> bound_type_var_info;

struct renamer_state
{
    const Module& m;

//    int var_index=0;
//
//    var get_fresh_wildcard() { return var(-var_index++);}
//    var get_fresh_var() { return var(var_index++);}
//    var get_fresh_var(const string& name) {return var(name,var_index++);}

    bound_var_info rename_patterns(vector<expression_ref>& pat, bool top = false);
    bound_var_info rename_pattern(expression_ref& pat, bool top = false);

    bound_var_info find_vars_in_patterns(const vector<expression_ref>& pats, bool top = false);
    bound_var_info find_vars_in_pattern(const expression_ref& pat, bool top = false);
    // the pattern*2 versions are for AFTER rename, and don't check things.  They just report what they find.
    bound_var_info find_bound_vars_in_stmt(const expression_ref& stmt);

    // these all assume decls have been translated to FunDecl or PatDecl
    bound_var_info find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Haskell::Decls& decls, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Haskell::Binds& decls, bool top = false);

    bound_var_info find_bound_vars_in_decl(const Haskell::ValueDecl& decl, bool top = false);
    bound_var_info find_bound_vars_in_decl(const Haskell::SignatureDecl& decl, bool top = false);

    vector<vector<int>> rename_grouped_decls(Haskell::Decls& decls, const bound_var_info& bound, set<string>& free_vars, bool top = false);
    bound_var_info rename_signatures(std::map<std::string, Hs::Type>& signatures, bool top = false);
    bound_var_info rename_decls(Haskell::Binds& decls, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars, bool top = false);
    bound_var_info rename_decls(Haskell::Binds& decls, const bound_var_info& bound, set<string>& free_vars, bool top = false);
    bound_var_info rename_rec_stmt(expression_ref& stmt, const bound_var_info& bound, set<string>& free_vars);
    bound_var_info rename_stmt(expression_ref& stmt, const bound_var_info& bound, set<string>& free_vars);
    bound_var_info rename_stmt(expression_ref& stmt, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars);
    Hs::MultiGuardedRHS rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars);
    Hs::MultiGuardedRHS rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, set<string>& free_vars);
    Hs::Match rename(Hs::Match match, const bound_var_info& bound, set<string>& free_vars);

    Haskell::Decls rename_type_decls(Haskell::Decls decls);
    Haskell::InstanceDecl rename(Haskell::InstanceDecl);
    Haskell::ClassDecl rename(Haskell::ClassDecl);
    Haskell::TypeSynonymDecl rename(Haskell::TypeSynonymDecl);
    Haskell::DataOrNewtypeDecl rename(Haskell::DataOrNewtypeDecl);
    Haskell::Type rename_type(const Haskell::Type&);
    Haskell::Context rename(Haskell::Context);

    pair<expression_ref,set<string>> rename(const expression_ref& E, const bound_var_info& bound);
    expression_ref rename(const expression_ref& E, const bound_var_info& bound, set<string>& free_vars);
    expression_ref rename(const expression_ref& E, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars);

    renamer_state(const Module& m_):m(m_) {}
};

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

Haskell::ModuleDecls rename(const Module& m, Haskell::ModuleDecls M)
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
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
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
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
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
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

bound_var_info renamer_state::find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top)
{
    if (auto d = decl.to<Haskell::PatDecl>())
        return find_vars_in_pattern(d->lhs, top);
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
        return find_vars_in_pattern(decl.lhs, top);
    }
    // For a function pattern, just rename the variable being defined
    else
    {
        auto head = decl.lhs.head();
        return find_vars_in_pattern(head, top);
    }
}

bound_var_info binders_for_renamed_decl(const expression_ref& decl)
{
    set<string> binders;
    if (auto pd = decl.to<Hs::PatDecl>())
        binders = find_vars_in_pattern2(pd->lhs);
    else if (auto fd = decl.to<Hs::FunDecl>())
        binders = { unloc(fd->v.name) };
    else
        std::abort();
    return binders;
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
            for(const string& name: find_vars_in_pattern2(PD.lhs))
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

Hs::Match renamer_state::rename(Hs::Match match, const bound_var_info& bound, set<string>& free_vars)
{
    for(auto& mrule: match.rules)
    {
        bound_var_info binders;

        for(auto& arg: mrule.patterns)
        {
            auto new_binders = rename_pattern(arg);
            auto overlap = intersection(binders, new_binders);
            if (not overlap.empty())
            {
                string bad = *overlap.begin();
                throw myexception()<<"Function declaration uses variable '"<<bad<<"' twice:\n"<<" "<<mrule.print();
            }
            add(binders, new_binders);
        }

        mrule.rhs = rename(mrule.rhs, bound, binders, free_vars);
    }

    return match;
}


// So... factor out rename_grouped_decl( ), and then make a version that splits into components, and a version that does not?
// Splitting the decls for classes and instances into  components really doesn't make sense...

vector<vector<int>> renamer_state::rename_grouped_decls(Haskell::Decls& decls, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    // NOTE: bound already includes the binder names.

    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];

        if (decl.is_a<Hs::PatDecl>())
        {
            auto PD = decl.as_<Hs::PatDecl>();

            rename_pattern(PD.lhs, top);
            PD.rhs = rename(PD.rhs, bound, PD.rhs_free_vars);
            decl = PD;
        }
        else if (decl.is_a<Hs::FunDecl>())
        {
            auto FD = decl.as_<Hs::FunDecl>();
            auto& name = unloc(FD.v.name);
            assert(not is_qualified_symbol(name));
            if (top)
                name = m.name + "." + name;

            FD.match = rename(FD.match, bound, FD.rhs_free_vars);

            decl = FD;
        }
        else
            std::abort();
    }

    // Map the names to indices
    map<string,int> index_for_name = get_indices_for_names(decls);

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

optional<Hs::Var> fundecl_head(const expression_ref& decl)
{
    assert(decl.is_a<Hs::SignatureDecl>() or decl.is_a<Hs::FixityDecl>() or decl.is_a<Hs::ValueDecl>());

    if (auto d = decl.to<Hs::ValueDecl>(); d and is_function_binding(*d))
    {
        auto fvar = d->lhs.head();
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
            decls2.push_back(Haskell::PatDecl{d->lhs, d->rhs});
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Match m;
            for(int j=i;j<decls.size();j++)
            {
                if (fundecl_head(decls[j]) != fvar) break;

                auto& D = decls[j].as_<Haskell::ValueDecl>();

                m.rules.push_back( Hs::MRule{ D.lhs.copy_sub(), D.rhs } );

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

bound_var_info renamer_state::rename_decls(Haskell::Binds& binds, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    assert(binds.size() == 1);
    auto& decls = binds[0];

    auto binders = find_bound_vars_in_decls(decls, top);

    auto sig_binders = rename_signatures(binds.signatures, top);
    /*
      This doesn't work yet because BuiltinDecls are not found.
    for(auto& sig_binder: sig_binders)
        if (not binders.count(sig_binder))
            throw myexception()<<"Signature but no definition for '"<<sig_binder<<"'";
    */
    
    set<string> decls_free_vars;
    auto refs = rename_grouped_decls(decls, plus(bound, binders), decls_free_vars, top);
    group_binds(binds, refs);

    add(free_vars, minus(decls_free_vars,binders));

    return binders;
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

        /*
         * See "Recursive binding groups" in https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
         *
         * "Like let and where bindings, name shadowing is not allowed within an mdo-expression or a rec-block"
         *
         * As an example:           ===>
         *   rec { b <- f a c              (b,c) <- mfix (\ ~(b,c) -> do { b <- f a c
         *       ; c <- f b a }                                          ; c <- f b a
         *                                                               ; return (b,c) } )
         *
         * See ghc/compiler/rename/RnExpr.hs
         */

// Here we want to find all the variables bound by the list of stmts, and make sure that they don't overlap.
// Getting the list of variables bound by a "rec" should return all the variables bound by the statements inside the rec.
bound_var_info renamer_state::rename_rec_stmt(expression_ref& rec_stmt, const bound_var_info& bound, set<string>& free_vars)
{
    bound_var_info rec_bound;
    for(auto& stmt: rec_stmt.as_<Haskell::RecStmt>().stmts.stmts)
    {
        bool overlap = not disjoint_add(rec_bound, find_bound_vars_in_stmt(stmt));
	if (overlap)
	    throw myexception()<<"rec command '"<<rec_stmt<<"' uses a variable twice!";
    }
    // 2. Construct the tuple
    vector<expression_ref> vars;
    for(auto& var_name: rec_bound)
        vars.push_back(Hs::Var({noloc,var_name}));
    expression_ref rec_tuple;
    if (vars.size() == 1)
        rec_tuple = vars[0];
    else
    {
        rec_tuple = Hs::Con({noloc, tuple_head(vars.size()).name()},vars.size());
        for(auto var: vars)
            rec_tuple = {rec_tuple, var};
    }

    // 3. Construct the do stmt
    auto stmts = rec_stmt.as_<Haskell::RecStmt>().stmts.stmts;
    expression_ref rec_return = Hs::Var({noloc, "return"});
    expression_ref rec_return_stmt = {rec_return, rec_tuple};
    stmts.push_back(Hs::SimpleQual(rec_return_stmt));
    auto rec_do = Haskell::Do(Haskell::Stmts(stmts));

    // 4. Construct the lambda function
    expression_ref rec_tuple_pattern = unapply(rec_tuple); // This makes the tuple expression into a pattern by translating `@ (@ ((,) x))` into `(,) x y`
    expression_ref rec_lambda = Haskell::LambdaExp({Haskell::LazyPattern(rec_tuple_pattern)}, rec_do);      // \ ~(b,c) -> do { ... }

    // 5. Construct rec_tuple_pattern <- mfix rec_lambda
    expression_ref mfix = Hs::Var({noloc, "mfix"});
    rec_stmt = Haskell::PatQual(rec_tuple_pattern, expression_ref{mfix, rec_lambda});

    // Combine the set of bound variables and rename our rewritten statement;
    return rename_stmt(rec_stmt, bound, rec_bound, free_vars);
}

bound_var_info
renamer_state::rename_stmt(expression_ref& stmt, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
{
    set<string> stmt_free_vars;
    auto new_binders = rename_stmt(stmt, plus(bound, binders), stmt_free_vars);
    add(free_vars, minus(stmt_free_vars, binders));
    return new_binders;
}

bound_var_info renamer_state::rename_stmt(expression_ref& stmt, const bound_var_info& bound, set<string>& free_vars)
{
    if (stmt.is_a<Hs::SimpleQual>())
    {
        auto SQ = stmt.as_<Hs::SimpleQual>();
	SQ.exp = rename(SQ.exp, bound, free_vars);
        stmt = SQ;
	return {};
    }
    else if (stmt.is_a<Haskell::PatQual>())
    {
        auto PQ = stmt.as_<Haskell::PatQual>();
	PQ.exp = rename(PQ.exp, bound, free_vars);
	auto bound_vars = rename_pattern(PQ.bindpat);
        stmt = PQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::LetQual>())
    {
        auto LQ = stmt.as_<Haskell::LetQual>();
	auto bound_vars = rename_decls(unloc(LQ.binds), bound, {}, free_vars);
	stmt = LQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::RecStmt>())
    {
        return rename_rec_stmt(stmt, bound, free_vars);
    }
    else
	std::abort();
}

Hs::MultiGuardedRHS renamer_state::rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, set<string>& free_vars)
{
    bound_var_info binders;

    if (R.decls)
        binders = rename_decls(unloc(*R.decls), bound, binders, free_vars);

    for(auto& guarded_rhs: R.guarded_rhss)
    {
        for(auto& guard: guarded_rhs.guards)
            add(binders, rename_stmt(guard, bound, binders, free_vars));

        guarded_rhs.body = rename(guarded_rhs.body, bound, binders, free_vars);
    }

    return R;
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

expression_ref renamer_state::rename(const expression_ref& E, const bound_var_info& bound, set<string>& free_vars)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = rename(element, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFrom>())
    {
        auto L = E.as_<Haskell::ListFrom>();
        L.from = rename(L.from, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThen>())
    {
        auto L = E.as_<Haskell::ListFromThen>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFromTo>())
    {
        auto L = E.as_<Haskell::ListFromTo>();
        L.from = rename(L.from, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThenTo>())
    {
        auto L = E.as_<Haskell::ListFromThenTo>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListComprehension>())
    {
        auto L = E.as_<Haskell::ListComprehension>();

        bound_var_info binders;
        for(auto& qual: L.quals)
            add(binders, rename_stmt(qual, bound, binders, free_vars));

        L.body = rename(L.body, bound, binders, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::LeftSection>())
    {
        auto S = E.as_<Haskell::LeftSection>();
        S.l_arg = rename(S.l_arg, bound, free_vars);
        S.op = rename(S.op, bound, free_vars);
        return S;
    }
    else if (E.is_a<Haskell::RightSection>())
    {
        auto S = E.as_<Haskell::RightSection>();
        S.op = rename(S.op, bound, free_vars);
        S.r_arg = rename(S.r_arg, bound, free_vars);
        return S;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = rename(element, bound, free_vars);
        return T;
    }
    else if (E.is_a<Hs::Var>())
    {
        auto V = E.as_<Hs::Var>();
        auto& name = unloc(V.name);
        auto& loc = V.name.loc;

        // Local vars bind id's tighter than global vars.
        if (includes(bound,name))
        {
            free_vars.insert(name);
            return E;
        }
        // If the variable is free, then try top-level names.
        else if (m.is_declared(name))
        {
            const symbol_info& S = m.lookup_symbol(name);
            string qualified_name = S.name;
            name = qualified_name;
            if (get_module_name(qualified_name) == m.name)
                free_vars.insert(qualified_name);
            return V;
        }
        else
        {
            if (loc)
                throw myexception()<<"Can't find id '"<<name<<"' at "<<*loc;
            else
                throw myexception()<<"Can't find id '"<<name<<"'";
        }
    }
    else if (E.is_a<Hs::Con>())
    {
        auto C = E.as_<Haskell::Con>();
        auto& name = unloc(C.name);
        auto& loc = C.name.loc;

        // FIXME: we should look the constructor up in a constructor environment
        // Does that mean that we look up constructors in a different table?
        if (m.is_declared(name))
        {
            const symbol_info& S = m.lookup_symbol(name);
            name = S.name; // use the qualified name
            // We return a reference to a lambda function, in case the constructor isn't fully applied.
            C.arity = S.arity;
            return C;
        }
        else
        {
            if (loc)
                throw myexception()<<"Can't find id '"<<name<<"' at "<<*loc;
            else
                throw myexception()<<"Can't find id '"<<name<<"'";
        }
    }
    else if (E.is_a<Haskell::RecStmt>())
    {
        bound_var_info binders;
        auto R = E.as_<Haskell::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        return R;
    }
    else if (E.is_a<Haskell::Do>())
    {
        bound_var_info binders;
        auto D = E.as_<Haskell::Do>();
        for(auto& stmt: D.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        return D;
    }
    else if (E.is_a<Haskell::MDo>())
    {
            /*
         * See "The mdo notation" in https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
         *
         * "Like let and where bindings, name shadowing is not allowed within an mdo-expression or a rec-block"
         *
         * mdo { a <- getChar      ===> do { a <- getChar
         *     ; b <- f a c                ; rec { b <- f a c
         *     ; c <- f b a                ;     ; c <- f b a }
         *     ; z <- h a b                ; z <- h a b
         *     ; d <- g d e                ; rec { d <- g d e
         *     ; e <- g a z                ;     ; e <- g a z }
         *     ; putChar c }               ; putChar c }
         */

        /*
         * See ghc/compiler/rename/RnExpr.hs: Note [Segmenting mdo]
         * What does rec {a;c  mean here?
         *                b;d}
         * I think it means rec {a;c;b;d}, but emphasizes that we have
         * to issue all the stmts from the first recursive group a;c, before
         * we issue any of the stmts from the second group b;d}.
         */

        // Hmm... as a dumb segmentation, we could take all the stmts except the last one and put them in a giant rec...
        // FIXME: implement segmentation, and insert recs.

        bound_var_info binders;
        auto MD = E.as_<Haskell::MDo>();
        for(auto& stmt: MD.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        return MD;
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename(TE.exp, bound, free_vars);
        TE.type = rename_type(TE.type);
        return TE;
    }
    else if (E.is_a<Haskell::CaseExp>())
    {
        auto C = E.as_<Haskell::CaseExp>();

        C.object = rename(C.object, bound, free_vars);

        for(auto& alt: C.alts)
        {
            // Rename pattern and get binders
            auto binders = rename_pattern(unloc(alt).pattern);

            // Rename rhs
            unloc(alt).rhs = rename(unloc(alt).rhs, bound, binders, free_vars);
        }

        return C;
    }
    else if (E.is_a<Haskell::LambdaExp>())
    {
        auto L = E.as_<Haskell::LambdaExp>();

        // 1. Rename patterns for lambda arguments
        bound_var_info binders;
        for(auto& arg: L.args)
            add(binders, rename_pattern(arg));

        // 2. Rename the body
        L.body = rename(L. body, bound, binders, free_vars);

        return L;
    }
    else if (E.is_a<Haskell::LetExp>())
    {
        auto L = E.as_<Haskell::LetExp>();

        auto binders = rename_decls(unloc(L.binds), bound, free_vars);
        unloc(L.body) = rename(unloc(L.body), bound, binders, free_vars);

        return L;
    }
    else if (E.is_a<Haskell::IfExp>())
    {
        auto I = E.as_<Haskell::IfExp>();

        unloc(I.condition)    = rename(unloc(I.condition), bound, free_vars);
        unloc(I.true_branch)  = rename(unloc(I.true_branch), bound, free_vars);
        unloc(I.false_branch) = rename(unloc(I.false_branch), bound, free_vars);

        return I;
    }
    else if (E.is_int() or E.is_log_double() or E.is_double() or E.is_char())
    {
        return E;
    }
    else if (is_apply(E.head()))
    {
        vector<expression_ref> v = E.copy_sub();

        for(auto& e: v)
            e = rename(e, bound, free_vars);

        // Apply fully-applied multi-argument constructors during rename.
        //
        // If we don't we get the following problem in simplification, so it takes too many rounds:
        // (:) x y => (\a b -> a:b) x y => let a=x;b=y in a:b => let a=3 in a:y (if a=3 in a containing let).
        //
        // We would really just like (:) x y to resolve to x:y.  But since it doesn't, lets do that here.
        // Could we make a special case for lambdas that are (say) fully apply only to variables?
        // Perhaps we should handle this in the simplifier instead, in the place where we do application of functions.
        //
        // For the meantime, we have this.  And it at least leads to more readable output from rename.
        // It could conceivably help the type-checker also...
        //

        // We don't do this for single-argument constructors. By leaving them as vars, we avoid
        //   getting many let-allocated copies of (), [], True, False, Nothing, etc.
        if (v[0].is_a<Hs::Con>())
        {
            auto C = v[0].as_<Hs::Con>();
            auto& id = unloc(C.name);
            const symbol_info& S = m.lookup_resolved_symbol(id);
            assert(S.symbol_type == constructor_symbol);
            // If the constructor is fully applied, then do the apply now -- this might avoid some rounds of simplification?
            if (v.size() == 1 + S.arity)
            {
                shift_list(v);
                assert(v.size());
                id = S.name;
                C.arity = S.arity;
                return expression_ref{C,v};
            }
        }
        if (E.size())
            return expression_ref{E.head(),v};
        else
            return E;
    }

    std::abort();
}
