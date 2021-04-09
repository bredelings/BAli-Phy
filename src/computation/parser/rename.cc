#include <string>
#include <vector>
#include <set>
#include <map>
#include <deque>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/constructor.H"
#include "computation/expression/AST_node.H"
#include "computation/parser/haskell.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;
using std::deque;

// 1. Some part of the routines in expression/let.cc work on Haskell::LetExp,
//    and some part work on the desugared (let_obj()) or executable (Let())
//    objects.  We need to seprate these...

// 2. Convert let_obj() and Let (the operation) into full objects that don't
//    use AST nodes or expression_refs, at least for the let part.

// 3. Convert Located<ID> to Located<Var>


expression_ref infix_parse(const Module& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
expression_ref infix_parse_neg(const Module& m, const symbol_info& op1, deque<expression_ref>& T)
{
    assert(not T.empty());

    expression_ref E1 = T.front();
    T.pop_front();

    // We are starting with a Neg
    if (E1.head() == AST_node("neg"))
    {
	if (op1.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

	E1 = infix_parse_neg(m, symbol_info("-",variable_symbol, 2,6,left_fix), T);

	return infix_parse(m, op1, {Located<Hs::ID>({},"negate"),E1}, T);
    }
    // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
    else
	return infix_parse(m, op1, E1, T);
}

symbol_info get_op_sym(const Module& m, const expression_ref& O)
{
    if (not O.is_a<Located<Hs::ID>>())
	throw myexception()<<"Can't use expression '"<<O.print()<<"' as infix operator.";

    symbol_info op_sym;
    auto name = unloc(O.as_<Located<Hs::ID>>()).name;

    if (m.is_declared( name ) )
	op_sym = m.get_operator( name );
    else
    {
	// FIXME: if this name is simply never declared, we should warn here.
	op_sym.name = name;
	op_sym.precedence = 9;
	op_sym.fixity = left_fix;
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
    if (op1.precedence == op2.precedence and (op1.fixity != op2.fixity or op1.fixity == non_fix))
	throw myexception()<<"Must use parenthesis to order operators '"<<op1.name<<"' and '"<<op2.name<<"'";

    // left association: ... op1 E1) op2 ...
    if (op1.precedence > op2.precedence or (op1.precedence == op2.precedence and op1.fixity == left_fix))
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

    return infix_parse_neg(m, {"",variable_symbol,2,-1,non_fix}, T2);
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
    else if (E.is_a<Located<Hs::ID>>())
    {
        auto& value = unloc(E.as_<Located<Hs::ID>>()).name;
	if (not is_haskell_con_name(value))
	    return {value};
    }
    return {};
}

string get_func_name(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));
    auto& lhs = decl.sub()[0];
    assert(lhs.head().is_a<Located<Hs::ID>>());
    return unloc(lhs.head().as_<Located<Hs::ID>>()).name;
}

string desugar_get_func_name(const expression_ref& decl)
{
    auto& lhs = decl.sub()[0];
    return lhs.head().as_<var>().name;
}

bool is_pattern_binding(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));
    if (decl.sub()[0].is_a<Haskell::List>())
        return true;
    if (decl.sub()[0].is_a<Haskell::Tuple>())
        return true;
    return is_haskell_con_name(get_func_name(decl));
}

bool is_function_binding(const expression_ref& decl)
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

expression_ref rename_infix(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::ClassDecl>())
    {
        auto C = E.as_<Haskell::ClassDecl>();
        if (C.decls)
            unloc(*C.decls) = rename_infix(m, unloc(*C.decls));
        return C;
    }
    else if (E.is_a<Haskell::InstanceDecl>())
    {
        auto I = E.as_<Haskell::InstanceDecl>();
        if (I.decls)
            unloc(*I.decls) = rename_infix(m, unloc(*I.decls));
        return I;
    }
    else if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = rename_infix(m, element);
        return L;
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
    else if (E.is_a<Haskell::Alts>())
    {
        auto A = E.as_<Haskell::Alts>();
        for(auto& alt: A.alts)
        {
            unloc(alt).pattern = rename_infix(m, unloc(alt).pattern);
            unloc(alt).pattern = unapply(unloc(alt).pattern);
            unloc(alt).rhs = rename_infix(m, unloc(alt).rhs);
        }
        return A;
    }
    else if (E.is_a<Haskell::Alt>())
    {
        std::abort();
    }
    else if (E.is_a<Haskell::SimpleRHS>())
    {
        auto R = E.as_<Haskell::SimpleRHS>();
        unloc(R.body) = rename_infix(m, unloc(R.body));
        if (R.decls)
            unloc(*R.decls) = rename_infix(m, unloc(*R.decls));
        return R;
    }
    else if (E.is_a<Haskell::MultiGuardedRHS>())
    {
        auto R = E.as_<Haskell::MultiGuardedRHS>();

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

        unloc(L.decls) = rename_infix(m, unloc(L.decls));
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
        C.alts = rename_infix(m, C.alts).as_<Haskell::Alts>();

        return C;
    }

    if (not E.is_expression()) return E;

    auto v = E.sub();

    for(auto& e: v)
	e = rename_infix(m, e);

    if (is_AST(E,"Decl"))
    {
	/* lhs */
	v[0] = unapply(v[0]);
	assert(v[0].head().is_a<Located<Hs::ID>>() or v[0].is_a<Haskell::List>() or v[0].is_a<Haskell::Tuple>());
    }
    else if (is_apply(E.head()))
    {
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
    else if (is_AST(E,"infixexp"))
	return desugar_infix(m, v);

    return expression_ref{E.head(),v};
}


expression_ref rename_infix_top(const Module& m, const expression_ref& decls)
{
    if (not decls.is_expression()) return decls;

    assert(is_AST(decls, "TopDecls"));

    auto v = decls.sub();

    for(auto& decl: decls.sub())
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
            auto [con, args] = Haskell::decompose_type_apps(constr);
            if (args.empty()) continue;

            auto ConName = con.as_<Haskell::TypeVar>().name;
            if (not args[0].is_a<Haskell::FieldDecls>()) continue;
            auto fields = args[0].as_<Haskell::FieldDecls>();

            int i = 0;
            for(auto& field_group: fields.field_decls)
            {
                for(auto& field_name: field_group.field_names)
                {
                    constructor_fields[field_name][ConName] = i;
                    i++;
                }
            }
            arity[ConName] = i;
        }

        if (not arity.empty())
        {
            for(auto& [field_name, constrs]: constructor_fields)
            {
                expression_ref name = Located<Hs::ID>({},field_name);
                vector<Located<Haskell::Alt>> alts;

                for(auto& [ConName,pos]: constrs)
                {
                    int a = arity[ConName];
                    vector<expression_ref> f;
                    for(int i=0;i<a;i++)
                        if (i == pos)
                            f.push_back(name);
                        else
                            f.push_back(Haskell::WildcardPattern());

                    expression_ref pattern = expression_ref{Located<Hs::ID>({},ConName),f};
                    expression_ref body = Haskell::SimpleRHS({noloc, name});
                    alts.push_back({noloc,{pattern,body}});
                }
                {
                    expression_ref pattern = Haskell::WildcardPattern();
                    expression_ref body = error(field_name+": pattern match failure");
                    body = Haskell::SimpleRHS({noloc,body});
                    alts.push_back({noloc,{pattern,body}});
                }

                Located<Hs::ID> x({},"#0");
                expression_ref body = Haskell::CaseExp(x,Haskell::Alts(alts));
                body = Haskell::LambdaExp({x},body);
                body = Haskell::SimpleRHS({noloc,body});

                v.push_back(AST_node("Decl") + name + body);
            }
        }
    }

    expression_ref E{AST_node("TopDecls"),v};

    return rename_infix(m, E);
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

struct renamer_state
{
    const Module& m;

//    int var_index=0;
//
//    var get_fresh_wildcard() { return var(-var_index++);}
//    var get_fresh_var() { return var(var_index++);}
//    var get_fresh_var(const string& name) {return var(name,var_index++);}


    bound_var_info find_vars_in_patterns(const vector<expression_ref>& pats, bool top = false);
    bound_var_info find_vars_in_pattern(const expression_ref& pat, bool top = false);
    bound_var_info find_bound_vars_in_stmt(const expression_ref& stmt);
    bound_var_info find_bound_vars_in_decls(const expression_ref& stmt);
    bound_var_info rename_patterns(vector<expression_ref>& pat, bool top = false);
    bound_var_info rename_pattern(expression_ref& pat, bool top = false);
    bound_var_info rename_decl_head(expression_ref& decl, bool is_top_level);
    expression_ref rename_decl(const expression_ref& decl, const bound_var_info& bound);
    bound_var_info rename_decls(expression_ref& decls, const bound_var_info& bound);
    bound_var_info rename_rec_stmt(expression_ref& stmt, const bound_var_info& bound);
    bound_var_info rename_stmt(expression_ref& stmt, const bound_var_info& bound);
    expression_ref rename(const expression_ref& E, const bound_var_info& bound);

    renamer_state(const Module& m_):m(m_) {}
};

expression_ref rename(const Module& m, const expression_ref& E)
{
    renamer_state Rn(m);
    return Rn.rename(E,set<string>());
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

	auto bound = find_vars_in_pattern(AP.var, false);
	bool overlap = not disjoint_add(bound, find_vars_in_pattern(AP.pattern, false));

	if (overlap)
	    throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";
	return bound;
    }

    if (pat.is_a<Haskell::List>())
    {
        auto& L = pat.as_<Haskell::List>();
        return find_vars_in_patterns(L.elements);
    }
    else if (pat.is_a<Haskell::Tuple>())
    {
        auto& T = pat.as_<Haskell::Tuple>();
        return find_vars_in_patterns(T.elements);
    }

    // 4. Handle literal values
    if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double()) return {};

    // 5. Get the identifier name for head
    expression_ref head = pat.head();
    if (not head.is_a<Located<Hs::ID>>())
	throw myexception()<<"Pattern '"<<pat<<"' doesn't start with an identifier!";
    auto id = unloc(head.as_<Located<Hs::ID>>()).name;

    // 6. Handle if identifier is a variable
    if (not is_haskell_con_name(id))
    {
	if (pat.size()) throw myexception()<<"Pattern "<<pat<<" has arguments, but doesn't start with a constructor!";
	if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!";
	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	return {id};
    }

    // 7. Resolve constructor name if identifier is a constructor
    if (not m.is_declared(id))
	throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

    const symbol_info& S = m.lookup_symbol(id);
    if (S.symbol_type != constructor_symbol)
	throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

    if (S.arity != pat.size())
	throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

    // 11. Return the variables bound
    return find_vars_in_patterns(pat.copy_sub());
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
    {
        // FIXME
	pat = var(-1);
	return {};
    }

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

    // 4. Handle literal values
    if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double()) return {};

    // 5. Get the identifier name for head
    expression_ref head = pat.head();
    if (not head.is_a<Located<Hs::ID>>())
	throw myexception()<<"Pattern '"<<pat<<"' doesn't start with an identifier!";
    auto id = unloc(head.as_<Located<Hs::ID>>()).name;

    // 6. Handle if identifier is a variable
    if (not is_haskell_con_name(id))
    {
	if (pat.size()) throw myexception()<<"Pattern "<<pat<<" has arguments, but doesn't start with a constructor!";
	if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!";
	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	// FIXME - since we are creating an ID here, we should give it a unique id!
	pat = var(id);
	return {id};
    }
    
    // 7. Resolve constructor name if identifier is a constructor
    if (not m.is_declared(id))
	throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

    const symbol_info& S = m.lookup_symbol(id);
    if (S.symbol_type != constructor_symbol)
	throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

    if (S.arity != pat.size())
	throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

    head = constructor(S.name, S.arity);

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
	pat = expression_ref{head,args};
    else
	pat = head;

    // 11. Return the variables bound
    return bound;
}

// FIXME make a RnM (or Renamer) object for renaming that can store the module, the set of bound vars, etc.
expression_ref renamer_state::rename_decl(const expression_ref& decl, const bound_var_info& bound)
{
    assert(is_AST(decl,"Decl"));
    assert(decl.is_expression());
    vector<expression_ref> v = decl.sub();

    auto& lhs = v[0];
    auto& rhs = v[1];
    assert(not is_apply(lhs.head()));

    auto f = lhs.head();

    // 1. We discover bound variables for the decls group in rename_decls( ), before we call this.
    auto bound2 = bound;

    // 2. If this is not a pattern binding, then rename the argument patterns x y z in `f x y z = ... `.
    //
    //    We deal with these here, since they are only in scope for this decl, whereas e.g. f is in scope
    //      for all decls in the decls group.
    bool pattern_bind = f.is_a<constructor>() or f.is_a<Haskell::List>() or f.is_a<Haskell::Tuple>();
    if (not pattern_bind)
    {
	assert(f.is_a<var>());
	assert(bound.count(f.as_<var>().name));

	if (lhs.size())
	{
	    auto args = lhs.sub();
	    assert(args.size());

            bool overlap = false;
	    bound_var_info arg_vars;
            for(auto& arg: args)
		overlap = overlap or not disjoint_add(arg_vars, rename_pattern(arg));
	    lhs = expression_ref{f, args};
	    if (overlap)
		throw myexception()<<"Function declaration '"<<lhs<<"' uses a variable twice!";

            // The args should be in scope when we process the rhs
            add(bound2,arg_vars);
	}
	else
	    lhs = f;
	assert(lhs.head().is_a<var>());
    }

    // 3. Rename the body given variables bound in the lhs
    rhs = rename(rhs, bound2);

    return expression_ref{decl.head(),v};
}

bound_var_info renamer_state::find_bound_vars_in_decls(const expression_ref& decls)
{
    assert(is_AST(decls,"Decls"));

    if (not decls.size()) return {};

    vector<expression_ref> v = decls.sub();

    // The idea is that we only add unqualified names here, and they shadow
    // qualified names.
    bound_var_info bound_names;
    for(auto& decl: v)
	if (is_AST(decl,"Decl"))
            add(bound_names, rename_decl_head(decl, false));
    return bound_names;
}

bound_var_info renamer_state::rename_decl_head(expression_ref& decl, bool is_top_level)
{
    bound_var_info bound_names;

    assert(is_AST(decl,"Decl"));

    auto w = decl.sub();
    auto& lhs = w[0];
    auto head = lhs.head();
    assert(head.is_a<Located<Hs::ID>>() or head.is_a<Haskell::List>() or head.is_a<Haskell::Tuple>());
    // For a constructor pattern, rename the whole lhs.
    if (head.is_a<Haskell::List>() or head.is_a<Haskell::Tuple>() or (head.is_a<Located<Hs::ID>>() and is_haskell_con_name(unloc(head.as_<Located<Hs::ID>>()).name)))
    {
        add(bound_names, rename_pattern(lhs, is_top_level));
    }
    // For a function pattern, just rename the variable being defined
    else if (lhs.size())
    {
        add(bound_names, rename_pattern(head, is_top_level));
        lhs = expression_ref{head,lhs.sub()};
    }
    // For a variable pattern, the variable being defined is the whole lhs
    else
    {
        add(bound_names,rename_pattern(lhs, is_top_level));
    }

    decl = expression_ref{decl.head(),w};

    return bound_names;
}

bound_var_info renamer_state::rename_decls(expression_ref& decls, const bound_var_info& bound)
{
    assert(is_AST(decls,"TopDecls") or is_AST(decls,"Decls"));

    if (not decls.size()) return {};

    vector<expression_ref> v = decls.sub();

    auto bound2 = bound;
    bool top = is_AST(decls,"TopDecls");

    // Find all the names bound HERE, versus in individual decls.

    // The idea is that we only add unqualified names here, and they shadow
    // qualified names.
    bound_var_info bound_names;
    for(auto& decl: v)
    {
	if (is_AST(decl,"Decl"))
            add(bound_names, rename_decl_head(decl, top));
        else if (is_AST(decl,"Decl:sigtype"))
        {
            auto id = decl.sub()[0];
            auto type = decl.sub()[1];
            assert(id.is_a<Located<Hs::ID>>());
            add(bound_names, rename_pattern(id, top));
            decl = expression_ref(AST_node("Decl:sigtype"),{id,type});
        }
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto C = decl.as_<Haskell::ClassDecl>();
            if (C.decls)
            {
                auto class_decls = unloc(*C.decls);
                assert(is_AST(class_decls, "Decls"));
                auto cdecls = class_decls.sub();
                for(auto& cdecl: cdecls)
                    if (is_AST(cdecl,"Decl"))
                        add(bound_names,rename_decl_head(cdecl, true));
                    else if (is_AST(cdecl,"Decl:sigtype"))
                    {
                        auto id = cdecl.sub()[0];
                        auto type = cdecl.sub()[1];
                        assert(id.is_a<Located<Hs::ID>>());
                        add(bound_names, rename_pattern(id, true));
                        cdecl = expression_ref(AST_node("Decl:sigtype"),{id,type});
                    }
                unloc(*C.decls) = expression_ref(AST_node("Decls"),cdecls);
            }
            decl = C;
        }
        if (decl.is_a<Haskell::InstanceDecl>())
        {
            auto I = decl.as_<Haskell::InstanceDecl>();
            if (I.decls)
            {
                auto idecls = unloc(*I.decls).sub();
                for(auto& idecl: idecls)
                    if (is_AST(idecl,"Decl"))
                        rename_decl_head(idecl, true);
                unloc(*I.decls) = expression_ref(AST_node("Decls"),idecls);
            }
            decl = I;
        }
    }

    // Replace ids with dummies
    add(bound2, bound_names);
    for(auto& decl: v)
    {
	if (is_AST(decl,"Decl"))
	    decl = rename_decl(decl, bound2);
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto C = decl.as_<Haskell::ClassDecl>();
            if (C.decls)
            {
                auto class_decls = unloc(*C.decls);
                auto cdecls = class_decls.sub();
                for(auto& cdecl: cdecls)
                    if (is_AST(cdecl,"Decl"))
                        cdecl = rename_decl(cdecl, bound2);
                unloc(*C.decls) = expression_ref(AST_node("Decls"),cdecls);
            }
            decl = C;
        }
        if (decl.is_a<Haskell::InstanceDecl>())
        {
            auto I = decl.as_<Haskell::InstanceDecl>();
            if (I.decls)
            {
                auto idecls = unloc(*I.decls).sub();
                for(auto& idecl: idecls)
                    if (is_AST(idecl,"Decl"))
                        idecl = rename_decl(idecl,bound2);
                unloc(*I.decls) = expression_ref(AST_node("Decls"),idecls);
            }
            decl = I;
        }
    }

    decls = expression_ref{decls.head(),v};
    return bound_names;
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
bound_var_info renamer_state::rename_rec_stmt(expression_ref& rec_stmt, const bound_var_info& bound)
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
        vars.push_back(Located<Hs::ID>({},var_name));
    expression_ref rec_tuple;
    if (vars.size() == 1)
        rec_tuple = vars[0];
    else
    {
        rec_tuple = Located<Hs::ID>({}, tuple_head(vars.size()).name());
        for(auto var: vars)
            rec_tuple = {rec_tuple, var};
    }

    // 3. Construct the do stmt
    auto stmts = rec_stmt.as_<Haskell::RecStmt>().stmts.stmts;
    expression_ref rec_return = Located<Hs::ID>({},"return");
    expression_ref rec_return_stmt = {rec_return, rec_tuple};
    stmts.push_back(Hs::SimpleQual(rec_return_stmt));
    auto rec_do = Haskell::Do(Haskell::Stmts(stmts));

    // 4. Construct the lambda function
    expression_ref rec_tuple_pattern = unapply(rec_tuple); // This makes the tuple expression into a pattern by translating `@ (@ ((,) x))` into `(,) x y`
    expression_ref rec_lambda = Haskell::LambdaExp({Haskell::LazyPattern(rec_tuple_pattern)}, rec_do);      // \ ~(b,c) -> do { ... }

    // 5. Construct rec_tuple_pattern <- mfix rec_lambda
    expression_ref mfix = Located<Hs::ID>({},"mfix");
    rec_stmt = Haskell::PatQual(rec_tuple_pattern, expression_ref{mfix, rec_lambda});

    // Combine the set of bound variables and rename our rewritten statement;
    return rename_stmt(rec_stmt, bound);
}

bound_var_info renamer_state::rename_stmt(expression_ref& stmt, const bound_var_info& bound)
{
    if (stmt.is_a<Hs::SimpleQual>())
    {
        auto SQ = stmt.as_<Hs::SimpleQual>();
	SQ.exp = rename(SQ.exp,bound);
        stmt = SQ;
	return {};
    }
    else if (stmt.is_a<Haskell::PatQual>())
    {
        auto PQ = stmt.as_<Haskell::PatQual>();
	PQ.exp = rename(PQ.exp, bound);
	auto bound_vars = rename_pattern(PQ.bindpat);
        stmt = PQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::LetQual>())
    {
        auto LQ = stmt.as_<Haskell::LetQual>();
	auto bound_vars = rename_decls(unloc(LQ.binds), bound);
	stmt = LQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::RecStmt>())
    {
        return rename_rec_stmt(stmt, bound);
    }
    else
	std::abort();
}

expression_ref renamer_state::rename(const expression_ref& E, const bound_var_info& bound)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = rename(element, bound);
        return L;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = rename(element, bound);
        return T;
    }
    else if (E.is_a<Located<Hs::ID>>())
    {
        auto& name = unloc(E.as_<Located<Hs::ID>>()).name;
        auto& loc = E.as_<Located<Hs::ID>>().loc;

        // Local vars bind id's tighter than global vars.
        if (includes(bound,name))
            return var(name);
        // If the variable is free, then try top-level names.
        else if (m.is_declared(name))
        {
            const symbol_info& S = m.lookup_symbol(name);
            string qualified_name = S.name;
            return var(qualified_name);
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
        auto bound2 = bound;
        auto R = E.as_<Haskell::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            add(bound2, rename_stmt(stmt, bound2));
        return R;
    }
    else if (E.is_a<Haskell::Do>())
    {
        auto bound2 = bound;
        auto D = E.as_<Haskell::Do>();
        for(auto& stmt: D.stmts.stmts)
            add(bound2, rename_stmt(stmt, bound2));
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

        auto bound2 = bound;
        auto MD = E.as_<Haskell::MDo>();
        for(auto& stmt: MD.stmts.stmts)
            add(bound2, rename_stmt(stmt, bound2));
        return MD;
    }
    else if (E.is_a<Haskell::CaseExp>())
    {
        auto& C = E.as_<Haskell::CaseExp>();
        auto object = rename(C.object, bound);
        auto alts = rename(C.alts, bound);
        return Haskell::CaseExp(object, alts.as_<Haskell::Alts>());
    }
    else if (E.is_a<Haskell::Alts>())
    {
        auto A = E.as_<Haskell::Alts>();
        for(auto& alt: A.alts)
        {
            auto bound2 = bound;

            add(bound2, rename_pattern(unloc(alt).pattern));
            unloc(alt).rhs = rename(unloc(alt).rhs, bound2);
        }
        return A;
    }
    else if (E.is_a<Haskell::Alt>())
    {
        std::abort();
    }
    else if (E.is_a<Haskell::WildcardPattern>())
        return var(-1);
    else if (E.is_a<Haskell::SimpleRHS>())
    {
        auto bound2 = bound;

        auto R = E.as_<Haskell::SimpleRHS>();
        if (R.decls)
            add(bound2, rename_decls(unloc(*R.decls), bound));
        unloc(R.body) = rename(unloc(R.body), bound2);

        return R;
    }
    else if (E.is_a<Haskell::MultiGuardedRHS>())
    {
        auto bound2 = bound;

        auto R = E.as_<Haskell::MultiGuardedRHS>();
        if (R.decls)
            add(bound2, rename_decls(unloc(*R.decls), bound));

        for(auto& guarded_rhs: R.guarded_rhss)
        {
            auto bound3 = bound2;
            for(auto& guard: guarded_rhs.guards)
                add(bound3, rename_stmt(guard, bound3));

            guarded_rhs.body = rename(guarded_rhs.body, bound3);
        }

        return R;
    }
    else if (E.is_a<Haskell::LambdaExp>())
    {
        auto L = E.as_<Haskell::LambdaExp>();

        // 1. Rename patterns for lambda arguments
        auto bound2 = bound;
        for(auto& arg: L.args)
            add(bound2, rename_pattern(arg));

        // 2. Rename the body
        L.body = rename(L. body, bound2);

        return L;
    }
    else if (E.is_a<Haskell::LetExp>())
    {
        auto L = E.as_<Haskell::LetExp>();

        auto bound2 = bound;
        add(bound2, rename_decls(unloc(L.decls), bound));
        unloc(L.body) = rename(unloc(L.body), bound2);

        return L;
    }
    else if (E.is_a<Haskell::IfExp>())
    {
        auto I = E.as_<Haskell::IfExp>();

        unloc(I.condition)    = rename(unloc(I.condition), bound);
        unloc(I.true_branch)  = rename(unloc(I.true_branch), bound);
        unloc(I.false_branch) = rename(unloc(I.false_branch), bound);

        return I;
    }

    vector<expression_ref> v = E.copy_sub();

    if (E.head().is_a<AST_node>())
    {
	auto& n = E.head().as_<AST_node>();
	if (n.type == "infixexp")
	    std::abort();
	else if (n.type == "Decls" or n.type == "TopDecls")
	{
	    expression_ref E2 = E;
	    rename_decls(E2, bound);
	    return E2;
	}
	else if (n.type == "Decl")
	    std::abort();
	else if (n.type == "rhs")
            std::abort();
	else if (n.type == "gdrhs")
            std::abort();
	else if (n.type == "ListComprehension")
	{
	    auto bound2 = bound;

	    for(int i=0;i<v.size()-1;i++)
		add(bound2, rename_stmt(v[i], bound2));
	    v.back() = rename(v.back(), bound2);

	    return expression_ref{E.head(),v};
	}
    }

    for(auto& e: v)
	e = rename(e, bound);

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
    if (is_apply(E.head()) and v[0].is_a<var>() and is_haskell_con_name(v[0].as_<var>().name))
    {
	auto& id = v[0].as_<var>().name;
	const symbol_info& S = m.lookup_resolved_symbol(id);
	assert(S.symbol_type == constructor_symbol);
	// If the constructor is fully applied, then do the apply now -- this might avoid some rounds of simplification?
	if (v.size() == 1 + S.arity)
	{
	    shift_list(v);
	    assert(v.size());
	    auto head  = constructor(S.name, S.arity);
	    return expression_ref{head,v};
	}
    }

    if (E.size())
	return expression_ref{E.head(),v};
    else
	return E;
}

