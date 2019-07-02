#include <string>
#include <vector>
#include <set>
#include <map>
#include <deque>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "computation/expression/constructor.H"
#include "computation/expression/AST_node.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::set;
using std::map;
using std::deque;

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

	return infix_parse(m, op1, {AST_node("id","negate"),E1}, T);
    }
    // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
    else
	return infix_parse(m, op1, E1, T);
}

symbol_info get_op_sym(const Module& m, const expression_ref& O)
{
    if (not is_AST(O, "id"))
	throw myexception()<<"Can't use expression '"<<O.print()<<"' as infix operator.";

    symbol_info op_sym;
    auto name = O.as_<AST_node>().value;

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
    else if (is_AST(E,"id"))
    {
	auto& value = E.as_<AST_node>().value;
	if (not is_haskell_con_name(value))
	    return {value};
    }
    return {};
}

string get_func_name(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));
    auto& lhs = decl.sub()[0];
    assert(is_AST(lhs,"id"));
    return lhs.head().as_<AST_node>().value;
}

string desugar_get_func_name(const expression_ref& decl)
{
    auto& lhs = decl.sub()[0];
    return lhs.head().as_<var>().name;
}

bool is_pattern_binding(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));
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
expression_ref unapply(const expression_ref& E)
{
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
    if (not E.is_expression()) return E;

    auto v = E.sub();

    for(auto& e: v)
	e = rename_infix(m, e);

    if (is_AST(E,"Decl"))
    {
	/* lhs */
	v[0] = unapply(v[0]);
	assert(is_AST(v[0],"id"));
    }
    else if (is_AST(E,"alt"))
    {
	/* pat */
	v[0] = unapply(v[0]);
    }
    else if (is_AST(E,"Lambda"))
    {
	for(int i=0;i<v.size()-1;i++)
	    v[i] = unapply(v[i]);
    }
    else if (is_AST(E,"PatQual"))
    {
	/* pat */
	v[0] = unapply(v[0]);
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

// 1. The primary purpose of the rename pass is to convert identifiers to (possibly qualified) vars.

// 2. Additionally, we also try and translate rec expressions to mfix expressions here.

// We keep track of locally bound variables only so that we know when to avoid looking for a qualified symbol.
typedef set<string> bound_var_info;

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

    bound_var_info rename_pattern(expression_ref& pat, bool top = false);
    expression_ref rename_decl(const expression_ref& decl, const bound_var_info& bound);
    bound_var_info rename_decls(expression_ref& decls, const bound_var_info& bound);
    bound_var_info rename_stmt(expression_ref& stmt, const bound_var_info& bound);
    expression_ref rename(const expression_ref& E, const bound_var_info& bound);

    renamer_state(const Module& m_):m(m_) {}
};

expression_ref rename(const Module& m, const expression_ref& E)
{
    renamer_state Rn(m);
    return Rn.rename(E,set<string>());
}

// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
bound_var_info renamer_state::rename_pattern(expression_ref& pat, bool top)
{
    assert(not is_apply_exp(pat));

    // 1. Handle _
    if (is_AST(pat,"WildcardPattern"))
    {
	pat = var(-1);
	return {};
    }

    // 2. Handle ~pat
    if (is_AST(pat,"LazyPattern"))
    {
	auto sub_pat = pat.sub()[0];
	auto bound = rename_pattern(sub_pat, top);

	pat = AST_node("LazyPattern")+sub_pat;
	return bound;
    }

    // 3. Handle x@pat
    if (is_AST(pat,"AsPattern"))
    {
	assert(not top);

	auto x = pat.sub()[0];
	auto sub_pat = pat.sub()[1];
	auto bound = rename_pattern(x, false);
	bool overlap = not disjoint_add(bound, rename_pattern(sub_pat, false));

	pat = AST_node("AsPattern")+x+sub_pat;
	if (overlap)
	    throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";
	return bound;
    }
    
    // 4. Handle literal values
    if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double()) return {};

    // 5. Get the identifier name for head
    expression_ref head = pat.head();
    if (not is_AST(head,"id"))
	throw myexception()<<"Pattern '"<<pat<<"' doesn't start with an identifier!";
    auto id = head.as_<AST_node>().value;

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
    vector<expression_ref> args = pat.copy_sub();;

    bound_var_info bound;
    // Rename the arguments
    bool overlap = false;
    for(auto& e: args)
    {
	auto bound_here =  rename_pattern(e, top);
	overlap = overlap or not disjoint_add(bound, bound_here);
    }

    // 10. Construct the renamed pattern
    if (args.size())
	pat = expression_ref{head,args};
    else
	pat = head;

    if (overlap)
	throw myexception()<<"Pattern '"<<pat<<"' uses a variable twice!";

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
    bool pattern_bind = f.is_a<constructor>();
    if (not pattern_bind)
    {
	assert(f.is_a<var>());
	assert(bound.count(f.as_<var>().name));

	if (lhs.size())
	{
	    bound_var_info arg_vars;
	    auto args = lhs.sub();
	    bool overlap = false;
	    for(auto& arg: args)
		overlap = overlap or not disjoint_add(arg_vars, rename_pattern(arg));
	    assert(args.size());
	    lhs = expression_ref{f, args};
	    if (overlap)
		throw myexception()<<"Function declaration '"<<lhs<<"' uses a variable twice!";
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
	if (not is_AST(decl,"Decl")) continue;

	auto w = decl.sub();
	auto& lhs = w[0];
	auto head = lhs.head();
	assert(is_AST(head,"id"));
	// For a constructor pattern, rename the whole lhs.
	if (is_haskell_con_name(head.as_<AST_node>().value))
	{
	    add(bound_names,rename_pattern(lhs, top));
	}
	// For a function pattern, just rename the variable being defined
	else if (lhs.size())
	{
	    add(bound_names,rename_pattern(head, top));
	    lhs = expression_ref{head,lhs.sub()};
	}
	// For a variable pattern, the variable being defined is the whole lhs
	else
	{
	    add(bound_names,rename_pattern(lhs, top));
	}
	decl = expression_ref{decl.head(),w};
    }

    // Replace ids with dummies
    add(bound2, bound_names);
    for(auto& e: v)
    {
	if (is_AST(e,"Decl"))
	    e = rename_decl(e, bound2);
    }

    decls = expression_ref{decls.head(),v};
    return bound_names;
}

bound_var_info renamer_state::rename_stmt(expression_ref& stmt, const bound_var_info& bound)
{
    if (is_AST(stmt, "SimpleQual"))
    {
	stmt = rename(stmt,bound);
	return {};
    }
    else if (is_AST(stmt, "PatQual"))
    {
	auto v = stmt.sub();
	auto bound_vars = rename_pattern(v[0]);
	v[1] = rename(v[1], bound);
	stmt = expression_ref{stmt.head(),v};
	return bound_vars;
    }
    else if (is_AST(stmt, "LetQual"))
    {
	auto v = stmt.sub();
	auto bound_vars = rename_decls(v[0], bound);
	stmt = expression_ref{stmt.head(),v};
	return bound_vars;
    }
    else if (is_AST(stmt, "Rec"))
    {
    }
    else
	std::abort();
}

expression_ref renamer_state::rename(const expression_ref& E, const bound_var_info& bound)
{
    vector<expression_ref> v = E.copy_sub();;
      
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
	else if (n.type == "WildcardPattern")
	{
	    return var(-1);
	}
	else if (n.type == "rhs")
	{
	    if (v.size() == 2)
	    {
		auto bound2 = bound;
		add(bound2, rename_decls(v[1],bound));
		v[0] = rename(v[0], bound2);
	    }
	    else
		v[0] = rename(v[0], bound);
	    return expression_ref{E.head(),v};
	}
	else if (n.type == "gdrhs")
	{
	    if (v.size() == 2)
	    {
		auto bound2 = bound;
		add(bound2, rename_decls(v[1],bound));
		v[0] = rename(v[0], bound2);
	    }
	    else
		v[0] = rename(v[0], bound);
	    return expression_ref{E.head(),v};
	}
	else if (n.type == "gdrh")
	{
	    auto& guards = v[0];
	    vector<expression_ref> w = guards.sub();
	    auto bound2 = bound;
	    for(int i=0;i<w.size();i++)
		add(bound2, rename_stmt(w[i], bound2));
	    guards = expression_ref{guards.head(),std::move(w)};

	    auto& rh = v[1];
	    rh = rename(rh, bound2);

	    return expression_ref{E.head(),std::move(v)};
	}
	else if (n.type == "id")
	{
	    assert(v.empty());
	    // Local vars bind id's tighter than global vars.
	    if (includes(bound,n.value))
		return var(n.value);
	    // If the variable is free, then try top-level names.
	    else if (m.is_declared(n.value))
	    {
		const symbol_info& S = m.lookup_symbol(n.value);
		string qualified_name = S.name;
		return var(qualified_name);
	    }
	    else
		throw myexception()<<"Can't find id '"<<n.value<<"'";
	}
	else if (n.type == "ListComprehension")
	{
	    auto bound2 = bound;

	    for(int i=0;i<v.size()-1;i++)
		add(bound2, rename_stmt(v[i], bound2));
	    v.back() = rename(v.back(), bound2);

	    return expression_ref{E.head(),v};
	}
	else if (n.type == "Lambda")
	{
	    // 1. Rename patterns for lambda arguments
	    auto bound2 = bound;
	    for(int i=0; i<v.size()-1; i++)
		add(bound2, rename_pattern(v[i]));

	    // 2. Rename the body
	    auto& body = v.back();
	    body = rename(body, bound2);

	    return expression_ref{E.head(),v};
	}
	else if (n.type == "Do")
	{
	    auto bound2 = bound;
	    for(auto& stmt: v)
		add(bound2, rename_stmt(stmt, bound2));
	    return expression_ref{E.head(),v};
	}
	else if (n.type == "MDo")
        {
        }
	else if (n.type == "Let")
	{
	    auto& decls = v[0];
	    auto& body = v[1];

	    auto bound2 = bound;
	    add(bound2, rename_decls(decls, bound));
	    body = rename(body, bound2);

	    return expression_ref{E.head(),v};
	}
	else if (n.type == "alt")
	{
	    auto& pat = v[0];
	    auto& body = v[1];
	    auto bound2 = bound;
	    add(bound2, rename_pattern(pat));
	    body = rename(body, bound2);

	    return expression_ref{E.head(),v};
	}
    }

    for(auto& e: v)
	e = rename(e, bound);

    // Apply fully-applied multi-argument constructors during rename.
    // We don't do this for single-argument constructors. By leaving them as vars, we avoid
    //   getting many let-allocated copies of (), [], True, False, Nothing, etc.
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

