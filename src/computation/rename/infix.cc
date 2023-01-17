#include <string>
#include <vector>
#include <set>
#include <deque>

#include "rename.H"
#include "computation/module.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::deque;

Located<expression_ref> infix_parse(const Module& m, const Located<symbol_info>& op1, const Located<expression_ref>& E1, deque<Located<expression_ref>>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
Located<expression_ref> infix_parse_neg(const Module& m, const Located<symbol_info>& op1, deque<Located<expression_ref>>& T)
{
    assert(not T.empty());

    Located<expression_ref> E1 = T.front();
    T.pop_front();

    // We are starting with a Neg
    if (unloc(E1).is_a<Hs::Neg>())
    {
	if (unloc(op1).fixity.precedence >= 6) throw myexception()<<"Cannot parse '"<<unloc(op1).name<<"' -";

        auto neg_loc = E1.loc;
        auto neg_sym = symbol_info("-",variable_symbol, {}, 2,{left_fix,6});
        auto neg = Located<symbol_info>{neg_loc, neg_sym};

	E1 = infix_parse_neg(m, neg, T);
        auto neg_E1_loc = *neg_loc + *E1.loc;

	return infix_parse(m, op1, {neg_E1_loc,Hs::ApplyExp({neg_loc,Hs::Var({noloc,"negate"})},{E1})}, T);
    }
    // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
    else
	return infix_parse(m, op1, E1, T);
}

// FIXME: just return the fixity
Located<symbol_info> get_op_sym(const Module& m, const Located<expression_ref>& O)
{
    symbol_info op_sym;
    string name;
    if (auto v = unloc(O).to<Haskell::Var>())
        name = unloc(v->name);
    else if (auto c = unloc(O).to<Haskell::Con>())
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

    return {O.loc, op_sym};
}

// FIXME: "h:t!!0 = h" gives an error that says that the arity of ":" is wrong.
// We get ":" "h" "!!" "t" 0 as a pattern...
//   This seems to be a result of the hack in unapply

/// Expression is of the form ... op1 E1 [op2 ...]. Get right operand of op1.
Located<expression_ref> infix_parse(const Module& m, const Located<symbol_info>& loc_op1, const Located<expression_ref>& E1, deque<Located<expression_ref>>& T)
{
    if (T.empty())
	return E1;

    auto& op1 = unloc(loc_op1);

    auto  op2_E = T.front();
    auto  loc_op2   = get_op_sym(m, op2_E);
    auto& op2       = unloc(loc_op2);

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
	auto E3 = infix_parse_neg(m, loc_op2, T);

	auto E1_op2_E3 = Hs::ApplyExp(op2_E, {E1, E3});

        auto E123_loc = E1.loc.value() + E3.loc.value();
        
	return infix_parse(m, loc_op1, {E123_loc, E1_op2_E3}, T);
    }
}

Located<expression_ref> desugar_infix(const Module& m, const vector<Located<expression_ref>>& T)
{
    deque<Located<expression_ref>> T2;
    T2.insert(T2.begin(), T.begin(), T.end());

    auto no_sym = symbol_info{"",variable_symbol,{}, 2,{non_fix,-1}};
    return infix_parse_neg(m, {noloc, no_sym}, T2);
}

