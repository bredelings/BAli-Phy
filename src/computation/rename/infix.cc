#include <string>
#include <vector>
#include <set>
#include <deque>
#include <functional>

#include "rename.H"
#include "computation/module.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::deque;
using std::function;

using InfixBuilder = function<Hs::LExp(const Hs::LExp&, const Hs::LExp&, const Hs::LExp&)>;
using NegBuilder = function<Hs::LExp(const Hs::LExp&, const Hs::LExp&)>;
using OpLookup = function<Located<OpInfo>(const Hs::LExp&)>;

Located<expression_ref> infix_parse(const OpLookup& get_op, const Located<OpInfo>& op1, const Located<expression_ref>& E1, deque<Located<expression_ref>>& T, const InfixBuilder& build_infix, const NegBuilder& build_neg);

namespace
{
    // Detect the leading prefix-negation marker in an unresolved infix spine.
    bool is_prefix_neg(const vector<Hs::LExp>& terms)
    {
        return not terms.empty() and unloc(terms[0]).is_a<Hs::Neg>();
    }
}

// Return whether a spine term occupies an operator slot, accounting for prefix negation.
bool is_infix_operator_term(const vector<Hs::LExp>& terms, int index)
{
    if (is_prefix_neg(terms))
        return index != 0 and index % 2 == 0;
    else
        return index % 2 == 1;
}

// Return whether a spine term occupies an operand slot that should be pattern-classified.
bool is_infix_operand_term(const vector<Hs::LExp>& terms, int index)
{
    return not unloc(terms[index]).is_a<Hs::Neg>() and not is_infix_operator_term(terms, index);
}

// Rebuild an unresolved infix expression from a slice of spine terms.
Hs::LExp make_infix_exp(const vector<Hs::LExp>& terms)
{
    assert(not terms.empty());
    if (terms.size() == 1)
        return terms[0];

    auto loc = terms.front().loc;
    for(const auto& term: terms)
        loc = loc * term.loc;

    return {loc, Hs::InfixExp(terms)};
}

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
Located<expression_ref> infix_parse_neg(const OpLookup& get_op, const Located<OpInfo>& op1, deque<Located<expression_ref>>& T, const InfixBuilder& build_infix, const NegBuilder& build_neg)
{
    assert(not T.empty());

    Located<expression_ref> E1 = T.front();
    T.pop_front();

    // We are starting with a Neg
    if (unloc(E1).is_a<Hs::Neg>())
    {
	if (unloc(op1).fixity.precedence >= 6) throw myexception()<<"Cannot parse '"<<unloc(op1).name<<"' -";

        auto neg_loc = E1.loc;
        auto neg_sym = OpInfo{"-",{left_fix,6}};
        auto neg = Located<OpInfo>{neg_loc, neg_sym};

	E1 = infix_parse_neg(get_op, neg, T, build_infix, build_neg);

	return infix_parse(get_op, op1, build_neg({neg_loc,Hs::Var("negate")}, E1), T, build_infix, build_neg);
    }
    // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
    else
	return infix_parse(get_op, op1, E1, T, build_infix, build_neg);
}

// Look up an operator through the current scoped fixity environment.
OpInfo renamer_state::get_operator(const string& name) const
{
    auto fixity = fixity_env.find(name);
    if (fixity == fixity_env.end())
        fixity = fixity_env.find(get_unqualified_name(name));

    if (fixity != fixity_env.end())
        return {name, fixity->second};
    else if (m.is_declared(name))
        return m.get_operator(name);
    else
        return {name, {left_fix, 9}};
}

Located<OpInfo> get_op_sym(const renamer_state& Rn, const Located<expression_ref>& O)
{
    string name;
    if (auto v = unloc(O).to<Haskell::Var>())
        name = v->name;
    else if (auto c = unloc(O).to<Haskell::Con>())
        name = c->name;
    else
        std::abort();

    return {O.loc, Rn.get_operator(name)};
}

/// Expression is of the form ... op1 E1 [op2 ...]. Get right operand of op1.
Located<expression_ref> infix_parse(const OpLookup& get_op, const Located<OpInfo>& loc_op1, const Located<expression_ref>& E1, deque<Located<expression_ref>>& T, const InfixBuilder& build_infix, const NegBuilder& build_neg)
{
    if (T.empty())
	return E1;

    auto& op1 = unloc(loc_op1);

    auto  op2_E = T.front();
    auto  loc_op2   = get_op(op2_E);
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
	auto E3 = infix_parse_neg(get_op, loc_op2, T, build_infix, build_neg);

	auto E1_op2_E3 = build_infix(op2_E, E1, E3);

	return infix_parse(get_op, loc_op1, E1_op2_E3, T, build_infix, build_neg);
    }
}

// Resolve an unresolved infix spine using the supplied output builders.
Located<expression_ref> resolve_infix(const vector<Located<expression_ref>>& T, const OpLookup& get_op, const InfixBuilder& build_infix, const NegBuilder& build_neg)
{
    deque<Located<expression_ref>> T2;
    T2.insert(T2.begin(), T.begin(), T.end());

    auto no_sym = OpInfo{"",{non_fix,-1}};
    return infix_parse_neg(get_op, {noloc, no_sym}, T2, build_infix, build_neg);
}

Located<expression_ref> desugar_infix(const renamer_state& Rn, const vector<Located<expression_ref>>& T)
{
    auto get_op = [&](const Hs::LExp& op)
    {
        return get_op_sym(Rn, op);
    };

    auto build_infix = [](const Hs::LExp& op, const Hs::LExp& lhs, const Hs::LExp& rhs)
    {
        return Hs::apply(op, {lhs, rhs});
    };

    auto build_neg = [](const Hs::LExp& neg, const Hs::LExp& arg)
    {
        return Hs::apply(neg, {arg});
    };

    return resolve_infix(T, get_op, build_infix, build_neg);
}

Hs::LPat desugar_pattern_infix(const renamer_state& Rn, const vector<Located<expression_ref>>& T)
{
    auto get_op = [&](const Hs::LExp& op)
    {
        return get_op_sym(Rn, op);
    };

    auto build_infix = [](const Hs::LExp& op, const Hs::LExp& lhs, const Hs::LExp& rhs)
    {
        if (auto con = unloc(op).to<Hs::Con>())
            return Hs::LExp{op.loc, Hs::ConPattern({op.loc, *con}, {lhs, rhs})};
        else
            return Hs::LExp{op.loc, Hs::WildcardPattern()};
    };

    auto build_neg = [](const Hs::LExp& neg, const Hs::LExp& arg)
    {
        return Hs::apply(neg, {arg});
    };

    return resolve_infix(T, get_op, build_infix, build_neg);
}
