#include "pattern.H"

#include "util/string/join.H"
#include "expression/tuple.H" // for tuple_name
#include "util/set.H"   // for includes( , )

#include "haskell.H" // for Var, Tuple, List
#include "ids.H"     // for is_haskell_consym

using std::string;
using std::pair;
using std::vector;
using std::optional;

#include <range/v3/all.hpp>
namespace views = ranges::views;

namespace Haskell
{

string VarPattern::print() const
{
    return var.print();
}


string ConPattern::print() const
{
    string con = head.print();

    vector<string> ss = {con};
    for(auto& arg: args)
        ss.push_back(parenthesize_pattern(arg));

    if (is_haskell_sym(con))
    {
        assert(args.size() == 2);

        std::swap(ss[0], ss[1]);
    }

    return join(ss, " ");
}

string TypedPattern::print() const
{
    return pat.print() + " :: " + type.print();
}

string LiteralPattern::print() const
{
    return lit.print();
}

string ListPattern::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back( element.print() );
    return "[" + join(parts,",") +"]";
}

string TuplePattern::print() const
{
    vector<string> parts;
    for(auto& pat: elements)
        parts.push_back( pat.print() );
    return "(" + join(parts,",") +")";
}

Pattern tuple_pattern(const LPats& es)
{
    if (es.size() == 1)
        return unloc(es[0]);
    else
        return TuplePattern(es);
}

string WildcardPattern::print() const
{
    return "_";
}

string parenthesize_pattern(const LPat& p)
{
    return parenthesize_pattern(unloc(p));
}

string parenthesize_pattern(const Pattern& p)
{
    string result = p.print();

    bool add_parens = false;
    if (auto c = p.to<ConPattern>(); c and not c->args.empty())
        add_parens = true;
    else if (p.is_a<TypedPattern>())
        add_parens = true;

    if (add_parens)
        result = "(" + result + ")";

    return result;
}

string LazyPattern::print() const
{
    return "~"+parenthesize_pattern(pattern);
}

string StrictPattern::print() const
{
    return "!"+parenthesize_pattern(pattern);
}

string AsPattern::print() const
{
    return var.print()+"@"+parenthesize_pattern(pattern);
}


std::set<Var> vars_in_patterns(const LPats& pats)
{
    std::set<Var> vars;

    for(auto& pat: pats)
        add(vars, vars_in_pattern(pat));

    return vars;
}

std::set<Var> vars_in_pattern(const LPat& lpat)
{
    auto& pat = unloc(lpat);

    if (pat.is_a<Haskell::WildcardPattern>())
	return {};
    else if (auto lp = pat.to<Haskell::LazyPattern>())
        return vars_in_pattern(lp->pattern);
    else if (auto sp = pat.to<Haskell::StrictPattern>())
        return vars_in_pattern(sp->pattern);
    else if (auto ap = pat.to<Haskell::AsPattern>())
	return plus( vars_in_pattern({noloc,VarPattern(ap->var)}),
                     vars_in_pattern(ap->pattern) );
    else if (auto l = pat.to<Haskell::ListPattern>())
        return vars_in_patterns(l->elements);
    else if (auto t = pat.to<Haskell::TuplePattern>())
        return vars_in_patterns(t->elements);
    else if (auto v = pat.to<Haskell::VarPattern>())
	return { v->var };
    else if (auto c = pat.to<ConPattern>())
        return vars_in_patterns(c->args);
    else if (auto tp = pat.to<TypedPattern>())
        return vars_in_pattern(tp->pat);
    else if (pat.is_a<LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

ConPattern TruePat()
{
    return {Hs::True(), {}};
}

ConPattern FalsePat()
{
    return {Hs::False(), {}};
}

ConPattern ConsPat(const LPat& p, const LPat& ps)
{
    return {Hs::ConsCon(),{p, ps}};
}

ConPattern NilPat()
{
    return {Hs::Nil(),{}};
}

ConPattern to_con_pat(const ListPattern& L)
{
    ConPattern list_pat = NilPat();

    for(auto& e: L.elements | views::reverse)
        list_pat = ConsPat(e, {noloc,list_pat});

    return list_pat;
}

ConPattern to_con_pat(const std::string& s)
{
    ListPattern L;
    for(char c: s)
	L.elements.push_back({noloc,LiteralPattern(Literal(Char(c)))});
    return to_con_pat(L);
}

ConPattern to_con_pat(const TuplePattern& T)
{
    int n = T.elements.size();
    return {TupleCon(n), T.elements};
}

}
