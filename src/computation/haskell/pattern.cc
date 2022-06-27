#include "pattern.H"
#include "util/string/join.H"
#include "expression/tuple.H" // for tuple_name
#include "util/set.H"   // for includes( , )

#include "haskell.H" // for Var, Tuple, List

using std::string;
using std::pair;
using std::vector;
using std::optional;

namespace Haskell
{

string VarPattern::print() const
{
    return v.print();
}

string ConPattern::print() const
{
    vector<string> ss;
    ss.push_back(head.print());
    for(auto& arg: args)
        ss.push_back(parenthesize_pattern(arg));
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
    for(auto& pat: pats)
        parts.push_back( pat.print() );
    return "(" + join(parts,",") +")";
}

string WildcardPattern::print() const
{
    return "_";
}

string parenthesize_pattern(const Pattern& p)
{
    string result = p.print();
    if (p.is_a<Var>() or p.is_a<Tuple>() or p.is_a<WildcardPattern>() or p.is_a<LazyPattern>() or p.is_a<AsPattern>() or p.is_a<List>())
        ;
    else
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

}
