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

Pattern tuple_pattern(const std::vector<Pattern>& es)
{
    if (es.size() == 1)
        return es[0];
    else
        return TuplePattern(es);
}

string WildcardPattern::print() const
{
    return "_";
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

}
