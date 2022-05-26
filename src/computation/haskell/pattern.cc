#include "haskell.H"
#include "util/string/join.H"
#include "expression/tuple.H" // for tuple_name
#include "util/set.H"   // for includes( , )

using std::string;
using std::pair;
using std::vector;
using std::optional;

namespace Haskell
{

string WildcardPattern::print() const
{
    return "_";
}

string parenthesize_pattern(const Pattern& p)
{
    string result = p.print();
    if (p.is_a<Located<Var>>() or p.is_a<Tuple>() or p.is_a<WildcardPattern>() or p.is_a<LazyPattern>() or p.is_a<AsPattern>() or p.is_a<List>())
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
