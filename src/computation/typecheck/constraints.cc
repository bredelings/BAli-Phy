#include "constraints.H"
#include "util/string/join.H"

using std::vector;
using std::string;

string print(const LIE& lie)
{
    std::ostringstream oss;
    vector<string> ss;
    for(auto& [value,type]: lie)
    {
        ss.push_back(value.print() + " :: " + type.print());
    }
    return "{ " + join(ss, "; ") + " }";
}

vector<Hs::Type> constraints_from_lie(const LIE& lie)
{
    vector<Hs::Type> constraints;
    for(auto& [_, constraint]: lie)
        constraints.push_back(constraint);
    return constraints;
}

vector<Core::Var> vars_from_lie(const LIE& lie)
{
    vector<Core::Var> vars;
    for(auto& [var, constraint]: lie)
        vars.push_back( var );
    return vars;
}

WantedConstraints::WantedConstraints(const LIE& l)
    : simple(l)
{
}


WantedConstraints& WantedConstraints::operator+=(const WantedConstraints& wc2)
{
    simple += wc2.simple;
    implications += wc2.implications;
    return *this;
}

WantedConstraints WantedConstraints::operator+(const WantedConstraints& wc2) const
{
    auto tmp = *this;
    tmp += wc2;
    return tmp;
}

bool WantedConstraints::empty() const
{
    return simple.empty() and implications.empty();
}

LIE WantedConstraints::all_simple() const
{
    LIE w = simple;

    for(auto& implication: implications)
        w += implication->wanteds.all_simple();

    return w;
}

Implication::Implication(int l, const vector<Hs::TypeVar>& v, const LIE& g, const WantedConstraints& w, const std::shared_ptr<Core::Decls>& eb)
    :level(l), evidence_binds(eb), tvs(v), givens(g), wanteds(w)
{
    for(auto& tv: tvs)
        assert(tv.level() == l);

    for(auto& [_,constraint]: givens)
    {
        assert(max_meta_level(constraint) < level);
        assert(max_level(constraint) <= level);
    }

    for(auto& [_,constraint]: wanteds.simple)
    {
        assert(max_level(constraint) <= level);
        assert(max_meta_level(constraint) <= level);
    }

    for(auto& implication: wanteds.implications)
        assert(implication->level > level);
}
