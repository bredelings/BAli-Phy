#include "constraints.H"
#include "util/string/join.H"

using std::vector;
using std::string;

string print(const LIE& lie)
{
    vector<string> ss;
    for(auto& [value,type]: lie)
    {
        ss.push_back(value.print() + " :: " + type.print());
    }
    return "{ " + join(ss, "; ") + " }";
}

vector<Type> constraints_from_lie(const LIE& lie)
{
    vector<Type> constraints;
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

LIE dictionary_constraints(const LIE& lie1)
{
    LIE lie2;

    for(auto& pred: lie1)
    {
        auto& [_, constraint] = pred;
        if (not is_equality_constraint(constraint))
            lie2.push_back(pred);
    }

    return lie2;
}

LIE equality_constraints(const LIE& lie1)
{
    LIE lie2;

    for(auto& pred: lie1)
    {
        auto& [_, constraint] = pred;
        if (is_equality_constraint(constraint))
            lie2.push_back(pred);
    }

    return lie2;
}

string WantedConstraints::print() const
{
    vector<string> cs;
    for(auto& [value, type]: simple)
        cs.push_back(value.print() + " :: " + type.print());
    for(auto& implication: implications)
        cs.push_back(implication->print());
    return "{ " + join(cs,"; ") + " }";
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

string Implication::print() const
{
    std::ostringstream oss;
    oss<<"forall["<<level<<"]";
    for(auto& tv: tvs)
        oss<<" "<<tv.print();
    oss<<".(";
    if (not givens.empty())
        oss<< ::print(givens) <<" => ";
    oss<<wanteds.print();
    if (not evidence_binds->empty())
        oss<<" =& "<<print_cdecls(*evidence_binds);
    oss<<")";
    return oss.str();
}

Implication::Implication(int l, const vector<TypeVar>& v, const LIE& g, const WantedConstraints& w, const std::shared_ptr<Core::Decls>& eb)
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

