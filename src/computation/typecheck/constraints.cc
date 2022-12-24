#include "constraints.H"
#include "util/string/join.H"

using std::vector;
using std::string;

string Constraint::print() const
{
    string s = (flavor==Given)?"[G] ":"[W] ";
    s += ev_var.print() + " :: " + pred.print();
    return s;
}

string print(const LIE& lie)
{
    vector<string> ss;
    for(auto& constraint: lie)
    {
        ss.push_back(constraint.print());
    }
    return "{ " + join(ss, "; ") + " }";
}

vector<Type> preds_from_lie(const LIE& constraints)
{
    vector<Type> preds;
    for(auto& constraint: constraints)
        preds.push_back(constraint.pred);
    return preds;
}

vector<Core::Var> vars_from_lie(const LIE& constraints)
{
    vector<Core::Var> vars;
    for(auto& constraint: constraints)
        vars.push_back( constraint.ev_var );
    return vars;
}

LIE dictionary_constraints(const LIE& lie1)
{
    LIE lie2;

    for(auto& constraint: lie1)
    {
        if (not is_equality_constraint(constraint.pred))
            lie2.push_back(constraint);
    }

    return lie2;
}

LIE equality_constraints(const LIE& lie1)
{
    LIE lie2;

    for(auto& constraint: lie1)
    {
        if (is_equality_constraint(constraint.pred))
            lie2.push_back(constraint);
    }

    return lie2;
}

string WantedConstraints::print() const
{
    vector<string> cs;
    for(auto& constraint: simple)
        cs.push_back(constraint.print());
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

    for(auto& given: givens)
    {
        assert(max_meta_level(given.pred) < level);
        assert(max_level(given.pred) <= level);
    }

    for(auto& wanted: wanteds.simple)
    {
        assert(max_level(wanted.pred) <= level);
        assert(max_meta_level(wanted.pred) <= level);
    }

    for(auto& implication: wanteds.implications)
        assert(implication->level > level);
}

