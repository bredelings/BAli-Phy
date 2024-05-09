#include "constraints.H"
#include "util/string/join.H"
#include "types.H"
#include "context.H"
#include "util/assert.hh"

using std::vector;
using std::set;
using std::string;

OccurrenceOrigin::OccurrenceOrigin(const std::string& s)
    :name(s)
{}

int Constraint::level() const
{
    return tc_state->level;
}

string Constraint::print() const
{
    string s = (flavor==Given)?"[G] ":"[W] ";
    s += ev_var.print() + " :: " + pred.print();
    return s;
}

Constraint::Constraint(const ConstraintOrigin& o, ConstraintFlavor f, Core::Var v, Type p,
                       const cow_ptr<TypeCheckerContext>& s)
    :origin(o),
     tc_state(s),

     flavor(f),
     ev_var(v),
     pred(p)
{
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

vector<Core::Var> dict_vars_from_lie(const LIE& constraints)
{
    vector<Core::Var> vars;
    for(auto& constraint: constraints)
        if (is_dictionary_pred(constraint.pred))
            vars.push_back( constraint.ev_var );
    return vars;
}

LIE dictionary_constraints(const LIE& lie1)
{
    LIE lie2;

    for(auto& constraint: lie1)
    {
        if (is_dictionary_pred(constraint.pred))
            lie2.push_back(constraint);
    }

    return lie2;
}

LIE equality_constraints(const LIE& lie1)
{
    LIE lie2;

    for(auto& constraint: lie1)
    {
        if (is_equality_pred(constraint.pred))
            lie2.push_back(constraint);
    }

    return lie2;
}

bool contains_equality_constraints(const LIE& constraints)
{
    for(auto& constraint: constraints)
        if (is_equality_pred(constraint.pred))
            return true;
    return false;
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

Implication::Implication(int l, const vector<TypeVar>& v, const LIE& g, const WantedConstraints& w, const std::shared_ptr<Core::Decls>& eb, const cow_ptr<TypeCheckerContext>& c)
    :level(l), evidence_binds(eb), tvs(v), givens(g), wanteds(w), tc_state(c)
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

set<TypeVar> free_type_variables(const LIE& env)
{
    set<TypeVar> free;
    for(auto& constraint: env)
        add(free, free_type_variables(constraint.pred));
    return free;
}

set<MetaTypeVar> free_meta_type_variables(const LIE& env)
{
    set<MetaTypeVar> free;
    for(auto& constraint: env)
        add(free, free_meta_type_variables(constraint.pred));
    return free;
}

set<TypeVar> free_type_variables(const WantedConstraints& wanteds)
{
    set<TypeVar> free = free_type_variables(wanteds.simple);
    for(auto& implic: wanteds.implications)
    {
        add(free, free_type_variables(implic->givens));
        add(free, free_type_variables(implic->wanteds));
    }
    return free;
}


set<MetaTypeVar> free_meta_type_variables(const WantedConstraints& wanteds)
{
    set<MetaTypeVar> free = free_meta_type_variables(wanteds.simple);
    for(auto& implic: wanteds.implications)
    {
        add(free, free_meta_type_variables(implic->givens));
        add(free, free_meta_type_variables(implic->wanteds));
    }
    return free;
}


LIE& operator+=(LIE& lie1, const LIE& lie2)
{
    lie1.insert(lie1.end(), lie2.begin(), lie2.end());
    return lie1;
}


