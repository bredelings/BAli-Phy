#include <set>
#include "lambda.H"
#include "var.H"
#include "case.H"
#include "let.H"
#include "substitute.H"
#include "computation/operations.H"
#include "util/string/join.H"

using std::vector;
using std::string;

namespace Run
{

string Alt::print() const
{
    return pattern.print() + " -> " + body.print();
}

bool Alt::operator==(const Alt& a) const
{
    return pattern == a.pattern and body == a.body;
}

bool Alt::operator!=(const Alt& a) const
{
    return not operator==(a);
}

string Alts::print() const
{
    vector<string> as;
    for(auto& a: *this)
        as.push_back(a.print());

    return "{" + join(as,"; ") + "}";
}

bool Alts::operator==(const Object& o) const
{
    const Alts* As = dynamic_cast<const Alts*>(&o);

    if (not As)
        return false;

    return (*this) == *As;
}

bool Alts::operator==(const Alts& as) const
{
    if (size() != as.size()) return false;

    for(int i=0;i<size();i++)
        if ((*this)[i] != as[i])
            return false;

    return true;
}

bool Alts::operator!=(const Alts& as) const
{
    return not operator==(as);
}


} // end namespace Run

void parse_alts(const Run::Alts& alts, vector<expression_ref>& patterns, vector<expression_ref>& bodies)
{
    for(auto& alt: alts)
    {
	patterns.push_back( alt.pattern );
	bodies.push_back( alt.body );
    }
}

/// R = case T of {patterns[i] -> bodies[i]}
bool parse_case_expression(const expression_ref& E, expression_ref& object, vector<expression_ref>& patterns, vector<expression_ref>& bodies)
{
    patterns.clear();
    bodies.clear();

    if (not is_case(E)) return false;

    object = E.sub()[0];

    parse_alts(E.sub()[1].as_<Run::Alts>(), patterns, bodies);

    return true;
}

/// R = case T of {patterns[i] -> bodies[i]}
bool parse_case_expression(const expression_ref& E, expression_ref& object, Run::Alts& alts)
{
    if (not is_case(E)) return false;

    object = E.sub()[0];

    alts = E.sub()[1].as_<Run::Alts>();

    return true;
}

Run::Alts make_alts(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    assert(patterns.size() == bodies.size());
    assert(not patterns.empty());

    Run::Alts alts;
    for(int i=0;i<patterns.size();i++)
	alts.push_back({patterns[i],bodies[i]});
    return alts;
}


expression_ref make_case_expression(const expression_ref& object, const Run::Alts& alts)
{
    assert(object);

    object_ptr<expression> E = new expression(Case());
    E->sub.push_back(object);
    E->sub.push_back(alts);

    return E;
}

expression_ref make_case_expression(const expression_ref& object, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    return make_case_expression(object, make_alts(patterns,bodies));
}

expression_ref make_if_expression(const expression_ref& condition, const expression_ref& true_branch, const expression_ref& false_branch)
{
    return make_case_expression(condition,{true,false},{true_branch, false_branch});
}

bool is_case(const expression_ref& E)
{
    return E.head().type() == case_type;
}

