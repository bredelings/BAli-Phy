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

namespace Expression
{

string CaseAlt::print() const
{
    return pattern.print() + " -> " + body.print();
}

bool CaseAlt::operator==(const CaseAlt& a) const
{
    return pattern == a.pattern and body == a.body;
}

string CaseAlts::print() const
{
    vector<string> as;
    for(auto& a: *this)
        as.push_back(a.print());

    return "{" + join(as,"; ") + "}";
}

bool CaseAlts::operator==(const Object& o) const
{
    const CaseAlts* As = dynamic_cast<const CaseAlts*>(&o);

    if (not As)
        return false;

    return (*this) == *As;
}

bool CaseAlts::operator==(const CaseAlts& as) const
{
    if (size() != as.size()) return false;

    for(int i=0;i<size();i++)
        if ((*this)[i] != as[i])
            return false;

    return true;
}

} // end namespace Expression

/// R = case T of {patterns[i] -> bodies[i]}
std::optional<std::tuple<expression_ref, Expression::CaseAlts>> parse_case_expression(const expression_ref& E)
{
    if (not is_case(E)) return {};

    auto object = E.sub()[0];

    auto alts = E.sub()[1].as_<Expression::CaseAlts>();

    return {{object, alts}};
}

Expression::CaseAlts make_alts(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    assert(patterns.size() == bodies.size());
    assert(not patterns.empty());

    Expression::CaseAlts alts;
    for(int i=0;i<patterns.size();i++)
	alts.push_back({patterns[i],bodies[i]});
    return alts;
}


expression_ref make_case_expression(const expression_ref& object, const Expression::CaseAlts& alts)
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
    return E.head().type() == type_constant::case_type;
}
